#include <config.h>
#include "../object.h"
#include "../lib/xstring.h"


#define GC_PARAM_0 10240
#define GC_PARAM_1 10240

/* The definition of heap structure is hidden to user, so we can
 * change the internal implementation unconspicuously */

#define TO_GC(o)      ((gc_header_t)(o) - 1)
#define TO_OBJECT(gc) ((object_t)((gc_header_t)(gc) + 1))

/* No magic check anymore */
// static char __sa[sizeof(gc_header_s) == GC_HEADER_SPACE ? 0 : -1] __attribute__((unused));

struct heap_s
{
	unsigned int count;			/* living object count */
	unsigned int threshold;		/* count trigger for collecting */
	/* lists for managed objects and hold objects(root objects) */
	struct gc_header_s managed, locked;
};

static inline void
object_free(object_t object)
{	
	switch (OBJECT_TYPE(object))
	{		
	case OBJECT_TYPE_STRING:
		xstring_free(object->string);
		break;

	case OBJECT_TYPE_VECTOR:
		RUNTIME_FREE(object->vector.slot_entry);
		break;

	case OBJECT_TYPE_ENVIRONMENT:
		RUNTIME_FREE(object->environment.slot_entry);
		break;
		
	case OBJECT_TYPE_EXTERNAL:
		object->external.type->free(object);
		break;
	}

	RUNTIME_FREE(TO_GC(object));
}

heap_t
heap_new(void)
{
	heap_t result = (heap_t)RUNTIME_MALLOC(sizeof(struct heap_s));

	/* Initialize the gc header list */
	result->managed.prev =
		result->managed.next = &result->managed;

	result->locked.prev =
		result->locked.next = &result->locked;

	/* Initialize counters */
	result->count = 0;
	result->threshold = GC_PARAM_0;

	return result;
}

void
heap_free(heap_t heap)
{
	gc_header_t cur_gc;
	
	/* Need to ignore all locked object since they may be hold by external
	 * prog ??? */
	/* Currently no */
	
#if 1
	cur_gc = heap->locked.next;
	while (cur_gc != &heap->locked)
	{
		gc_header_t last_gc = cur_gc;
		cur_gc = cur_gc->next;
		
		object_free(TO_OBJECT(last_gc));
	}
#endif

	cur_gc = heap->managed.next;
	while (cur_gc != &heap->managed)
	{
		gc_header_t last_gc = cur_gc;
		cur_gc = cur_gc->next;
		
		object_free(TO_OBJECT(last_gc));
	}

	RUNTIME_FREE(heap);
}

typedef struct exqueue_s
{
	unsigned int alloc;
	unsigned int head;
	unsigned int tail;

	object_t *queue;
} exqueue_s;

static void
exqueue_enqueue(exqueue_s *q, object_t o)
{
	if (IS_OBJECT(o) && TO_GC(o)->mark == 0)
	{
		TO_GC(o)->mark = 1;
		
		q->queue[q->head ++] = o;
		q->head %= q->alloc;
		if (q->head == q->tail)
		{
			q->queue = (object_t *)RUNTIME_REALLOC(q->queue, sizeof(object_t) * (q->alloc << 1));
			RUNTIME_MEMCPY(q->queue + q->alloc, q->queue, sizeof(object_t) * q->head);
			q->head += q->alloc;
			q->alloc <<= 1;
		}
	}
}
	
static void
do_gc(heap_t heap)
{
	exqueue_s q;
	q.alloc = 256;
	q.head  = 0;
	q.tail  = 0;
	q.queue = (object_t *)RUNTIME_MALLOC(sizeof(object_t) * q.alloc);

	gc_header_t cur_gc = heap->locked.next;
	while (cur_gc != &heap->locked)
	{
		cur_gc->mark = 0;
		exqueue_enqueue(&q, TO_OBJECT(cur_gc));
		cur_gc = cur_gc->next;
	}

	object_t now;
	while (q.head != q.tail)
	{
		now = q.queue[q.tail ++];
		q.tail %= q.alloc;

		/* Enumerate all links inside a object */
		switch (OBJECT_TYPE(now))
		{
		case OBJECT_TYPE_PAIR:
			exqueue_enqueue(&q, SLOT_GET(now->pair.slot_car));
			exqueue_enqueue(&q, SLOT_GET(now->pair.slot_cdr));
			break;

		case OBJECT_TYPE_VECTOR:
		{
			int i;
			for (i = 0; i != now->vector.length; ++ i)
			{
				exqueue_enqueue(&q, SLOT_GET(now->vector.slot_entry[i]));
			}
			break;
		}

		case OBJECT_TYPE_ENVIRONMENT:
		{
			int i;
			for (i = 0; i != now->environment.length; ++ i)
			{
				exqueue_enqueue(&q, SLOT_GET(now->environment.slot_entry[i]));
			}
			exqueue_enqueue(&q, now->environment.parent);
			break;
		}

		case OBJECT_TYPE_CLOSURE:
			exqueue_enqueue(&q, now->closure.env);
			break;
		
		case OBJECT_TYPE_CONTEXT:
		{
			context_t ctx = (context_t)now;
			exqueue_enqueue(&q, ctx->lambda);
			exqueue_enqueue(&q, ctx->env);

			/* list operation on context */
			cur_gc = ctx->relax.next;
			while (cur_gc != &ctx->relax)
			{
				exqueue_enqueue(&q, TO_OBJECT(cur_gc));
				cur_gc = cur_gc->next;
			}
			break;
		}
		
		case OBJECT_TYPE_EXTERNAL:
		{
			now->external.type->enumerate(now, &q, (void(*)(void *, object_t))&exqueue_enqueue);
			break;
		}

		}
	}

	cur_gc = heap->managed.next;
	while (cur_gc != &heap->managed)
	{
		if (cur_gc->mark)
		{
			cur_gc->mark = 0;
			cur_gc = cur_gc->next;
		}
		else
		{
			cur_gc->prev->next = cur_gc->next;
			cur_gc->next->prev = cur_gc->prev;

			gc_header_t last_gc = cur_gc;
			cur_gc = cur_gc->next;
			
			object_free(TO_OBJECT(last_gc));
			
			-- heap->count;
		}
	}

	if (q.queue) RUNTIME_FREE(q.queue);
	return;
}

static inline void
detach(heap_t heap, object_t object)
{
	gc_header_t gc = TO_GC(object);
	/* list operation on heap (maybe) */
	gc->next->prev = gc->prev;
	gc->prev->next = gc->next;
}

void
heap_protect_from_gc(heap_t heap, object_t object)
{
	gc_header_t gc = TO_GC(object);
	++ gc->prot_level;

	if (gc->prot_level == 1)
	{
		/* list operation on heap */
		detach(heap, object);
		gc->next = &heap->locked;
		gc->prev = gc->next->prev;
		gc->prev->next = gc;
		gc->next->prev = gc;
	}
}

void
heap_unprotect(heap_t heap, object_t object)
{
	gc_header_t gc = TO_GC(object);
	if (gc->prot_level > 0)
	{
		-- gc->prot_level;

		if (gc->prot_level == 0)
		{
			/* list operation on heap */
			detach(heap, object);
			gc->next = &heap->managed;
			gc->prev = gc->next->prev;
			gc->prev->next = gc;
			gc->next->prev = gc;
		}
	}
}

context_t
heap_context_new(heap_t heap)
{
	gc_header_t gc = (gc_header_t)RUNTIME_MALLOC(sizeof(gc_header_s) + sizeof(context_s));
	context_t ctx = (context_t)TO_OBJECT(gc);

	ctx->lambda = NULL;
	ctx->env = NULL;
	ctx->heap = heap;

	ctx->relax.prev = ctx->relax.next = &ctx->relax;
	
	gc->type = OBJECT_TYPE_CONTEXT;
	gc->mark = 0;

	/* list operation on heap */
	gc->next = &heap->locked;
	gc->prev = gc->next->prev;
	gc->next->prev = gc;
	gc->prev->next = gc;
	
	return ctx;
}

void
heap_context_free(context_t ctx)
{
	context_relax(ctx);
	/* list operation on heap */
	detach(ctx->heap, (object_t)ctx);
	
	RUNTIME_FREE(TO_GC(ctx));
}

object_t
context_object_new(context_t ctx)
{
	++ ctx->relax_count;
	
	gc_header_t result = (gc_header_t)RUNTIME_MALLOC(sizeof(gc_header_s) + sizeof(object_s));
	result->type = OBJECT_TYPE_UNINITIALIZED;
	result->mark = 0;

	/* list operation on context */
	result->prot_level = 0;
	result->next = &ctx->relax;
	result->prev = result->next->prev;
	result->prev->next = result;
	result->next->prev = result;
	
	return TO_OBJECT(result);
}

void
context_attach(context_t ctx, object_t object)
{
	gc_header_t gc = TO_GC(object);
	if (gc->prot_level == 0)
	{
		/* list operation on heap */
		detach(ctx->heap, object);
		
		/* list operation on context */
		gc->next = &ctx->relax;
		gc->prev = gc->next->prev;
		gc->next->prev = gc;
		gc->prev->next = gc;
	}
}

void
context_relax(context_t ctx)
{
	if (ctx->relax.next == &ctx->relax) return;

	heap_t heap = ctx->heap;
	gc_header_t head = ctx->relax.next, tail = ctx->relax.prev;

	/* list operation on heap and context */
	tail->next = &heap->managed;
	head->prev = heap->managed.prev;

	tail->next->prev = tail;
	head->prev->next = head;

	ctx->relax.next = ctx->relax.prev = &ctx->relax;
	ctx->relax_count = 0;
}


static void dummy_enumerate(object_t object, void *list, void(*list_add)(void *, object_t)) { } 
static void dummy_free(object_t object) { }

struct runtime_external_type_s external_type_dummy =
{
	.name = "DUMMY",
	.free = dummy_free,
	.enumerate = dummy_enumerate,
};
