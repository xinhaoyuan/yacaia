#ifndef __RUNTIME_OBJECT_H__
#define __RUNTIME_OBJECT_H__

/* Base on the object system in SEE */

/* The types */
typedef struct heap_s       heap_s;
typedef struct object_s     object_s;
typedef struct slot_s       slot_s;
typedef struct context_s    context_s;

typedef heap_s       *heap_t;
typedef object_s     *object_t;
typedef slot_s       *slot_t;
typedef context_s    *context_t;
typedef void(*lambda_t)(context_t);

/* The integer inside the object system */
/* Which may have the same bit width with pointers */
#if defined(__i386__)
typedef long          runtime_int_t;
typedef unsigned long runtime_uint_t;
#elif defined(__x86_64)
typedef long long          runtime_int_t;
typedef unsigned long long runtime_uint_t;
#else
#error unknown arch
#endif

/* The encoding suffix for distinguish object */
#define ENCODE_SUFFIX_OBJECT  0
#define ENCODE_SUFFIX_SPECIAL 1
#define ENCODE_SUFFIX_INT     2
#define ENCODE_SUFFIX_BOXED   3

#define OBJECT_NULL          ((object_t)0x1)

/* Assume that the object pointer is at least aligned by 2 bits */
#define ENCODE_SUFFIX(object) ((runtime_uint_t)(object) & 0x3)

/* The object type id */
#define OBJECT_TYPE_UNINITIALIZED 0

#define OBJECT_TYPE_VALID         4
#define OBJECT_TYPE_STRING        4
#define OBJECT_TYPE_PAIR          5
#define OBJECT_TYPE_VECTOR        6
#define OBJECT_TYPE_ENVIRONMENT   7
#define OBJECT_TYPE_CLOSURE       8
#define OBJECT_TYPE_CONTEXT       9
#define OBJECT_TYPE_EXTERNAL      10

#include "om/object_impl.h"

#ifndef SLOT_INIT
void slot_init(slot_t slot, object_t object);
#define SLOT_INIT(slot) slot_init(&(slot), object)
#endif

#ifndef SLOT_GET
object_t slot_get(slot_t slot);
#define SLOT_GET(slot) (slot_get(&(slot)))
#endif

#ifndef SLOT_SET
void slot_set(slot_t slot, object_t object);
#define SLOT_SET(slot) slot_set(&(slot), object)
#endif

#ifndef OBJECT_TYPE
int object_type(object_t object);
#define OBJECT_TYPE(object) object_type(object)
#endif

#ifndef OBJECT_TYPE_INIT
void object_type_init(object_t object, int type);
#define OBJECT_TYPE_INIT(object, type) object_type_init(object, type)
#endif

struct runtime_external_type_s
{
	const char *name;
	
	void(*enumerate)(object_t, void *, void(*)(void *, object_t));
	void(*free)(object_t);
};

extern struct runtime_external_type_s external_type_dummy;

struct object_s
{
	union
	{
		xstring_t string;

		struct
		{
			slot_s slot_car, slot_cdr;
		} pair;

		struct
		{
			unsigned int length;
			slot_s      *slot_entry;
		} vector;

		struct
		{
			unsigned int length;
			slot_s      *slot_entry;
			object_t     parent;
		} environment;

		struct
		{
			lambda_t     proc;
			unsigned int argc;
			object_t     env;
		} closure;

		struct
		{
			struct runtime_external_type_s *type;
			
			union
			{
				runtime_int_t   id;
				runtime_uint_t uid;
				void     *priv;
			};			
		} external;
	};
};

struct context_s
{
	object_t lambda;
	heap_t   heap;
};

#define INT_UNBOX(object)      ((runtime_int_t)(object) >> 2)
#define INT_BOX(i)             ((object_t)(((runtime_uint_t)(i) << 2) | ENCODE_SUFFIX_INT))

#define SPECIAL_UNBOX(object)  ((runtime_int_t)(object) >> 2)
#define SPECIAL_BOX(s)         ((object_t)(((runtime_uint_t)(s) << 2) | ENCODE_SUFFIX_SPECIAL))

#define EXTERNAL_UNBOX(object) ((void *)((runtime_uint_t)(object) & ~(runtime_uint_t)0x3))
#define EXTERNAL_BOX(e)        ((object_t)((runtime_uint_t)(e) | ENCODE_SUFFIX_BOXED))

#define IS_OBJECT(object)      (ENCODE_SUFFIX(object) == ENCODE_SUFFIX_OBJECT)
#define IS_INT(object)         (ENCODE_SUFFIX(object) == ENCODE_SUFFIX_INT)
#define IS_SPECIAL(object)     (ENCODE_SUFFIX(object) == ENCODE_SUFFIX_SPECIAL)
#define IS_EXTERNAL(object)    (ENCODE_SUFFIX(object) == ENCODE_SUFFIX_BOXED)

heap_t   heap_new(void);
void     heap_free(heap_t heap);
object_t heap_object_new(heap_t heap);
void     heap_object_free(heap_t heap, object_t object);

/* When create a reference to the object, the object should be protect from the GC collection  */
void heap_protect_from_gc(heap_t heap, object_t object);
void heap_unprotect(heap_t heap, object_t object);

void heap_detach(object_t object);

context_t heap_context_new(heap_t heap);
void      heap_context_free(context_t ex);

#endif
