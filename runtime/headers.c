#include <config.h>
#include "headers.h"

object_t
set_global(context_t context, const char *name, object_t value)
{ return NULL; }

object_t
set_local(context_t context, int uplevel, int offset, object_t value)
{
	object_t cur = context->env;
	if (cur == NULL) return NULL;
	while (uplevel > 0)
	{
		if (cur->environment.parent == OBJECT_NULL) return NULL;
		else cur = cur->environment.parent;
		-- uplevel;
	}

	if (offset >= cur->environment.length) return NULL;
	object_t result = SLOT_GET(cur->environment.slot_entry[offset]);
	if (IS_OBJECT(result))
		context_attach(context, result);
	SLOT_SET(cur->environment.slot_entry[offset], value);
	return result;
}

object_t
get_global(context_t context, const char *name)
{ return NULL; }

object_t
get_local(context_t context, int uplevel, int offset)
{
	object_t cur = context->env;
	while (uplevel > 0)
	{
		if (cur->environment.parent == OBJECT_NULL) return NULL;
		else cur = cur->environment.parent;
		-- uplevel;
	}

	if (offset >= cur->environment.length) return NULL;
	object_t result = SLOT_GET(cur->environment.slot_entry[offset]);
	if (IS_OBJECT(result))
		context_attach(context, result);

	return result;
}

object_t create_string_from_cstr(context_t context, const char *cstr);
object_t create_symbol_from_cstr(context_t context, const char *cstr);

object_t create_list_with_tail(context_t context, int count, ...);
object_t create_vector(context_t context, int count, ...);

object_t create_closure(context_t context, lambda_t lambda, int argc);

void apply(context_t context, int argc, ...);
void apply_tl(context_t context, int argc, ...);
