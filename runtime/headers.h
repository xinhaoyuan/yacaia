#ifndef __RUNTIME_HEADERS_H__
#define __RUNTIME_HEADERS_H__

#include "object.h"

#define BOOLEAN_FALSE(context)       SPECIAL_BOX(1)
#define BOOLEAN_TRUE(context)        SPECIAL_BOX(2)
#define INTEGER(context,i)           INT_BOX(i)
#define NUMBER(context,i)            INT_BOX((int)(i))
#define STRING(context,s)            create_string_from_cstr(context,s)
#define SYMBOL(context,s)            create_symbol_from_cstr(context,s)
#define LCONS(context,c,v ...)       create_list_with_tail(context,c,v)
#define VECTOR(context,c,v ...)      create_vector(context,c,v)
#define CLOSURE(context,lambda,argc) create_closure(context,lambda,argc)
#define APPLY(context,argc,v ...)    apply(context,argc,v)
#define APPLY_TL(context,argc,v ...) apply_tl(context,argc,v)
#define EXIT(context,v)              apply(context, 2, EXIT_PROC(context), v)
#define EXIT_PROC(context)           NULL
#define IS_TURE(context,V)           ((V) != __BOOLEAN_FALSE(context))

#define SET_GLOBAL(context,name,value)          set_global(context,name,value)
#define SET_LOCAL(context,uplevel,offset,value) set_local(context,uplevel,offset,value)
#define GET_GLOBAL(context,name)                get_global(context,name)
#define GET_LOCAL(context,uplevel,offset)       get_local(context,uplevel,offset)

object_t set_global(context_t context, const char *name, object_t value);
object_t set_local(context_t context, int uplevel, int offset, object_t value);
object_t get_global(context_t context, const char *name);
object_t get_local(context_t context, int uplevel, int offset);

object_t create_string_from_cstr(context_t context, const char *cstr);
object_t create_symbol_from_cstr(context_t context, const char *cstr);

object_t create_list_with_tail(context_t context, int count, ...);
object_t create_vector(context_t context, int count, ...);

object_t create_closure(context_t context, lambda_t lambda, int argc);

void apply(context_t context, int argc, ...);
void apply_tl(context_t context, int argc, ...);

#endif
