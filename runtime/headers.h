#ifndef __RUNTIME_HEADERS_H__
#define __RUNTIME_HEADERS_H__

#include "object.h"

#define __NULL           SPECIAL_BOX(0)
#define __BOOLEAN_FALSE  SPECIAL_BOX(1)
#define __BOOLEAN_TRUE   SPECIAL_BOX(2)

#define __INTEGER(context,i) INT_BOX(i)
#define __NUMBER(context,i)  INT_BOX((int)(i))
#define __STRING(context,s)  create_string_from_cstr(context,s)
#define __SYMBOL(context,s)  create_symbol_from_cstr(context,s)

#define __LCONS(context,c,v ...)  create_list_with_tail(context,c,v)
#define __VECTOR(context,c,v ...) create_vector(context,c,v)

#define __EXIT(v)       apply(__EXIT_PROC, 1, v)
#define __EXIT_PROC     NULL

#define IS_TURE(CONTEXT,V) ((V) != __BOOLEAN_FALSE)

#define SET_GLOBAL     set_global
#define SET_LOCAL      set_local
#define GET_GLOBAL     get_global
#define GET_LOCAL      get_local
#define CREATE_CLOSURE create_closure

object_t create_string_from_cstr(context_t context, const char *cstr);
object_t create_symbol_from_cstr(context_t context, const char *cstr);

object_t create_list_with_tail(context_t context, int count, ...);
object_t create_vector(context_t context, int count, ...);

object_t set_global(context_t context, const char *name, object_t value);
object_t set_local(context_t context, int uplevel, int offset, object_t value);
object_t get_global(context_t context, const char *name);
object_t get_local(context_t context, int uplevel, int offset);
object_t create_closure(context_t context, lambda_t lambda, int argc);

void apply(context_t context, int argc, ...);
void apply_tl(context_t context, int argc, ...);

#endif
