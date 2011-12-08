#ifndef __RUNTIME_CONFIG_H__
#define __RUNTIME_CONFIG_H__

#define RUNTIME_STDLIBC   1
#define RUNTIME_SYSTEM_IO 1
#define RUNTIME_DEBUG     0

/* **************************************** */

#if RUNTIME_STDLIBC

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define RUNTIME_MALLOC  malloc
#define RUNTIME_REALLOC realloc
#define RUNTIME_FREE    free
#define RUNTIME_STRLEN  strlen
#define RUNTIME_MEMCPY  memcpy
#define RUNTIME_MEMCMP  memcmp
#define RUNTIME_PRINTF  printf
#define RUNTIME_FPRINTF fprintf
typedef FILE *RUNTIME_FILE_T;

#else

#error Unkonwn configuration

#endif

#if RUNTIME_DEBUG

#define ERROR(ARGS ...)   RUNTIME_FPRINTF(stderr, ARGS)
#define WARNING(ARGS ...) RUNTIME_FPRINTF(stderr, ARGS)

#else

#define ERROR(ARGS ...)
#define WARNING(ARGS ...)

#endif

#endif
