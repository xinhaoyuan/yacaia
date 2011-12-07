#ifndef __HEADERS_H__
#define __HEADERS_H__

typedef struct __context_s *__context_t;
typedef struct __context_s
{
	int closure_id;
} __context_s;

typedef void *object_t;

#define __BOOLEAN_TRUE   1
#define __BOOLEAN_FALSE  0
#define __EXIT(...)     -1
#define __EXIT_PROC     -1
#define __NULL           0
#define NULL             0

#endif
