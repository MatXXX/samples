#ifndef __FIFO_H__
#define __FIFO_H__

#define FIFO_SIZE 256

#define FIFO_TYPE(INFIX, TYPE) \
\
typedef struct {\
  TYPE arr[FIFO_SIZE];\
  int reader;\
  int writer;\
} fifo_##INFIX;\
\
void fifo_##INFIX##_init(fifo_##INFIX* fifo) {\
  fifo->reader = 0;\
  fifo->writer = 0;\
}\
\
int fifo_##INFIX##_empty(fifo_##INFIX* fifo) {\
  return fifo->writer == fifo->reader;\
}\
\
int fifo_##INFIX##_full(fifo_##INFIX* fifo) {\
  return (fifo->writer + 1) % FIFO_SIZE == fifo->reader;\
}\
\
void fifo_##INFIX##_pop(fifo_##INFIX* fifo) {\
  if (fifo_##INFIX##_empty(fifo))\
    return;\
  fifo->reader++;\
  fifo->reader %= FIFO_SIZE;\
}\
\
TYPE fifo_##INFIX##_get(fifo_##INFIX* fifo) {\
  TYPE ret = fifo->arr[fifo->reader];\
  fifo->reader++;\
  fifo->reader %= FIFO_SIZE;\
  return ret;\
}\
\
TYPE* fifo_##INFIX##_peek(fifo_##INFIX* fifo) {\
  if (fifo_##INFIX##_empty(fifo))\
    return 0;\
  return &fifo->arr[fifo->reader];\
}\
\
void fifo_##INFIX##_add(fifo_##INFIX* fifo, const TYPE* c) {\
  if (fifo_##INFIX##_full(fifo))\
    return;\
  if (!c)\
    return;\
  fifo->arr[fifo->writer++] = *c;\
  fifo->writer %= FIFO_SIZE;\
}\
\
void fifo_##INFIX##_add_array(fifo_##INFIX* fifo, const TYPE* str, int len) {\
    int i;\
    for (i = 0; i < len; i++)\
        fifo_##INFIX##_add(fifo, &str[i]);\
}

#endif //__FIFO_H__
