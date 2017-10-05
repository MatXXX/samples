#ifndef __FIFO_H__
#define __FIFO_H__

#define Fifo_SIZE 256

#define Fifo_INSTANTIATE(INFIX, TYPE) \
\
typedef struct {\
  TYPE arr[Fifo_SIZE];\
  int reader;\
  int writer;\
} Fifo_##INFIX;\
\
void Fifo_##INFIX##_Init(Fifo_##INFIX* fifo) {\
  fifo->reader = 0;\
  fifo->writer = 0;\
}\
\
int Fifo_##INFIX##_Empty(Fifo_##INFIX* fifo) {\
  return fifo->writer == fifo->reader;\
}\
\
int Fifo_##INFIX##_Full(Fifo_##INFIX* fifo) {\
  return (fifo->writer + 1) % Fifo_SIZE == fifo->reader;\
}\
\
TYPE Fifo_##INFIX##_Get(Fifo_##INFIX* fifo) {\
  if(Fifo_##INFIX##_Empty(fifo))\
    return 0;\
  TYPE ret = fifo->arr[fifo->reader];\
  fifo->reader++;\
  fifo->reader %= Fifo_SIZE;\
  return ret;\
}\
\
void Fifo_##INFIX##_Add(Fifo_##INFIX* fifo, TYPE c) {\
  if(Fifo_##INFIX##_Full(fifo))\
    return;\
  fifo->arr[fifo->writer++] = c;\
  fifo->writer %= Fifo_SIZE;\
}\
\
void Fifo_##INFIX##_AddArray(Fifo_##INFIX* fifo, const TYPE* str, int len) {\
    int i;\
    for(i = 0; i < len; i++)\
        Fifo_##INFIX##_Add(fifo, str[i]);\
}

#define Fifo_Init() \
    { {0}, 0, 0 }


Fifo_INSTANTIATE(UINT, uint32_t);
Fifo_INSTANTIATE(CHAR_PTR, const char*);

#define Fifo_UINT_New(NAME) \
    Fifo_UINT NAME = Fifo_Init()

#define Fifo_CHAR_PTR_New(NAME) \
    Fifo_CHAR_PTR NAME = Fifo_Init()

#endif //__FIFO_H__
