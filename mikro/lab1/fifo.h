#ifndef __FIFO_H__
#define __FIFO_H__

#define FIFO_SIZE 128

typedef struct {
  char arr[FIFO_SIZE];
  int reader;
  int writer;
} FIFO;

void FIFO_Init(FIFO* fifo) {
  fifo->reader = 0;
  fifo->writer = 0;
}

int FIFO_Empty(FIFO* fifo) {
  return fifo->writer == fifo->reader;
}

int FIFO_Full(FIFO* fifo) {
  return (fifo->writer + 1) % FIFO_SIZE == fifo->reader;
}

char FIFO_Read(FIFO* fifo) {
  if(FIFO_Empty(fifo))
    return 0;
  char ret = fifo->arr[fifo->reader];
  fifo->reader++;
  fifo->reader %= FIFO_SIZE;
  return ret;
}

void FIFO_Write(FIFO* fifo, char c) {
  if(FIFO_Full(fifo))
    return;
  fifo->arr[fifo->writer++] = c;
  fifo->writer %= FIFO_SIZE;
}

void FIFO_WriteStr(FIFO* fifo, const char* str, int len) {
    int i;
    for(i = 0; i < len; i++)
        FIFO_Write(fifo, str[i]);
}

#endif //__FIFO_H__