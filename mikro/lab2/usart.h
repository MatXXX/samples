#ifndef __USART_H__
#define __USART_H__

#define USART_Mode_Rx_Tx (USART_CR1_RE | \
                          USART_CR1_TE)

#define USART_WordLength_8b 0x0000
#define USART_WordLength_9b USART_CR1_M

#define USART_Parity_No 0x0000
#define USART_Parity_Even USART_CR1_PCE
#define USART_Parity_Odd (USART_CR1_PCE | \
                          USART_CR1_PS)

#define USART_StopBits_1 0x0000
#define USART_StopBits_0_5 0x1000
#define USART_StopBits_2 0x2000
#define USART_StopBits_1_5 0x3000

#define USART_FlowControl_None 0x0000
#define USART_FlowControl_RTS USART_CR3_RTSE
#define USART_FlowControl_CTS USART_CR3_CTSE

#define HSI_HZ 16000000U
#define PCLK1_HZ HSI_HZ

int USART_CanRead() {
  return USART2->SR & USART_SR_RXNE;
}

int USART_Read(char* out) {
  if(USART_CanRead()) {
    *out = USART2->DR;
    return 1;
  }
  return 0;
}

int USART_CanSend() {
  return USART2->SR & USART_SR_TXE;
}

int USART_Send(char c) {
  if(USART_CanSend()) {
    USART2->DR = c;
    return 1;
  }
  return 0;
}

void USART_Configure() {
  GPIOafConfigure(GPIOA,
                  2,
                  GPIO_OType_PP,
                  GPIO_Fast_Speed,
                  GPIO_PuPd_NOPULL,
                  GPIO_AF_USART2);

  USART2->CR1 = USART_CR1_TE;
  USART2->CR2 = USART_StopBits_1;
  USART2->CR3 = USART_CR3_DMAT;

  uint32_t const baudrate = 9600U;
  USART2->BRR = (PCLK1_HZ + (baudrate / 2U)) / baudrate;

  USART2->CR1 |= USART_CR1_UE;
}

#endif //__USART_H__
