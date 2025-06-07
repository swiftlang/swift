//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors.
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include <stddef.h>
#include <stdint.h>

__attribute__((naked))
__attribute__((section(".start")))
void start() {
  // stack is 0x1e00 ..< 0x2200 (1 kB), see linkerscript.ld
  // set SP to 0x21ff, last byte of SRAM
  asm volatile("ldi r16, 0xff");
  asm volatile("out 0x3d, r16");
  asm volatile("ldi r16, 0x21");
  asm volatile("out 0x3e, r16");
  asm volatile("call copy_data_from_flash_to_sram");
  asm volatile("call usart_init");
  asm volatile("call main");
  asm volatile("call halt");
}

void usart_init() {
  *((volatile char *)0xc1) = 1 << 3; // enable TX on UART0
}

void *memcpy_flash_to_sram(void *restrict dst, const void __attribute__((__address_space__(1))) *restrict src, size_t n) {
  for (int i = 0; i < n; i++) {
    ((char *)dst)[i] = ((char __attribute__((__address_space__(1))) *)src)[i];
  }
  return dst;
}

void copy_data_from_flash_to_sram() {
  // Copy data segment from 0x200-program-space to 0x200-data-space, 7 kB in size (see linkerscript.ld)
  memcpy_flash_to_sram((void *)0x200, (void __attribute__((__address_space__(1))) *)0x200, 7 * 1024);
}

int putchar(int c) {
  // This is only valid in an emulator (QEMU), and it's skipping a proper configuration of the UART device
  // and waiting for a "ready to transit" state.

  // AVR's UART0 DR register
  *((volatile char *)0xc6) = c;
  return c;
}

int puts(const char *);
void halt(void) {
  puts("HALT\n");
  asm("break");
}

void abort(void) {
  asm("break");
}
