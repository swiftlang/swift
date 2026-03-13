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

int puts(const char *p);

__attribute__((naked))
__attribute__((section(".start")))
void start() {
  asm volatile("la sp, stack + 8192 - 4");
  asm volatile("call main");
  asm volatile("call halt");
}

void halt(void) {
  puts("HALT\n");
  asm("unimp");
}

__attribute__((aligned(4))) char stack[8192];

int putchar(int c) {
  // This is only valid in an emulator (QEMU), and it's skipping a proper configuration of the UART device
  // and waiting for a "ready to transit" state.

  // QEMU riscv32-virt's specific location of the 16550A UART and its THR register
  *(volatile uint8_t *)(0x10000000 + 0) = c;
  return c;
}
