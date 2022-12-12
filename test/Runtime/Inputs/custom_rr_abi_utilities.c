#include <stdio.h>

#define NUM_REGS 29

// Apply `macro` to "all" registers. Skip x18 since it's reserved, x29
// since it's the frame pointer, and x30 since it's the link register.
#define ALL_REGS(macro)                                                        \
  macro( 0)                                                                    \
  macro( 1)                                                                    \
  macro( 2)                                                                    \
  macro( 3)                                                                    \
  macro( 4)                                                                    \
  macro( 5)                                                                    \
  macro( 6)                                                                    \
  macro( 7)                                                                    \
  macro( 8)                                                                    \
  macro( 9)                                                                    \
  macro(10)                                                                    \
  macro(11)                                                                    \
  macro(12)                                                                    \
  macro(13)                                                                    \
  macro(14)                                                                    \
  macro(15)                                                                    \
  macro(16)                                                                    \
  macro(17)                                                                    \
  macro(19)                                                                    \
  macro(20)                                                                    \
  macro(21)                                                                    \
  macro(22)                                                                    \
  macro(23)                                                                    \
  macro(24)                                                                    \
  macro(25)                                                                    \
  macro(26)                                                                    \
  macro(27)                                                                    \
  macro(28)

void printRegisters_x(void) {
  asm("sub sp, sp, #(30 * 8)\n"
#define STORE(reg) "str x" #reg ", [sp, 8 * " #reg "]\n"
    ALL_REGS(STORE)
  );
  
  void **regs;
  asm("mov %0, sp" : "=r" (regs));
  
  for (int i = 0; i < NUM_REGS; i++) {
    fprintf(stderr, "x%d = %p\n", i, regs[i]);
  }
  
  asm("add sp, sp, #(30 * 8)");
}
