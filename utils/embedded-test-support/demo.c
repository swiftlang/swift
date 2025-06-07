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

#include <stdint.h>
#include <stddef.h>

void *malloc(size_t count);

int global1 = 42;
int global2 = 777;

void *global_with_reloc = &global1;

__attribute__((noinline))
int recur(int p, int n) {
  if (p == 0) return 1;
  global2++;
  return recur(p - n, n) + 1;
}

__attribute__((noinline))
void *testheap() {
  void *p = malloc(12);
  *(uint32_t *)p = 1234;
  return p;
}


int puts(const char *);
int main() {
  puts("Hello Embedded Swift!\n");
  puts("-- printing works\n");
  int res = recur(10, 1);
  if (res == 11) 
    puts("-- stack works\n");
  else
    puts("???\n");

  if (global1 == 42)
    puts("-- global1 works\n");
  else
    puts("???\n");

  if (global2 == 787)
    puts("-- global2 works\n");
  else
    puts("???\n");

  if ((void *)global_with_reloc == (void *)&global1)
    puts("-- global_with_reloc works\n");
  else
    puts("???\n");

  if (*(int *)global_with_reloc == 42)
    puts("-- global_with_reloc has right value\n");
  else
    puts("???\n");

  void *p = testheap();
  if (*(uint32_t *)p == 1234)
    puts("-- heap work\n");
  else
    puts("???\n");

  puts("DONE!\n");

  return 0;
}
