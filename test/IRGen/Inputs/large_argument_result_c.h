#pragma once

typedef struct {
  unsigned long long arr[16];
} large_thing;

large_thing pass_and_return(large_thing a, large_thing b);
