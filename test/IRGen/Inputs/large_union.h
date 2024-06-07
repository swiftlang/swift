#pragma once

typedef union {
  struct {
    unsigned char a;
    unsigned char arr[32];
  } in;
  struct {
    int a;
    unsigned char arr[32];
    unsigned char b;
  } out;
} some_struct;
