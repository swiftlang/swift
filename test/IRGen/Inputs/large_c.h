#pragma once

struct SamplesType {
    void* A;
    void* B;
    void* C;
    void* D;
    void* E;
    void* F;
    void* G;
    void* H;
    void* I;
    void* J;
    void* K;
    void* L;
    void* M;
    void* N;
    void* O;
    void* P;
    void* Q;
    void* R;
    void* S;
    void* T;
    void* U;
    void* V;
    void* W;
    void* X;
    void* Y;
    void* Z;
    void* AA;
};

struct SamplesType samples();

typedef struct _ContainedType {
  unsigned int f1;
  float    f2;
} __attribute__((packed)) ContainedType;

typedef struct _ContainerType {
  char x1;
  ContainedType l[10];
}  __attribute__((packed)) ContainerType;

typedef unsigned char arr_t[32];

typedef enum : unsigned int {
    entry_0       = 0,
    entry_1       = 1,
    entry_2       = 2,
    entry_3       = 3,
    entry_4       = 4,
    entry_5       = 5,
    entry_6       = 6,
    entry_7       = 7,
    entry_8       = 8,
    entry_9       = 9,
    entry_10      = 10,
    entry_11      = 11,
    entry_12      = 12,
    entry_13      = 13,
    entry_14      = 14,
    entry_15      = 15,
    entry_16      = 16,
    entry_17      = 17,
    entry_18      = 18,
    entry_invalid = 255,
} enum_t;

typedef union {
    struct {
        enum_t  slot;
        arr_t   buf;
    } in;
    struct {
        int     result;
        arr_t   buff;
        unsigned char cnt;
    } out;
} union_t;


typedef enum {
  TYPE1,
  TYPE2,
  TYPE3
} member_type_t;

typedef unsigned char uuid_t[16];
typedef struct {
  member_type_t member_type;
  union {
    uuid_t uuid;
    unsigned x;
  } member_value;
} member_id_t;
