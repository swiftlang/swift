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


typedef struct _ContainedType {
  unsigned int f1;
  float    f2;
} __attribute__((packed)) ContainedType;

typedef struct _ContainerType {
  char x1;
  ContainedType l[10];
}  __attribute__((packed)) ContainerType;
