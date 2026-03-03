#ifndef TEST_INTEROP_CXX_METADATA_INPUTS_MIRROR_H
#define TEST_INTEROP_CXX_METADATA_INPUTS_MIRROR_H

struct EmptyStruct {};

struct BaseStruct {
private:
  int priv;

public:
  int publ;

protected:
  int prot;

public:
  BaseStruct(int i1, int i2, int i3) : priv(i1), publ(i2), prot(i3) {}
};

class EmptyClass {};

struct OuterStruct {
private:
  BaseStruct privStruct;

public:
  BaseStruct publStruct;

  OuterStruct() : privStruct(1, 2, 3), publStruct(4, 5, 6) {}
};

struct FRTStruct {
private:
  int priv = 1;

public:
  int publ = 2;
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retain")))
__attribute__((swift_attr("release:release")));

void retain(FRTStruct *v) {};
void release(FRTStruct *v) {};

class FRTImmortalClass {} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal")));

#endif
