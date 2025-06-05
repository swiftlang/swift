#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_SIMPLE_STRUCTS_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_SIMPLE_STRUCTS_H

struct HasPrivateFieldsOnly {
private:
  int priv1;
  int priv2;

public:
  HasPrivateFieldsOnly(int i1, int i2) : priv1(i1), priv2(i2) {}
};

struct HasPublicFieldsOnly {
  int publ1;
  int publ2;

  HasPublicFieldsOnly(int i1, int i2) : publ1(i1), publ2(i2) {}
};

struct HasPrivatePublicProtectedFields {
private:
  int priv1;

public:
  int publ1;

protected:
  int prot1;

protected:
  int prot2;

private:
  int priv2;

public:
  int publ2;

  HasPrivatePublicProtectedFields(int i1, int i2, int i3, int i4, int i5,
                                  int i6)
      : priv1(i1), publ1(i2), prot1(i3), prot2(i4), priv2(i5),
        publ2(i6) {}
};

struct Outer {
private:
  HasPrivatePublicProtectedFields privStruct;

public:
  HasPrivatePublicProtectedFields publStruct;

  Outer() : privStruct(1, 2, 3, 4, 5, 6), publStruct(7, 8, 9, 10, 11, 12) {}
};

struct ImmortalFRT {
private:
  int priv = 1;

public:
  int publ = 2;
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal")));

struct FRTCustomStringConvertible {
public:
private:
  int priv = 1;

public:
  int publ = 2;
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal")));

struct FRType {
private:
  int priv = 1;

public:
  int publ = 2;
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retain")))
__attribute__((swift_attr("release:release")));

void retain(FRType *v) {};
void release(FRType *v) {};

#endif
