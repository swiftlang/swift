#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_BIT_FIELDS_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_BIT_FIELDS_H

struct BitFields {
  unsigned a : 3;
  unsigned b : 5;
  unsigned c : 24;
};

struct NonCopyableBitFields {
  unsigned a : 3;
  unsigned b : 5;
  NonCopyableBitFields() : a(1), b(2) {}
  NonCopyableBitFields(NonCopyableBitFields &&) = default;
};

inline int copyCountingBitFieldsCopies = 0;

struct CopyCountingBitFields {
  unsigned a : 3;
  CopyCountingBitFields() : a(3) {}
  CopyCountingBitFields(const CopyCountingBitFields &other) : a(other.a) {
    ++copyCountingBitFieldsCopies;
  }
};

struct FirstBase {
  int x = 1;
};

struct BitFieldBase {
  unsigned b : 3;
};

struct BitFieldInSecondBase : FirstBase, BitFieldBase {};

struct BitFieldBehindVTable : BitFieldBase {
  virtual int virt() const { return 42; }
};

#endif // TEST_INTEROP_CXX_CLASS_INPUTS_BIT_FIELDS_H
