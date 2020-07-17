#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_CANONICAL_TYPES_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_CANONICAL_TYPES_H

template<class T>
struct MagicWrapper {
  T t;
  int callGetInt() const {
    return t.getInt() + 5;
  }
};

struct MagicNumber {
  int getInt() const { return 24; }
};

typedef MagicWrapper<MagicNumber> WrappedMagicNumberA;
typedef MagicWrapper<MagicNumber> WrappedMagicNumberB;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_CANONICAL_TYPES_H
