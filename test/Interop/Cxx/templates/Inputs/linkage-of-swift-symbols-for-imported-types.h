#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_LINKAGE_OF_SWIFT_SYMBOLS_FOR_IMPORTED_TYPES_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_LINKAGE_OF_SWIFT_SYMBOLS_FOR_IMPORTED_TYPES_H

template<class T>
struct MagicWrapper {
  T t;
  int callGetInt() const {
    return t.getInt() + 5;
  }
};

struct MagicNumber {
  // Swift runtime defines many value witness tables for types with some common layouts.
  // This struct's uncommon size forces the compiler to define a new value witness table instead of reusing one from the runtime.
  char forceVWTableCreation[57];
  int getInt() const { return 12; }
};

typedef MagicWrapper<MagicNumber> WrappedMagicNumber;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_LINKAGE_OF_SWIFT_SYMBOLS_FOR_IMPORTED_TYPES_H
