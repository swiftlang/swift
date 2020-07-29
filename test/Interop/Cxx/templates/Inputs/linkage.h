#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_LINKAGE_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_LINKAGE_H

template<class T>
struct MagicWrapper {
  T t;
  int callGetInt() const {
    return t.getInt() + 5;
  }
};

struct MagicNumber {
  // Swift runtime defines many value witness tables for common types.
  // The struct's uncommon size forces creation of its own witness table.
  char forceVWTableCreation[57];
  int getInt() const { return 12; }
};

typedef MagicWrapper<MagicNumber> WrappedMagicNumber;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_LINKAGE_H
