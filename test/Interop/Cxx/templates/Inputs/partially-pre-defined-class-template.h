#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITH_DEFINITION_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITH_DEFINITION_H

template<class T>
struct MagicWrapper {
  T t;
  int getValuePlusArg(int arg) const { return t.getValue() + arg; }
};

struct IntWrapper {
  int value;
  int getValue() const { return value; }
};

inline MagicWrapper<IntWrapper> forceInstantiation() {
  return MagicWrapper<IntWrapper>();
}

// The ClassTemplateSpecializationDecl node for MagicWrapper<IntWrapper> already has a definition
// because function above forced the instantiation. Its members are not
// instantiated though, the Swift compiler needs to instantiate them.
typedef MagicWrapper<IntWrapper> PartiallyPreDefinedMagicallyWrappedInt;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITH_DEFINITION_H
