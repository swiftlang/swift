#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITH_DEFINITION_INCLUDING_MEMBERS_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITH_DEFINITION_INCLUDING_MEMBERS_H

template<class T>
struct MagicWrapper {
  T t;
  int getValuePlusArg(int arg) const { return t.getValue() + arg; }
};

struct IntWrapper {
  int value;
  int getValue() const { return value; }
};

inline int forceInstantiation() {
  auto t = MagicWrapper<IntWrapper>();
  return t.getValuePlusArg(14);
}

// The ClassTemplateSpecializationDecl node for MagicWrapper<IntWrapper> already has a definition
// because function above forced the instantiation. Its members are fully
// instantiated, so nothing needs to be explicitly instantiated by the Swift
// compiler.
typedef MagicWrapper<IntWrapper> FullyPreDefinedMagicallyWrappedInt;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITH_DEFINITION_INCLUDING_MEMBERS_H
