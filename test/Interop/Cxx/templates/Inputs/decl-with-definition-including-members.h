#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITH_DEFINITION_INCLUDING_MEMBERS_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITH_DEFINITION_INCLUDING_MEMBERS_H

template<class T>
struct MagicWrapper {
  T t;
  int callGetInt() const {
    return t.getInt() + 5;
  }
};

struct MagicNumber {
  int getInt() const { return 48; }
};

inline int forceInstantiating() {
  auto t = MagicWrapper<MagicNumber>();
  return t.callGetInt();
}

// MagicWrapper<MagicNumber> ClassTemplateSpecializationDecl has definition
// because function above forced the instantiation. Its members are fully
// instantiated, so nothing needs to be explicitly instantiated in swift.
typedef MagicWrapper<MagicNumber> FullyDefinedWrappedMagicNumber;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITH_DEFINITION_INCLUDING_MEMBERS_H
