#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITH_DEFINITION_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITH_DEFINITION_H

template<class T>
struct MagicWrapper {
public:
  T t;
  inline int callGetInt() const {
    return t.getInt() + 5;
  }
};

struct MagicNumber {
public:
  inline int getInt() const { return 24; }
};

inline MagicWrapper<MagicNumber> forceInstantiating() {
  return MagicWrapper<MagicNumber>();
}

// MagicWrapper<MagicNumber> ClassTemplateSpecializationDecl has definition
// because function above forced the instantiation. Its members are not
// instantiated though, we need to instantiate them in Swift.
typedef MagicWrapper<MagicNumber> PartiallyDefinedWrappedMagicNumber;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITH_DEFINITION_H
