#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITHOUT_DEFINITION_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITHOUT_DEFINITION_H

template<class T>
struct MagicWrapper {
public:
  T t;
  int callGetInt() const {
    return t.getInt() + 5;
  }
};

struct MagicNumber {
  int getInt() const { return 12; }
};

// MagicWrapper<MagicNumber> ClassTemplateSpecializationDecl doesn't have a definition in Clang
// because nothing in this header required the instantiation. Therefore we have
// to construct the definition on the swift side.
typedef MagicWrapper<MagicNumber> WrappedMagicNumberWithoutDefinition;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITHOUT_DEFINITION_H
