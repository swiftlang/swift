#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITHOUT_DEFINITION_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITHOUT_DEFINITION_H

template<class T>
struct Tpl {
public:
  T t;
  inline int callMethod() const {
    return t.method() + 5;
  }
};

struct Arg {
public:
  inline int method() const { return 12; }
};

// Tpl<Arg> ClassTemplateSpecializationDecl doesn't have a definition in Clang
// because nothing in this header required the instantiation. Therefore we have
// to construct the definition on the swift side.
typedef Tpl<Arg> DoesNotHaveDefinition;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITHOUT_DEFINITION_H
