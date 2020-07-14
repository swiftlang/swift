#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITH_DEFINITION_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITH_DEFINITION_H

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
  inline int method() const { return 48; }
};

inline Tpl<Arg> forceInstantiating() {
  return Tpl<Arg>();
}

// Tpl<Arg> ClassTemplateSpecializationDecl has definition because function
// above forced the instantiation. It's members are not instantiated, we need to
// instantiate them in Swift.
typedef Tpl<Arg> DeclWithDefinition;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITH_DEFINITION_H
