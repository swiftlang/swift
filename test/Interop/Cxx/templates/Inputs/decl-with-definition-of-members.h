#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITH_DEFINITION_OF_MEMBERS_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITH_DEFINITION_OF_MEMBERS_H

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

inline int forceInstantiating() {
  auto t = Tpl<Arg>();
  return t.callMethod();
}

// Tpl<Arg> ClassTemplateSpecializationDecl has definition because function
// above forced the instantiation. Its members are fully instantiated, so
// nothing needs to be explicitly instantiated in swift.
typedef Tpl<Arg> DefinedMembers;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITH_DEFINITION_OF_MEMBERS_H
