#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_EXPLICIT_SPECIALIZATION_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_EXPLICIT_SPECIALIZATION_H

struct Arg {
public:
  inline int method() const { return 26; }
};

template <class T> struct Tpl {
public:
  T t;
  inline int callMethod() const { return t.method() + 5; }
};

template <> struct Tpl<Arg> {
  Arg t;
  int callMethod() const { return t.method() + 10; }
};

typedef Tpl<Arg> TplWithExplicitSpecialization;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_EXPLICIT_SPECIALIZATION_H
