#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_USING_DIRECTIVE_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_USING_DIRECTIVE_H

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
  inline int method() const { return 6; }
};

using ComesFromUsingDirective = Tpl<Arg>;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_USING_DIRECTIVE_H
