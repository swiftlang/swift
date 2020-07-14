#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_CANONICAL_TYPES_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_CANONICAL_TYPES_H

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
  inline int method() const { return 24; }
};

typedef Tpl<Arg> TplA;
typedef Tpl<Arg> TplB;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_CANONICAL_TYPES_H
