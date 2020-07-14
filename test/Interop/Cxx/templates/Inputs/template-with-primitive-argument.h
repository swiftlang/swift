#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_TEMPLATE_WITH_PRIMITIVE_ARGUMENT_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_TEMPLATE_WITH_PRIMITIVE_ARGUMENT_H

template<class T>
struct Tpl {
public:
  T t;
  inline int callMethod() const {
    return t + 5;
  }
};

typedef Tpl<int> HasPrimitiveArgument;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_TEMPLATE_WITH_PRIMITIVE_ARGUMENT_H
