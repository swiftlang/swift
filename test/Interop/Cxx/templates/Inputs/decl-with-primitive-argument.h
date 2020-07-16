#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITH_PRIMITIVE_ARGUMENT_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITH_PRIMITIVE_ARGUMENT_H

template<class T>
struct MagicWrapper {
public:
  T t;
  inline int getInt() const {
    return t + 5;
  }
};

typedef MagicWrapper<int> WrappedMagicInt;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITH_PRIMITIVE_ARGUMENT_H
