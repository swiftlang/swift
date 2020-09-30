#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITH_PRIMITIVE_ARGUMENT_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITH_PRIMITIVE_ARGUMENT_H

template<class T>
struct MagicWrapper {
  T t;
  int getValuePlusArg(int arg) const { return t + arg; }
};

typedef MagicWrapper<int> WrappedMagicInt;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_DECL_WITH_PRIMITIVE_ARGUMENT_H
