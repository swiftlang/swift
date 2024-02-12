#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_WITH_PRIMITIVE_ARGUMENT_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_WITH_PRIMITIVE_ARGUMENT_H

template<class T>
struct MagicWrapper {
  T t;
  int getValuePlusArg(int arg) const { return t + arg; }
};

typedef MagicWrapper<int> WrappedMagicInt;
typedef MagicWrapper<int*> WrappedMagicIntPtr;
typedef MagicWrapper<const int*> WrappedMagicIntConstPtr;
typedef MagicWrapper<int**> WrappedMagicIntPtrPtr;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_WITH_PRIMITIVE_ARGUMENT_H
