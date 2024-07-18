#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_WITH_PRIMITIVE_ARGUMENT_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_WITH_PRIMITIVE_ARGUMENT_H

template<class T>
struct MagicWrapper {
  T t;
  int getValuePlusArg(int arg) const { return t + arg; }
};

template<class M>
struct DoubleWrapper {
  M m;
  int getValuePlusArg(int arg) const { return m.getValuePlusArg(arg); }
};

typedef MagicWrapper<int> WrappedMagicInt;
typedef MagicWrapper<const int> WrappedMagicIntConst;
typedef MagicWrapper<const long> WrappedMagicLongConst;
typedef MagicWrapper<int*> WrappedMagicIntPtr;
typedef MagicWrapper<const int*> WrappedMagicIntConstPtr;
typedef MagicWrapper<int**> WrappedMagicIntPtrPtr;

typedef DoubleWrapper<MagicWrapper<int>> DoubleWrappedInt;
typedef DoubleWrapper<MagicWrapper<const int>> DoubleWrappedIntConst;
typedef DoubleWrapper<MagicWrapper<const long>> DoubleWrappedLongConst;
typedef DoubleWrapper<MagicWrapper<int*>> DoubleWrappedIntPtr;
typedef DoubleWrapper<MagicWrapper<const int*>> DoubleWrappedIntConstPtr;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_WITH_PRIMITIVE_ARGUMENT_H
