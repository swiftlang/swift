#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_WITH_PRIMITIVE_ARGUMENT_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_WITH_PRIMITIVE_ARGUMENT_H

#include <cstddef>

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
typedef MagicWrapper<int[]> WrappedMagicIntArr;
typedef MagicWrapper<long[]> WrappedMagicLongArr;
typedef MagicWrapper<int[123]> WrappedMagicIntFixedSizeArr1;
typedef MagicWrapper<int[124]> WrappedMagicIntFixedSizeArr2;
typedef MagicWrapper<std::nullptr_t> WrappedMagicNullPtr;

typedef DoubleWrapper<MagicWrapper<int>> DoubleWrappedInt;
typedef DoubleWrapper<MagicWrapper<const int>> DoubleWrappedIntConst;
typedef DoubleWrapper<MagicWrapper<const long>> DoubleWrappedLongConst;
typedef DoubleWrapper<MagicWrapper<int*>> DoubleWrappedIntPtr;
typedef DoubleWrapper<MagicWrapper<const int*>> DoubleWrappedIntConstPtr;
typedef DoubleWrapper<MagicWrapper<int[]>> DoubleWrappedMagicIntArr;
typedef DoubleWrapper<MagicWrapper<long[]>> DoubleWrappedMagicLongArr;
typedef DoubleWrapper<MagicWrapper<int[42]>> DoubleWrappedMagicIntFixedSizeArr1;
typedef DoubleWrapper<MagicWrapper<int[43]>> DoubleWrappedMagicIntFixedSizeArr2;
typedef DoubleWrapper<MagicWrapper<std::nullptr_t>> DoubleWrappedMagicNullPtr;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_WITH_PRIMITIVE_ARGUMENT_H
