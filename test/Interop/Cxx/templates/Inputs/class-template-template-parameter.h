#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_TEMPLATE_PARAMETER_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_TEMPLATE_PARAMETER_H

struct IntWrapper {
  int value;
  int getValue() const { return value; }
};

template<class T>
struct MagicWrapper {
  T t;
  int getValuePlusArg(int arg) const { return t.getValue() + arg; }
};

template<template <class> class V>
struct TemplatedMagicWrapper {
  V<IntWrapper> i;
  int getValuePlusTwiceTheArg(int arg) const { return i.getValuePlusArg(arg) + arg; }
};

typedef TemplatedMagicWrapper<MagicWrapper> TemplatedWrappedMagicInt;
typedef MagicWrapper<IntWrapper> WrappedMagicInt;
#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_TEMPLATE_PARAMETER_H
