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

template<template <typename> typename>
struct HasTemplateTemplateParam {};

template <typename> struct Traits1 {};
template <typename> struct Traits2 {};

using HasTraits1 = HasTemplateTemplateParam<Traits1>;
using HasTraits2 = HasTemplateTemplateParam<Traits2>;

struct Outer {
  template <typename> struct NestedTraits {};
};

using HasNestedTraits = HasTemplateTemplateParam<Outer::NestedTraits>;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_TEMPLATE_PARAMETER_H
