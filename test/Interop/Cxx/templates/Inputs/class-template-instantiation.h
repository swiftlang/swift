#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_INSTANTIATION_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_INSTANTIATION_H

struct IntWrapper {
  int value;
  int getValue() const { return value; }
};

template<class T>
struct MagicWrapper {
  T t;
  int getValuePlusArg(int arg) const { return t.getValue() + arg; }
  int getValuePlusArg(T arg) const { return t.getValue() + arg.getValue(); }
};

template<>
struct MagicWrapper<int> {
  int i;
  int getValuePlusArg(int arg) const { return i + arg; }
};

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_INSTANTIATION_H
