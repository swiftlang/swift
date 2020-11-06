#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_MAGIC_WRAPPER_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_MAGIC_WRAPPER_H

struct IntWrapper {
  int value;
  int getValue() const { return value; }
};

template<class T>
struct MagicWrapper {
  T t;
  int getValuePlusArg(int arg) const { return t.getValue() + arg; }
};

template<>
struct MagicWrapper<int> {
  int i;
  int getValuePlusArg(int arg) const { return i + arg; }
};

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_MAGIC_WRAPPER_H
