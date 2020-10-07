#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_MAGIC_WRAPPER_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_MAGIC_WRAPPER_H

template<class T>
struct MagicWrapper {
  T t;
  int getValuePlusArg(int arg) const { return t.getValue() + arg; }
};

struct IntWrapper {
  int value;
  int getValue() const { return value; }
};

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_MAGIC_WRAPPER_H
