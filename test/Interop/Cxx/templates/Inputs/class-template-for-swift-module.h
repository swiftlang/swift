#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_FOR_SWIFT_MODULE_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_FOR_SWIFT_MODULE_H

struct IntWrapper {
  int value;
  int getValue() const { return value; }
};

template<class T>
struct MagicWrapper {
  T t;
  int getValuePlusArg(int arg) const { return t.getValue() + arg; }
};

using MagicWrapperSpec = MagicWrapper<IntWrapper>;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_FOR_SWIFT_MODULE_H
