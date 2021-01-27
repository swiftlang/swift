#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_INSTANTIATION_ECISTING_SPECIALIZATION_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_INSTANTIATION_EXISTING_SPECIALIZATION_H

struct IntWrapper {
  int value;
  int getValue() const { return value; }
};

template<class T>
struct MagicWrapper {
  T t;
  int getValuePlusArg(int arg) const { return t.getValue() + arg; }
};

inline int forceInstantiation() {
  auto t = MagicWrapper<IntWrapper>();
  return t.getValuePlusArg(14);
}

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_INSTANTIATION_EXISTING_SPECIALIZATION_H
