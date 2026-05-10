#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_INSTANTIATION_ERRORS_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_INSTANTIATION_ERRORS_H

template<class T>
struct MagicWrapper {
  T t;
  int getValuePlusArg(int arg) const { return t.getValue() + arg; }
};

template<class T>
struct MagicWrapperWithExplicitCtor {
  T t;
  MagicWrapperWithExplicitCtor(T t) : t(t) {}
};

struct IntWrapper {
  int value;
  int getValue() const { return value; }
};

template<class T>
struct CannotBeInstantianted {
  T value;

  CannotBeInstantianted(char, T value) { value.doesNotExist(); }
  CannotBeInstantianted(char, char) { memberWrongType(); }
  CannotBeInstantianted(T value) : value(value) {}

  void callsMethodWithError() { memberWrongType(); }

  void memberWrongType() { value.doesNotExist(); }

  void argWrongType(T t) { t.doesNotExist(); }

  int getOne() { return 1; }
  int incValue() { return value.value + getOne(); }
  int incValue(T t) { return t.value + getOne(); }
};

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_INSTANTIATION_ERRORS_H
