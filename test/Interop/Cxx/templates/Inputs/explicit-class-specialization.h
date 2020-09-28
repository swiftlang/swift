#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_EXPLICIT_SPECIALIZATION_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_EXPLICIT_SPECIALIZATION_H

struct SpecializedIntWrapper {
  int value;
  int getValue() const { return value; }
};

struct NonSpecializedIntWrapper {
  int value;
  int getValue() const { return value; }
};

template <class T>
struct MagicWrapper {
  T t;
  int doubleIfSpecializedElseTriple() const { return 3 * t.getValue(); }
};

template <>
struct MagicWrapper<SpecializedIntWrapper> {
  SpecializedIntWrapper t;
  int doubleIfSpecializedElseTriple() const { return 2 * t.getValue(); }
};

typedef MagicWrapper<SpecializedIntWrapper> WrapperWithSpecialization;
typedef MagicWrapper<NonSpecializedIntWrapper> WrapperWithoutSpecialization;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_EXPLICIT_SPECIALIZATION_H
