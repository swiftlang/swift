#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_DEBUG_INFO_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_DEBUG_INFO_H

struct IntWrapper {
  int value;
};

#define IMMORTAL_REF                                                           \
  __attribute__((swift_attr("import_as_ref")))                                 \
  __attribute__((swift_attr("retain:immortal")))                               \
  __attribute__((swift_attr("release:immortal")))

struct Passed {
  inline ~Passed() {}
};

struct IMMORTAL_REF CppReceiver {
  virtual void callMe(Passed str) = 0;
};

// struct SomeType {
//   int value;
// };

#endif // TEST_INTEROP_CXX_CLASS_INPUTS_DEBUG_INFO_H
