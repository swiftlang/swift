#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_DESTRUCTORS_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_DESTRUCTORS_H

struct DummyStruct {};

struct __attribute__((swift_attr("import_unsafe")))
HasUserProvidedDestructorAndDummy {
  DummyStruct dummy;
  ~HasUserProvidedDestructorAndDummy() {}
};

struct __attribute__((swift_attr("import_unsafe"))) HasUserProvidedDestructor {
  int *value;
  ~HasUserProvidedDestructor() { *value = 42; }
};

struct HasNonTrivialImplicitDestructor {
  HasUserProvidedDestructor member;
};

#endif // TEST_INTEROP_CXX_CLASS_INPUTS_DESTRUCTORS_H
