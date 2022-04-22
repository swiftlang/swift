#ifndef TEST_INTEROP_CXX_VALUE_WITNESS_TABLE_INPUTS_CUSTOM_DESTRUCTORS_H
#define TEST_INTEROP_CXX_VALUE_WITNESS_TABLE_INPUTS_CUSTOM_DESTRUCTORS_H

struct HasUserProvidedDestructor {
  int *value;
  ~HasUserProvidedDestructor() { *value = 42; }
};

struct HasEmptyDestructorAndMemberWithUserDefinedConstructor {
  HasUserProvidedDestructor member;
  ~HasEmptyDestructorAndMemberWithUserDefinedConstructor() { /* empty */
  }
};

struct HasNonTrivialImplicitDestructor {
  HasUserProvidedDestructor member;
};

struct HasNonTrivialDefaultedDestructor {
  HasUserProvidedDestructor member;
  ~HasNonTrivialDefaultedDestructor() = default;
};

struct HasDefaultedDestructor {
  ~HasDefaultedDestructor() = default;
};

// For the following objects with virtual bases / destructors, make sure that
// any executable user of these objects disable rtti and exceptions. Otherwise,
// the linker will error because of undefined vtables.
// FIXME: Once we can link with libc++ we can enable RTTI.

struct HasVirtualBaseAndDestructor : virtual HasDefaultedDestructor {
  int *value;
  HasVirtualBaseAndDestructor(int *value) : value(value) {}
  ~HasVirtualBaseAndDestructor() { *value = 42; }
};

struct HasVirtualDestructor {
  // An object with a virtual destructor requires a delete operator in case
  // we try to delete the base object. Until we can link against libc++, use
  // this dummy implementation.
  static void operator delete(void *p) { __builtin_unreachable(); }
  virtual ~HasVirtualDestructor(){};
};

struct HasVirtualDefaultedDestructor {
  static void operator delete(void *p) { __builtin_unreachable(); }
  virtual ~HasVirtualDefaultedDestructor() = default;
};

struct HasBaseWithVirtualDestructor : HasVirtualDestructor {
  int *value;
  HasBaseWithVirtualDestructor(int *value) : value(value) {}
  ~HasBaseWithVirtualDestructor() { *value = 42; }
};

struct HasVirtualBaseWithVirtualDestructor : virtual HasVirtualDestructor {
  int *value;
  HasVirtualBaseWithVirtualDestructor(int *value) : value(value) {}
  ~HasVirtualBaseWithVirtualDestructor() { *value = 42; }
};

struct DummyStruct {};

struct HasUserProvidedDestructorAndDummy {
  DummyStruct dummy;
  ~HasUserProvidedDestructorAndDummy() {}
};

// Make sure that we don't crash on struct templates with destructors.
template <typename T> struct S {
  ~S() {}
};

#endif // TEST_INTEROP_CXX_VALUE_WITNESS_TABLE_INPUTS_CUSTOM_DESTRUCTORS_H
