#ifndef TEST_INTEROP_CXX_CLASS_METHOD_ALWAYS_UNSAFE_SAFE_WRAPPER_H
#define TEST_INTEROP_CXX_CLASS_METHOD_ALWAYS_UNSAFE_SAFE_WRAPPER_H

struct Element {
  int value;
};

// An owned type whose 'insert' returns an interior pointer, i.e. an unsafe
// projection. This is the shape that 'CxxSet' has in the C++ standard library,
// where the overlay hand-writes a same-named safe 'insert(_:)' wrapper.
struct Container {
  Element *storage;
  long size;
  Container(const Container &);

  Element *insert(int value);
  long count() const;
};

#endif // TEST_INTEROP_CXX_CLASS_METHOD_ALWAYS_UNSAFE_SAFE_WRAPPER_H
