#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_DESTRUCTORS_WITH_TEMPORARY_VALUES_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_DESTRUCTORS_WITH_TEMPORARY_VALUES_H

struct Value {
  void referencedByDestructor() { r(); }

  double r();
};

void referencedByDestructor(Value *ptr) { ptr->referencedByDestructor(); }

struct Reference {
  ~Reference() {
    if (ptr)
      referencedByDestructor(ptr);
  }
  Value *ptr;
};

inline Reference getRef();
inline void takeDouble(double);

inline void testFunction() { takeDouble(getRef().ptr->r()); }

#endif // TEST_INTEROP_CXX_CLASS_INPUTS_DESTRUCTORS_WITH_TEMPORARY_VALUES_H
