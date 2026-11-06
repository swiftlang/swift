#ifndef TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_BORROW_ACCESSOR_OPTIONAL_CLOSURE_H
#define TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_BORROW_ACCESSOR_OPTIONAL_CLOSURE_H

#include <swift/bridging>

class SWIFT_SHARED_REFERENCE(.retain, .release) Object {
public:
  virtual void retain() const;
  virtual void release() const;
  virtual int value() const;
};

class Owner : public Object {
public:
  void *raw;
};

#endif
