#pragma once

#include "swift/bridging"

class CxxForeignRef;

void retain(CxxForeignRef * obj);
void release(CxxForeignRef * obj);

struct NonTrivial {
  NonTrivial(const NonTrivial &other);
  ~NonTrivial();
};

class CxxForeignRef {
public:
  CxxForeignRef(const CxxForeignRef &) = delete;
  CxxForeignRef() = default;

  virtual void takesRValRef(NonTrivial &&);
} SWIFT_SHARED_REFERENCE(retain, release);

