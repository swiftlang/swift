#pragma once

#include "swift/bridging"

struct MoveOnly {
  MoveOnly() {}
  MoveOnly(MoveOnly&&) {}
  MoveOnly(const MoveOnly&) = delete;
  ~MoveOnly() {}
};

class CxxForeignRef;

void retain(CxxForeignRef * obj);
void release(CxxForeignRef * obj);

class CxxForeignRef {
public:
  CxxForeignRef(const CxxForeignRef &) = delete;
  CxxForeignRef() = default;

  virtual void takesMoveOnly(MoveOnly);
} SWIFT_SHARED_REFERENCE(retain, release);
