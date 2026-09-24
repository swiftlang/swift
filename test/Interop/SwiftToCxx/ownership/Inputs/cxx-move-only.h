#ifndef TEST_INTEROP_SWIFT_TO_CXX_OWNERSHIP_INPUTS_CXX_MOVE_ONLY_H
#define TEST_INTEROP_SWIFT_TO_CXX_OWNERSHIP_INPUTS_CXX_MOVE_ONLY_H

struct CxxMoveOnly {
  int value;

  CxxMoveOnly(int initialValue) : value(initialValue) {}
  CxxMoveOnly(const CxxMoveOnly &) = delete;
  CxxMoveOnly &operator=(const CxxMoveOnly &) = delete;
  CxxMoveOnly(CxxMoveOnly &&other) noexcept : value(other.value) {
    other.value = 0;
  }
  CxxMoveOnly &operator=(CxxMoveOnly &&other) noexcept {
    value = other.value;
    other.value = 0;
    return *this;
  }

  int get() const { return value; }
};

#endif
