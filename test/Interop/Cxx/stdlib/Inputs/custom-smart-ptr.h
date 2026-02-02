#pragma once

static int copies2 = 0;

struct CountCopies2 {
  CountCopies2() = default;
  CountCopies2(const CountCopies2& other) { ++copies2; }
  ~CountCopies2() {}

  int getCopies() const { return copies2; }
  void method() {}
  void constMethod() const {}
  int field = 42;
};

struct MySmartPtr {
    CountCopies2& operator*() const [[clang::lifetimebound]] { return *ptr; }

    CountCopies2* ptr;
};

inline MySmartPtr getPtr() { return MySmartPtr{new CountCopies2()}; }
