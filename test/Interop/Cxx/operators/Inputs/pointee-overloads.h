#pragma once

struct Pointee_Const {
  int x = 111;
  const int &operator*() const { return x; }
};

struct Pointee_NonConst {
  int x = 222;
  int &operator*() { return x; }
};

struct Pointee_Const_NonConst {
  int x = 333;
  int y = 444;
  const int &operator*() const { return x; }
  int &operator*() { return y; }
};

struct Pointee_NonConst_Const {
  int x = 333;
  int y = 444;
  int &operator*() { return y; }
  const int &operator*() const { return x; }
};

struct Pointee_NonConst_NonConst {
  int x = 333;
  int y = 444;
  const int &operator*() const { return x; }
  const int &operator*() { return y; }
};

struct Pointee_Volatile {
  int x = 404;
  volatile int &operator*() volatile { return x; }
};

struct Pointee_ConstVolatile {
  int x = 404;
  const volatile int &operator*() const volatile { return x; }
};

struct Pointee_Volatile_Const {
  int x = 404;
  int y = 555;
  volatile int &operator*() volatile { return x; }
  const int &operator*() const { return y; }
};

struct Pointee_NonConstGetter {
  int x = 666;
  const int &operator*() { return x; }
};

struct Pointee_MutableConst {
  mutable int x = 666;
  int &operator*() const { return x; }
};

struct Pointee_LConst {
  int x = 1111;
  const int &operator*() const & { return x; }
};

struct Pointee_LNonConst {
  int x = 2222;
  int &operator*() & { return x; }
};

struct Pointee_LConst_LNonConst {
  int x = 3333;
  int y = 4444;
  const int &operator*() const & { return x; }
  int &operator*() & { return y; }
};

struct Pointee_LNonConst_LConst {
  int x = 3333;
  int y = 4444;
  int &operator*() & { return y; }
  const int &operator*() const & { return x; }
};

struct Pointee_LConst_RConst {
  int x = 5555;
  int y = 6666;
  const int &operator*() const & { return x; }
  const int &operator*() const && { return y; }
};

struct Pointee_LNonConst_RNonConst {
  int x = 5555;
  int y = 6666;
  int &operator*() & { return x; }
  int &operator*() && { return y; }
};
