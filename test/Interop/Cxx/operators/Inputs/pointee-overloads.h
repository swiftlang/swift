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

struct FRT_Const_NonConst {
  void ref() const { ++count; }
  void deref() const { if (--count <= 0) delete this; }

  const int& operator*() const { return value; }
  int& operator*() { return value; }

  FRT_Const_NonConst(int value) : value{value}, count{1} {}

  int value;
  mutable int count;
} __attribute__((swift_attr("import_reference")))
  __attribute__((swift_attr("retain:.ref")))
  __attribute__((swift_attr("release:.deref")));

__attribute__((swift_attr("returns_retained")))
inline FRT_Const_NonConst * _Nonnull new_FRT_Const_NonConst(int value) {
  return new FRT_Const_NonConst{value};
}

template<typename T>
struct Template_FRT_Const_NonConst {
  void ref() const { ++count; }
  void deref() const { if (--count <= 0) delete this; }

  const T& operator*() const { return value; }
  T& operator*() { return value; }

  Template_FRT_Const_NonConst(T value) : value{value}, count{1} {}

  T value;
  mutable int count;
} __attribute__((swift_attr("import_reference")))
  __attribute__((swift_attr("retain:.ref")))
  __attribute__((swift_attr("release:.deref")));

__attribute__((swift_attr("returns_retained")))
inline Template_FRT_Const_NonConst<int> * _Nonnull
Int_Template_FRT_Const_NonConst(int value) {
  return new Template_FRT_Const_NonConst{value};
}

__attribute__((swift_attr("returns_retained")))
inline Template_FRT_Const_NonConst<double> * _Nonnull
Double_Template_FRT_Const_NonConst(double value) {
  return new Template_FRT_Const_NonConst{value};
}
