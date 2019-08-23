#pragma once

class A {
public:
  virtual ~A() {}
};
class B {
public:
  virtual ~B() {}
  int v;
};
class C : public A, public B {
public:
  virtual ~C() {}
};

inline B *do_cast(C *v) { return v; }
