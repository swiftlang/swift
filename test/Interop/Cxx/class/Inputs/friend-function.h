#pragma once

struct A {
  A();
  void memberInA(int x) const;
};

struct B {
  // These should not get imported as members of B, because friendship means
  // nothing in Swift
  friend A::A();
  friend void A::memberInA(int) const;

  void memberInB() const;
};

// C should not mistakenly "inherit" the friend declarations in B
struct C : B {
  void memberInC() const;
};
