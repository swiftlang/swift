#pragma once

struct One {
  int method(void) const { return 1; }
  int operator[](int i) const { return 1; }
};

struct IOne : One {
  int methodI(void) const { return -1; }
};

struct IIOne : IOne {
  int methodII(void) const { return -11; }
};

struct IIIOne : IIOne {
  int methodIII(void) const { return -111; }
};

class Base {
public:
  bool baseMethod() const { return true; }
};

namespace Bar {
class Derived : public Base {};
} // namespace Bar
