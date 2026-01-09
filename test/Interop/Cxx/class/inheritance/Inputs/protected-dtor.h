#pragma once
#include <iostream>

// expected-note@+1 {{record 'InheritMe' is not automatically available}}
struct InheritMe {
   int fromBase = 111;
protected:
  ~InheritMe() { std::cout << "~InheritMe(fromBase = " << fromBase << ")\n"; }
};

struct Derived : InheritMe {
  int inDerived = 222;
};
