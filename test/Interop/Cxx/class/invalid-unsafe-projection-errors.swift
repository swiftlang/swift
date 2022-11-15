// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -I %t/Inputs  %t/test.swift  -enable-experimental-cxx-interop

//--- Inputs/module.modulemap
module Test {
    header "test.h"
    requires cplusplus
}

//--- Inputs/test.h

struct Ptr { int *p; };

struct M {
        int *test1() const;
        int &test2() const;
        Ptr test3() const;

        int *begin() const;
};
//--- test.swift

import Test

public func test(x: M) {
  // expected-error@+2{{value of type 'M' has no member 'test1'}}
  // expected-note@+1{{C++ method 'test1' is unavailable in Swift as it returns an address (or struct containing an address).}}
  x.test1() 
  // expected-error@+2{{value of type 'M' has no member 'test2'}}
  // expected-note@+1{{the address returned by 'test2' may outlive the value it references resulting in an use-after-free.}}
  x.test2()
  // expected-error@+2{{value of type 'M' has no member 'test3'}}
  // expected-note@+1{{C++ method 'test3' is unavailable in Swift as it returns an address (or struct containing an address).}}
  x.test3()
  // expected-error@+2{{value of type 'M' has no member 'begin'}}
  // expected-note@+1{{'begin' is unavailable in Swift as it returns a C++ iterator; Use 'for-in' loop or 'Sequence' methods such as 'map', 'reduce' to iterate over the collection}}
  x.begin()
}



