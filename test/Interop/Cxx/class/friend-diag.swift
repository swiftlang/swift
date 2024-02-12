// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swiftxx-frontend -typecheck -I %t/Inputs %t/test.swift -verify

//--- Inputs/module.modulemap
module FriendClass {
  header "test.h"
  requires cplusplus
  export *
}

//--- Inputs/test.h

template <class T>
class B {
  ~B() = delete;
};

class D : public B<D> { // expected-note {{record 'D' is not automatically available}}
  friend class B<D>;
};

//--- test.swift

import FriendClass

func test() {
  var v: D // expected-error {{cannot find type 'D' in scope}}
}
