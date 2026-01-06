// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -I %t/Inputs -cxx-interoperability-mode=default -verify %t/test.swift

// REQUIRES: objc_interop

//--- Inputs/header.h

class Trivial {
public:
  int x;
};

class NonTrivial {
public:
  NonTrivial(const NonTrivial &other) : x(other.x) {}
  ~NonTrivial() { }

private:
  int x;
};

struct NonTrivialDestrOnly {
  ~NonTrivialDestrOnly() { }

private:
  int x;
};

//--- Inputs/module.modulemap

module NonTrivial {
    header "header.h"
    export *
}

//--- test.swift

import Foundation
import NonTrivial

@objc
class ObjCObject: NSObject {
  @objc var prop: NonTrivial // expected-error {{property cannot be marked '@objc' because its type cannot be represented in Objective-C}}
  // expected-note@-1 {{non-trivial C++ classes cannot be represented in Objective-C}}

  @objc var trivProp: Trivial

  override init() { fatalError() }

  @objc func getNonTrivial() -> NonTrivialDestrOnly { // expected-error {{method cannot be marked '@objc' because its result type cannot be represented in Objective-C}}
    // expected-note@-1 {{non-trivial C++ classes cannot be represented in Objective-C}}
    fatalError()
  }

  @objc func getTrivial() -> Trivial {
    return Trivial(x: 11)
  }
}
