// RUN: %target-swift-frontend %s -enable-objc-interop -import-objc-header %S/Inputs/duplicate_opened_archetypes.h -emit-sil -o /dev/null

// REQUIRES: objc_interop

// Check that SILGen does not crash because of duplicate opened archetypes
// in two functions.

import Foundation

struct S {
    let i: I
}

@propertyWrapper
public struct W<Value> {
    public var wrappedValue: Value

    public init(wrappedValue: Value) {
        self.wrappedValue = wrappedValue
    }

    public init(initialValue: Value) {
        wrappedValue = initialValue
    }

}


struct Test {
  @W private var s = S(i: .x)

  var t: S = S(i: .x)
}
