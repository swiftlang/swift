// Verify that a private nested type's descriptor gets correct linkage when
// compiled with -enable-testing, so that @testable importers can use the
// enclosing ~Copyable type without hitting "Global is external, but doesn't
// have external or weak linkage!" from the LLVM verifier.
//
// When the test module needs to destroy Foo, the outline destroy helper
// instantiates Mutex<State> metadata via a symbolic mangled type string that
// contains a reference to State's nominal type descriptor. Because State is
// private, its descriptor gets SILLinkage::Private → InternalLinkage, which
// is invalid for a cross-module declaration.

// REQUIRES: synchronization

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module \
// RUN:   -enable-testing \
// RUN:   -disable-availability-checking \
// RUN:   -swift-version 6 \
// RUN:   -parse-as-library \
// RUN:   -module-name Lib \
// RUN:   %t/Lib.swift \
// RUN:   -emit-module-path %t/Lib.swiftmodule

// RUN: %target-swift-frontend -c \
// RUN:   -enable-testing \
// RUN:   -disable-availability-checking \
// RUN:   -swift-version 6 \
// RUN:   -parse-as-library \
// RUN:   -I %t \
// RUN:   %t/Test.swift \
// RUN:   -module-name Test \
// RUN:   -o /dev/null

//--- Lib.swift
import Synchronization

struct Foo: ~Copyable {
  private struct State {
    var data: [Int] = []
  }
  private let mutex = Mutex(State())
}

//--- Test.swift
@testable import Lib

func test() {
  var foo = Foo()
}
