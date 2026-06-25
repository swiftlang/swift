// RUN: %empty-directory(%t/src)
// RUN: %empty-directory(%t/sdk)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %t/src/main.swift \
// RUN:   -import-objc-header %t/src/Test.h \
// RUN:   -strict-concurrency=complete \
// RUN:   -module-name main -I %t -verify

// REQUIRES: objc_interop
// REQUIRES: concurrency

//--- Test.h
#define NS_SWIFT_SENDABLE __attribute__((__swift_attr__("@Sendable")))

#pragma clang assume_nonnull begin

@import Foundation;

NS_SWIFT_SENDABLE
@protocol MyObjCProtocol <NSObject>
@property (readonly, nonatomic) NSString *test;
@end

@protocol MyRefinedObjCProtocol <MyObjCProtocol>
@end

#pragma clang assume_nonnull end

//--- main.swift
struct MyStruct: Sendable {
  let value: any MyObjCProtocol // Ok
}

func compute<T: Sendable>(_ t: T) {}

extension MyObjCProtocol {
  func test() {
    compute(self) // Ok
  }
}

class K : NSObject, MyObjCProtocol {
  // expected-warning@-1 {{non-final class 'K' cannot conform to the 'Sendable' protocol}}
  let test: String = "k"
}

class UncheckedK : NSObject, MyObjCProtocol, @unchecked Sendable { // ok
  let test: String = "unchecked"
}

class KRefined : NSObject, MyRefinedObjCProtocol {
  // expected-warning@-1 {{non-final class 'KRefined' cannot conform to the 'Sendable' protocol}}
  let test: String = "refined"
}

class KUncheckedRefined : NSObject, MyRefinedObjCProtocol, @unchecked Sendable { // Ok
  let test: String = "refined unchecked"
}

final class FinalK : NSObject, MyObjCProtocol { // Ok
  let test: String = "finalK"
}

@preconcurrency
protocol P : Sendable {
}

protocol Q : Sendable {
}

protocol SwiftRefinesObjCProtocol : MyObjCProtocol {}

do {
  class A : NSObject {}

  final class B : A, P {
    // expected-warning@-1:15 {{class 'B' cannot conform to the 'Sendable' protocol}}
    // expected-note@-2:15 {{a 'Sendable' class cannot inherit from a non-Sendable class}}
    // expected-note@-3:19 {{inherits from non-Sendable class 'A'}}
  }

  final class UncheckedB : A, P, @unchecked Sendable { // Ok
  }

  // TODO: 'C' is both non-final and inherits a non-Sendable superclass, so
  // it gets two similar warnings. Could be nice to collect all the reasons a
  // conformance is illegal into one diagnostic.
  class C : A, MyObjCProtocol {
    // expected-warning@-1:9 {{non-final class 'C' cannot conform to the 'Sendable' protocol}}
    // expected-warning@-2:9 {{class 'C' cannot conform to the 'Sendable' protocol}}
    // expected-note@-3:9 {{a 'Sendable' class cannot inherit from a non-Sendable class}}
    // expected-note@-4:13 {{inherits from non-Sendable class 'A'}}
    let test: String = "c"
  }

  class UncheckedC : A, MyObjCProtocol, @unchecked Sendable { // Ok
    let test: String = "c"
  }

  final class D : A, Q {
    // expected-warning@-1:15 {{class 'D' cannot conform to the 'Sendable' protocol; this is an error in the Swift 6 language mode}}
    // expected-note@-2:15 {{a 'Sendable' class cannot inherit from a non-Sendable class}}
    // expected-note@-3:19 {{inherits from non-Sendable class 'A'}}
  }

  final class SwiftRefinedSub : A, SwiftRefinesObjCProtocol {
    // expected-warning@-1:15 {{class 'SwiftRefinedSub' cannot conform to the 'Sendable' protocol}}
    // expected-note@-2:15 {{a 'Sendable' class cannot inherit from a non-Sendable class}}
    // expected-note@-3:33 {{inherits from non-Sendable class 'A'}}
    let test: String = "refinedSub"
  }

  // Conformance through P is @preconcurrency, downgrading the error even though Q isn't...
  final class PQSub : A, P, Q {
    // expected-warning@-1:15 {{class 'PQSub' cannot conform to the 'Sendable' protocol}}
    // expected-note@-2:15 {{a 'Sendable' class cannot inherit from a non-Sendable class}}
    // expected-note@-3:23 {{inherits from non-Sendable class 'A'}}
  }
}

do {
  func testSendable<T: Sendable>(_: T) {}

  testSendable(K()) // Ok
  testSendable(UncheckedK()) // Ok
  testSendable(KRefined()) // Ok
  testSendable(KUncheckedRefined()) // Ok
}
