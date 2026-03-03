// RUN: %target-swift-frontend -disable-objc-attr-requires-foundation-module -typecheck -verify %s -swift-version 5  -target %target-swift-5.1-abi-triple
// RUN: %target-swift-ide-test -skip-deinit=false -print-ast-typechecked -source-filename %s -function-definitions=true -prefer-type-repr=false -print-implicit-attrs=true -explode-pattern-binding-decls=true -disable-objc-attr-requires-foundation-module -swift-version 5  | %FileCheck %s
// REQUIRES: objc_interop
// REQUIRES: concurrency
import Foundation
import ObjectiveC

@objc protocol P {
  func doBigJob() async -> Int
}

// Infer @objc from protocol conformance
// CHECK: class ConformsToP
class ConformsToP: P {
  // CHECK: @objc func doBigJob() async -> Int
  func doBigJob() async -> Int { 5 }
}

// Infer @objc from superclass
class Super {
  @objc func longRunningRequest() async throws -> [String] { [] }
}

// CHECK: class Sub
class Sub : Super {
  // CHECK-NEXT: @objc override func longRunningRequest() async throws -> [String]
  override func longRunningRequest() async throws -> [String] { [] }
}

// Check selector computation.
@objc protocol MakeSelectors {
  func selectorAsync() async -> Int
  func selector(value: Int) async -> Int
}

func testSelectors() {
  // expected-warning@+1{{use '#selector' instead of explicitly constructing a 'Selector'}}
  _ = Selector("selectorAsyncWithCompletionHandler:")

  // expected-warning@+1{{use '#selector' instead of explicitly constructing a 'Selector'}}
  _ = Selector("selectorWithValue:completionHandler:")

  _ = Selector("canary:") // expected-warning{{no method declared with Objective-C selector 'canary:'}}
  // expected-note@-1{{wrap the selector name in parentheses to suppress this warning}}
}
