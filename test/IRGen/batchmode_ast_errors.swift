// RUN: not %target-swift-frontend -primary-file %s %S/Inputs/batchmode_errors.swift -emit-ir 2>&1 | %FileCheck %s

// REQUIRES: objc_interop

// Verify that we don't crash (in IRGen).

// CHECK-NOT: Stack dump

extension SomeClass : SomeProto {
  func conform() {}
}

func genericParam<T: WithAssoc>(_ t: T) {
}

func testBuggyGenericParam() {
   genericParam(BuggyConformer())
}
