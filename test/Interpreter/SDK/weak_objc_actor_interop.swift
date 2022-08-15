// RUN: %empty-directory(%t)
//
// RUN: cp %s %t/main.swift
// RUN: %target-clang -fno-objc-arc %S/Inputs/ObjCWeak/ObjCWeak.m -c -o %t/ObjCWeak.o
// RUN: %target-build-swift %t/main.swift -I %S/Inputs/ObjCWeak/ -Xlinker %t/ObjCWeak.o -o %t/weak_objc_actor_interop -Xfrontend -disable-access-control
// RUN: %target-codesign %t/weak_objc_actor_interop
// RUN: %target-run %t/weak_objc_actor_interop 2>&1 | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Foundation
import ObjCWeak

// Test that instances of Swift actors can be referenced using ObjC
// weak references.

@available(SwiftStdlib 5.1, *)
actor A {
  @objc nonisolated var description: String {
    return "Swift Actor"
  }
}

// This availability check will always be true, so we don't need to provide
// FileCheck with fake output in an else branch.
if #available(SwiftStdlib 5.1, *) {
  tryWeakReferencing { A() }
  // CHECK:       before giving up strong reference:
  // CHECK-NEXT:  Swift Actor
  // CHECK-NEXT:  after giving up strong reference:
  // CHECK-NEXT:  Gone
}
