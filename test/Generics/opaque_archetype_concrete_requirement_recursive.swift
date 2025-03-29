// RUN: %target-swift-frontend -typecheck -verify %s -target %target-swift-5.1-abi-triple -debug-generic-signatures 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen %s -target %target-swift-5.1-abi-triple

// FIXME: This does not work with -enable-requirement-machine-opaque-archetypes.
// See opaque_archetype_concrete_requirement_rejected.swift for a demonstration
// that it fails with the flag.

protocol P {
  associatedtype T

  var t: T { get }
}

protocol RecursiveP {
  associatedtype T : RecursiveP
}

struct S_RecursiveP : RecursiveP {
  typealias T = S_RecursiveP
}

struct DefinesRecursiveP : P {
  var t: some RecursiveP {
    return S_RecursiveP()
  }
}

protocol HasRecursiveP {
  associatedtype T : RecursiveP
}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=HasRecursiveP
// CHECK-NEXT: Generic signature: <Self where Self : HasRecursiveP, Self.[HasRecursiveP]T == some RecursiveP>
extension HasRecursiveP where T == DefinesRecursiveP.T {
  func checkSameType(_ t: T) -> DefinesRecursiveP.T { return t }
}
