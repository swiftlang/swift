// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -com-interop-model=microsoft -I %t -module-name Witnesses -debug-generic-signatures > %t.dump 2>&1
// RUN: %FileCheck %s < %t.dump
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -com-interop-model=corefoundation -I %t

@com(interface: "10000000-0000-0000-0000-000000000001")
protocol IWidget {}

// Exercise both explicitly declared and inferred COM implementations.
@com
class Explicit {}

class Inferred: IWidget {}
class Generic<T>: IWidget {}
class Derived: Inferred {}

class Extended {}
extension Extended: IWidget {}

class MemberWitnesses: IWidget {
  var object: UnsafeMutableRawPointer { fatalError() }
  var metadata: UnsafeRawPointer { fatalError() }
}

func requiresIdentity<T: ISwiftObject>(_: T.Type) {}

func check<T>(_: T.Type) {
  requiresIdentity(Explicit.self)
  requiresIdentity(Inferred.self)
  requiresIdentity(Generic<T>.self)
  requiresIdentity(Derived.self)
  requiresIdentity(Extended.self)
  requiresIdentity(MemberWitnesses.self)
}

// CHECK: normal_conformance type="Explicit" protocol="ISwiftObject"
// CHECK: value req="object" witness="COM.(file).ISwiftObject extension.object
// CHECK: value req="metadata" witness="COM.(file).ISwiftObject extension.metadata
// CHECK: normal_conformance type="Inferred" protocol="ISwiftObject"
// CHECK: value req="object" witness="COM.(file).ISwiftObject extension.object
// CHECK: value req="metadata" witness="COM.(file).ISwiftObject extension.metadata
// CHECK: normal_conformance type="MemberWitnesses" protocol="ISwiftObject"
// CHECK: value req="object" witness="Witnesses.(file).MemberWitnesses.object
// CHECK: value req="metadata" witness="Witnesses.(file).MemberWitnesses.metadata
