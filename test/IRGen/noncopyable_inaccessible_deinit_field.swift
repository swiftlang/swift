// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -target %target-swift-6.2-abi-triple -emit-module -Onone -wmo -parse-as-library -module-name Lib -emit-module-path %t/Lib.swiftmodule %t/Lib.swift
// RUN: %target-swift-frontend -target %target-swift-6.2-abi-triple -emit-ir -Onone -wmo -parse-as-library -module-name main -I %t %t/Main.swift | %FileCheck %s --implicit-check-not='$s3Lib3MidV' --implicit-check-not='$s3Lib1EO' --implicit-check-not='$s3Lib5PairEO' --implicit-check-not='$s3Lib5InnerV'

// Check that a client destroys a struct through the struct's own metadata when a field
// contains a value whose type has a deinit and metadata the client cannot access.

// CHECK-LABEL: define {{.*}} @"$s4main13destroyNestedyy3Lib0C7ManagerVnF"(
// CHECK:   call void %Destroy(ptr noalias %{{.*}}, ptr @"$s3Lib13NestedManagerVN")
// CHECK:   ret void

// CHECK-LABEL: define {{.*}} @"$s4main11destroyEnumyy3Lib0C7ManagerVnF"(
// CHECK:   call void %Destroy(ptr noalias %{{.*}}, ptr @"$s3Lib11EnumManagerVN")
// CHECK:   ret void

// CHECK-LABEL: define {{.*}} @"$s4main11destroyPairyy3Lib0C7ManagerVnF"(
// CHECK:   call void %Destroy(ptr noalias %{{.*}}, ptr @"$s3Lib11PairManagerVN")
// CHECK:   ret void

// CHECK-LABEL: define {{.*}} @"$s4main14destroyGenericyy3Lib0C7ManagerVnF"(
// CHECK:   call void %Destroy(ptr noalias %{{.*}}, ptr @"$s3Lib14GenericManagerVN")
// CHECK:   ret void

// CHECK-LABEL: define {{.*}} @"$s4main12destroyArrayyy3Lib0C7ManagerVnF"(
// CHECK:   call void %Destroy(ptr noalias %{{.*}}, ptr @"$s3Lib12ArrayManagerVN")
// CHECK:   ret void

// The payload of an indirect case is a box, which the client can release itself.
// CHECK-LABEL: define {{.*}} @"$s4main11destroyTreeyy3Lib0C7ManagerVnF"(
// CHECK:   call ptr @"$s3Lib11TreeManagerVWOh"(ptr %{{.*}})
// CHECK:   ret void

//--- Lib.swift

struct Inner: ~Copyable {
  let p: UnsafeMutablePointer<Int>
  init() { p = .allocate(capacity: 1) }
  deinit { p.deallocate() }
}

struct Mid: ~Copyable {
  var inner: Inner
  init() { inner = Inner() }
}

enum E: ~Copyable {
  case empty
  case some(Inner)
}

enum PairE: ~Copyable {
  case empty
  case pair(Inner, Int)
}

public struct Box<T: ~Copyable>: ~Copyable {
  var value: T
  init(_ v: consuming T) { value = v }
}

public struct NestedManager: ~Copyable {
  var mid: Mid
  public init() { mid = Mid() }
}

public struct EnumManager: ~Copyable {
  var e: E
  public init() { e = .some(Inner()) }
}

public struct PairManager: ~Copyable {
  var e: PairE
  public init() { e = .pair(Inner(), 0) }
}

public struct GenericManager: ~Copyable {
  var box: Box<Inner>
  public init() { box = Box(Inner()) }
}

public struct ArrayManager: ~Copyable {
  var arr: InlineArray<2, Inner>
  public init() { arr = InlineArray { _ in Inner() } }
}

public enum Tree<T: ~Copyable>: ~Copyable {
  case empty
  indirect case node(Tree<T>, T, Tree<T>)
}

extension Tree: Copyable where T: Copyable {}

public struct TreeManager: ~Copyable {
  var tree: Tree<Inner>
  public init() { tree = .node(.empty, Inner(), .empty) }
}

//--- Main.swift

import Lib

public func destroyNested(_ m: consuming NestedManager) {}
public func destroyEnum(_ m: consuming EnumManager) {}
public func destroyPair(_ m: consuming PairManager) {}
public func destroyGeneric(_ m: consuming GenericManager) {}
public func destroyArray(_ m: consuming ArrayManager) {}
public func destroyTree(_ m: consuming TreeManager) {}
