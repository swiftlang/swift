// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(COM)) -Xfrontend -enable-experimental-com-interop -Xfrontend -com-interop-model=microsoft -module-name COM -module-link-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-build-swift -Xfrontend -enable-experimental-com-interop -Xfrontend -com-interop-model=microsoft -I %t -module-name main %s -o %t/test.exe -L %t %target-rpath(%t)
// RUN: %target-codesign %t/test.exe %t/%target-library-name(COM)
// RUN: %target-run %t/test.exe | %FileCheck %s

// REQUIRES: executable_test

import COM

@com(interface: "37000000-0000-0000-0000-000000000001")
protocol IFirst {
  func first()
}

@com(interface: "37000000-0000-0000-0000-000000000002")
protocol ISecond {
  func second()
}

@com
final class IdentityObject: IFirst, ISecond {
  func first() {}
  func second() {}
}

@inline(never)
func projectFirst(_ object: IdentityObject) -> any IFirst {
  return object
}

@inline(never)
func projectSecond(_ object: IdentityObject) -> any ISecond {
  return object
}

do {
  let object = IdentityObject()
  let first = projectFirst(object)
  let second = projectSecond(object)
  let other = projectFirst(IdentityObject())

  print("same identity: \(first === second)")
  print("different identity: \(first === other)")
  print("identity operator: \(first !== second)")
  print("identifier: \(ObjectIdentifier(first) == ObjectIdentifier(second))")
  print("different identifier: \(ObjectIdentifier(first) == ObjectIdentifier(other))")

  let optionalFirst: (any IFirst)? = first
  let optionalSecond: (any ISecond)? = second
  let noFirst: (any IFirst)? = nil
  let noSecond: (any ISecond)? = nil
  print("optional identity: \(optionalFirst === optionalSecond)")
  print("nil identity: \(noFirst === noSecond)")
  print("nil and value: \(noFirst === optionalSecond)")
}

// CHECK:      same identity: true
// CHECK-NEXT: different identity: false
// CHECK-NEXT: identity operator: false
// CHECK-NEXT: identifier: true
// CHECK-NEXT: different identifier: false
// CHECK-NEXT: optional identity: true
// CHECK-NEXT: nil identity: true
// CHECK-NEXT: nil and value: false
