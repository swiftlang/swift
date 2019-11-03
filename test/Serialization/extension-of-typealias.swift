// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -module-name Library -o %t -D LIBRARY %s
// RUN: %target-swift-ide-test -print-module -module-to-print=Library -I %t -source-filename=%s | %FileCheck %s
// RUN: %target-swift-frontend -typecheck -I %t %s -verify

// Check that base types of extensions are desugared. This isn't necessarily
// the behavior we want long-term, but it's the behavior we need right now.

#if LIBRARY

public typealias Zahl = Int

// CHECK: typealias List
// CHECK: typealias Zahl

// CHECK-LABEL: extension Int {
extension Zahl {
  // CHECK-NEXT: addedMember()
  public func addedMember() {}
} // CHECK-NEXT: {{^}$}}

public typealias List<T> = Array<T>

// CHECK-LABEL: extension List {
extension List {
  // CHECK-NEXT: addedMember()
  public func addedMember() {}
} // CHECK-NEXT: {{^}$}}

// CHECK-LABEL: extension List where Element == Int {
extension List where Element == Int {
  // CHECK-NEXT: addedMemberInt()
  public func addedMemberInt() {}
} // CHECK-NEXT: {{^}$}}

#else

import Library

func test(x: Int) {
  x.addedMember()
  [x].addedMember()
  [x].addedMemberInt()
  ([] as [Bool]).addedMemberInt() // expected-error {{referencing instance method 'addedMemberInt()' on 'Array' requires the types 'Bool' and 'Int' be equivalent}}
}

#endif
