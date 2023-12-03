// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -swift-version 5 -enable-library-evolution -emit-module \
// RUN:     -enable-experimental-feature NoncopyableGenerics \
// RUN:     -o %t/NoncopyableGenerics_Misc.swiftmodule \
// RUN:     -emit-module-interface-path %t/NoncopyableGenerics_Misc.swiftinterface \
// RUN:     %S/Inputs/NoncopyableGenerics_Misc.swift

// RUN: %target-swift-frontend -swift-version 5 -enable-library-evolution -emit-module \
// RUN:     -enable-experimental-feature NoncopyableGenerics \
// RUN:     -o %t/Swiftskell.swiftmodule \
// RUN:     -emit-module-interface-path %t/Swiftskell.swiftinterface \
// RUN:     %S/Inputs/Swiftskell.swift

// Check the interfaces

// RUN: %FileCheck %s --check-prefix=CHECK-MISC < %t/NoncopyableGenerics_Misc.swiftinterface
// RUN: %FileCheck %s < %t/Swiftskell.swiftinterface

// REQUIRES: asserts

import NoncopyableGenerics_Misc

// CHECK-MISC: public struct rdar118697289_S1<Element> where Element : Swift.Copyable {
// CHECK-MISC: public struct rdar118697289_S2<Element> where Element : Swift.Copyable {
// CHECK-MISC: public static func pickOne<T>(_ a: T, _ b: T) -> T where T : Swift.Copyable

// CHECK-MISC: extension NoncopyableGenerics_Misc._Toys : Swift.Copyable {}
// CHECK-MISC: extension NoncopyableGenerics_Misc._Toys.rdar118697289_S1 : Swift.Copyable {}
// CHECK-MISC: extension NoncopyableGenerics_Misc._Toys.rdar118697289_S2 : Swift.Copyable {}

import Swiftskell

// CHECK: #if compiler(>=5.3) && $NoncopyableGenerics
// CHECK-NEXT: public protocol Show {

// CHECK: #if compiler(>=5.3) && $NoncopyableGenerics
// CHECK-NEXT: public func print(_ s: borrowing some Show & ~Copyable)

// CHECK: #if compiler(>=5.3) && $NoncopyableGenerics
// CHECK-NEXT: public protocol Eq {

// CHECK: #if compiler(>=5.3) && $NoncopyableGenerics
// CHECK-NEXT: extension Swiftskell.Eq {

// CHECK: #if compiler(>=5.3) && $NoncopyableGenerics
// CHECK-NEXT: extension Swiftskell.Eq where Self : Swift.Equatable {

// CHECK: #if compiler(>=5.3) && $NoncopyableGenerics
// CHECK-NEXT: public struct Vector<T> {

// CHECK: #if compiler(>=5.3) && $NoncopyableGenerics
// CHECK-NEXT: public enum Maybe<Value> {

// CHECK: #if compiler(>=5.3) && $NoncopyableGenerics
// CHECK-NEXT: extension Swiftskell.Maybe : Swiftskell.Show {

// CHECK: #if compiler(>=5.3) && $NoncopyableGenerics
// CHECK-NEXT: extension Swiftskell.Maybe : Swiftskell.Eq where Value : Swiftskell.Eq {

// CHECK: #if compiler(>=5.3) && $NoncopyableGenerics
// CHECK-NEXT: public func maybe<A, B>(_ defaultVal: B, _ fn: (consuming A) -> B) -> (consuming Swiftskell.Maybe<A>) -> B where B : Swift.Copyable

// CHECK: #if compiler(>=5.3) && $NoncopyableGenerics
// CHECK-NEXT: @inlinable public func fromMaybe<A>(_ defaultVal: A) -> (Swiftskell.Maybe<A>) -> A where A : Swift.Copyable {

// CHECK: #if compiler(>=5.3) && $NoncopyableGenerics
// CHECK-NEXT: public func isJust<A>(_ m: borrowing Swiftskell.Maybe<A>) -> Swift.Bool


struct FileDescriptor: ~Copyable, Eq, Show {
  let id: Int

  func show() -> String {
    return "FileDescriptor(id:\(id))"
  }

  static func ==(_ a: borrowing Self, _ b: borrowing Self) -> Bool {
    return a.id == b.id
  }
}
