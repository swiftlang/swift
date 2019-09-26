// RUN: %target-swift-frontend -typecheck -emit-module-interface-path - %s -enable-library-evolution -module-name PreferTypeRepr -module-interface-preserve-types-as-written | %FileCheck %s --check-prefix PREFER
// RUN: %target-swift-frontend -typecheck -emit-module-interface-path - %s -enable-library-evolution -module-name PreferTypeRepr | %FileCheck %s --check-prefix DONTPREFER

public protocol Pet {}

// PREFER: public struct Parrot : Pet {
// DONTPREFER: public struct Parrot : PreferTypeRepr.Pet {
// CHECK-NEXT: }
public struct Parrot: Pet {}

// CHECK: public struct Ex<T> where T : PreferTypeRepr.Pet {
public struct Ex<T: Pet> {
  // PREFER: public var hasCeasedToBe: Bool {
  // DONTPREFER: public var hasCeasedToBe: Swift.Bool {
  // CHECK: get
  // CHECK-NEXT: }
  public var hasCeasedToBe: Bool { false }

// CHECK-NEXT: }
}

// CHECK: public struct My<T> {
// CHECK-NEXT: }
public struct My<T> {}

// CHECK: extension My where T : PreserveTypeRepr.Pet
extension My where T: Pet {
  // CHECK: public func isPushingUpDaisies() -> Swift.String
  public func isPushingUpDaisies() -> String { "" }
}

// PREFER: public func isNoMore(_ pet: Ex<Parrot>) -> Bool
// DONTPREFER: public func isNoMore(_ pet: PreferTypeRepr.Ex<PreferTypeRepr.Parrot>) -> Swift.Bool
public func isNoMore(_ pet: Ex<Parrot>) -> Bool {}
