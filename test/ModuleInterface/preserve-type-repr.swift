// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -enable-library-evolution -emit-module-interface-path %t/External.swiftinterface -module-name External -DEXTERNAL
// RUN: %target-swift-frontend -typecheck -emit-module-interface-path - %s -enable-library-evolution -module-name PreferTypeRepr -module-interface-preserve-types-as-written -I %t | %FileCheck %s --check-prefix PREFER
// RUN: %target-swift-frontend -typecheck -emit-module-interface-path - %s -enable-library-evolution -module-name PreferTypeRepr -I %t | %FileCheck %s --check-prefix DONTPREFER

#if EXTERNAL
public struct Toy {
    public init() {}
}

public struct GenericToy<T> {
    public init() {}
}
#else
import External

// PREFER: extension Toy
// DONTPREFER: extension External.Toy
extension Toy {
    public static var new: Toy { Toy() }
}

// PREFER: extension GenericToy {
// DONTPREFER: extension External.GenericToy {
extension GenericToy {
    public static var new: GenericToy<T> { GenericToy() }
}

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
#endif
