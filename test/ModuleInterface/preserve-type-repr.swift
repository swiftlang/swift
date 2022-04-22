// RUN: %empty-directory(%t)

// -- Generate an interface for the External module.
// RUN: %target-swift-emit-module-interface(%t/External.swiftinterface) %s -module-name External -DEXTERNAL
// RUN: %target-swift-typecheck-module-from-interface(%t/External.swiftinterface) -module-name External

// -- Check output with -module-interface-preserve-types-as-written.
// RUN: %target-swift-emit-module-interface(%t/Preserve.swiftinterface) %s -module-name PreferTypeRepr -module-interface-preserve-types-as-written -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/Preserve.swiftinterface) -module-name PreferTypeRepr -I %t
// RUN: %FileCheck --check-prefixes=CHECK,PREFER %s < %t/Preserve.swiftinterface

// -- Check output without the flag.
// RUN: %target-swift-emit-module-interface(%t/DontPreserve.swiftinterface) %s -module-name PreferTypeRepr -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/DontPreserve.swiftinterface) -module-name PreferTypeRepr -I %t
// RUN: %FileCheck --check-prefixes=CHECK,DONTPREFER %s < %t/DontPreserve.swiftinterface

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

// PREFER: extension My where T : PreferTypeRepr.Pet
// DONTPREFER: extension PreferTypeRepr.My where T : PreferTypeRepr.Pet
extension My where T: Pet {
  // PREFER: public func isPushingUpDaisies() -> String
  // DONTPREFER: public func isPushingUpDaisies() -> Swift.String
  public func isPushingUpDaisies() -> String { "" }
}

// PREFER: public func isNoMore(_ pet: Ex<Parrot>) -> Bool
// DONTPREFER: public func isNoMore(_ pet: PreferTypeRepr.Ex<PreferTypeRepr.Parrot>) -> Swift.Bool
public func isNoMore(_ pet: Ex<Parrot>) -> Bool {}
#endif
