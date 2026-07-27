// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -typecheck -I %S/Inputs/custom-modules -D ONE_MODULE %s -verify
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -typecheck -I %S/Inputs/custom-modules -D SUB_MODULE %s -verify
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -typecheck -I %S/Inputs/custom-modules -D TWO_MODULES %s -verify

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -enable-objc-interop -print-module -module-to-print RedeclaredProperties -I %S/Inputs/custom-modules -source-filename %s | %FileCheck %s
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -enable-objc-interop -print-module -module-to-print RedeclaredPropertiesSub -module-to-print RedeclaredPropertiesSub.Private -I %S/Inputs/custom-modules -source-filename %s | %FileCheck %s --check-prefixes=CHECK,CHECK-SUB
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -enable-objc-interop -print-module -module-to-print RedeclaredPropertiesSplit -module-to-print RedeclaredPropertiesSplit2 -I %S/Inputs/custom-modules -source-filename %s | %FileCheck %s --check-prefixes=CHECK,CHECK-TWO

// REQUIRES: objc_interop

#if ONE_MODULE
import RedeclaredProperties
#elseif SUB_MODULE
import RedeclaredPropertiesSub
import RedeclaredPropertiesSub.Private
#elseif TWO_MODULES
import RedeclaredPropertiesSplit
import RedeclaredPropertiesSplit2
#endif

func test(obj: RPFoo) {
  if let _ = obj.nonnullToNullable {} // expected-error {{initializer for conditional binding must have Optional type}}
  obj.nonnullToNullable = obj // expected-error {{cannot assign to property: 'nonnullToNullable' is a get-only property}}
  // expected-error@-1 {{cannot assign value of type 'RPFoo' to type 'UnsafeMutablePointer<CInt>' (aka 'UnsafeMutablePointer<Int32>')}}

  if let _ = obj.nullableToNonnull {} // okay
  obj.nullableToNonnull = obj // expected-error {{cannot assign to property: 'nullableToNonnull' is a get-only property}}
  // expected-error@-1 {{cannot assign value of type 'RPFoo' to type 'UnsafeMutablePointer<CInt>' (aka 'UnsafeMutablePointer<Int32>')}}

  let _: RPFoo = obj.typeChangeMoreSpecific // expected-error {{cannot convert value of type 'Any' to specified type 'RPFoo'}}
  obj.typeChangeMoreSpecific = obj // expected-error {{cannot assign to property: 'typeChangeMoreSpecific' is a get-only property}}

  let _: RPFoo = obj.typeChangeMoreGeneral
  obj.typeChangeMoreGeneral = obj // expected-error {{cannot assign to property: 'typeChangeMoreGeneral' is a get-only property}}

  if let _ = obj.accessorRedeclaredAsNullable {} // expected-error {{initializer for conditional binding must have Optional type}}
  if let _ = obj.accessorDeclaredFirstAsNullable {} // expected-error {{initializer for conditional binding must have Optional type}}

  obj.accessorInProto = nil // okay

  // A readonly weak/assign/strong object property redeclared as readwrite in a
  // class extension imports with a setter regardless of ownership.
  obj.weakRedeclared = obj // okay
  obj.assignRedeclared = obj // okay
  obj.strongRedeclared = obj // okay
}

// https://github.com/apple/swift/issues/51011
func f_51011(obj: RPSub) {
  obj.accessorInProto = nil
}

// CHECK-SUB:      import RedeclaredPropertiesSub.Private
// CHECK-SUB-EMPTY:

// CHECK:      protocol RPProto {
// CHECK-NEXT:   func accessorInProto() -> Any?
// CHECK-NEXT: }
// CHECK-NEXT: class RPFoo : RPProto {
// CHECK-NEXT:   var nonnullToNullable: UnsafeMutablePointer<CInt> { get }
// CHECK-NEXT:   var nullableToNonnull: UnsafeMutablePointer<CInt>? { get }
// CHECK-NEXT:   var typeChangeMoreSpecific: Any { get }
// CHECK-NEXT:   var typeChangeMoreGeneral: RPFoo { get }
// CHECK-NEXT:   var accessorRedeclaredAsNullable: Any { get }
// CHECK-NEXT:   class func accessorRedeclaredAsNullable() -> Any
// CHECK-NEXT:   class func accessorDeclaredFirstAsNullable() -> Any
// CHECK-NEXT:   var accessorDeclaredFirstAsNullable: Any { get }
// CHECK-NEXT:   var accessorInProto: Any? { get }
// CHECK-NEXT:   weak var weakRedeclared: @sil_weak RPFoo? { get }
// CHECK-NEXT:   unowned(unsafe) var assignRedeclared: @sil_unmanaged RPFoo? { get }
// CHECK-NEXT:   var strongRedeclared: RPFoo? { get }
// CHECK-NEXT:   class func nonnullToNullable() -> UnsafeMutablePointer<CInt>
// CHECK-NEXT:   class func nullableToNonnull() -> UnsafeMutablePointer<CInt>?
// CHECK-NEXT:   class func typeChangeMoreSpecific() -> Any
// CHECK-NEXT:   class func typeChangeMoreGeneral() -> RPFoo
// CHECK-NEXT:   class func accessorInProto() -> Any?
// CHECK-NEXT:   class func weakRedeclared() -> RPFoo?
// CHECK-NEXT:   class func assignRedeclared() -> RPFoo?
// CHECK-NEXT:   class func strongRedeclared() -> RPFoo?
// CHECK-NEXT:   func accessorInProto() -> Any?
// CHECK-NEXT: }
// CHECK-NEXT: class RPBase : RPProto {
// CHECK-NEXT:   var accessorInProto: Any? { get }
// CHECK-NEXT:   class func accessorInProto() -> Any?
// CHECK-NEXT:   func accessorInProto() -> Any?
// CHECK-NEXT: }

// CHECK-SUB-NEXT: import RedeclaredPropertiesSub
// CHECK-SUB-EMPTY:

// CHECK-TWO-NEXT: import RedeclaredPropertiesSplit
// CHECK-TWO-EMPTY:

// CHECK-NEXT: extension RPFoo {
// CHECK-NEXT: }
// CHECK-NEXT: class RPSub : RPBase {
// CHECK-NEXT:   var accessorInProto: Any?
// CHECK-NEXT: }
