// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -typecheck -F %S/Inputs/frameworks -verify -verify-ignore-unrelated -verify-additional-prefix public- %s

// RUN: echo '#import <CategoryOverrides/Private.h>' > %t.h
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -F %S/Inputs/frameworks -enable-objc-interop -import-objc-header %t.h -verify -verify-ignore-unrelated -verify-additional-prefix private- %s

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-pch -F %S/Inputs/frameworks -o %t.pch %t.h
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -F %S/Inputs/frameworks -enable-objc-interop -import-objc-header %t.pch -verify -verify-ignore-unrelated -verify-additional-prefix private- %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -F %S/Inputs/frameworks -enable-objc-interop -import-objc-header %t.h -pch-output-dir %t/pch -verify -verify-ignore-unrelated -verify-additional-prefix private- %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -enable-objc-interop -print-module -module-to-print CategoryOverrides -F %S/Inputs/frameworks -source-filename %s | %FileCheck %s --check-prefix=CHECK-PUBLIC
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -enable-objc-interop -source-filename %s -print-header -header-to-print %S/Inputs/frameworks/CategoryOverrides.framework/PrivateHeaders/Private.h --cc-args %target-cc-options -F %S/Inputs/frameworks -fsyntax-only %t.h | %FileCheck %s --check-prefix=CHECK-PRIVATE

// REQUIRES: objc_interop

import CategoryOverrides

// Nail down some emergent behaviors of the Clang Importer's override checking:

// A category declared in a (private) header can happen to double-import a property
// and a function with the same name - both before and after omit-needless-words -
// as long as they have different contextual types.
//
// This configuration appears as an undiagnosed redeclaration of a property and
// function, which is illegal.
func colors() {
  let _ : MyColor = MyColor.systemRed
  let _ : MyColor = MyColor.systemRed()!
  // expected-public-error@-1 {{cannot call value of non-function type 'MyColor?'}}
}

// Another manifestation of the above for an instance property this time.
func structs(_ base: MyBaseClass, _ derived: MyDerivedClass) {
  let _ : SomeStruct = base.myStructure
  let _ : SomeStruct = base.myStructure()
  // expected-public-error@-1 {{cannot call value of non-function type 'SomeStruct'}}

  let _ : SomeStruct = derived.myStructure
  let _ : SomeStruct = derived.myStructure()
  // expected-public-error@-1 {{cannot call value of non-function type 'SomeStruct'}}
}

// A category declared in a (private) header can introduce overrides of a property
// that is otherwise not declared in a base class.
//
// This configuration appears as an undiagnosed override in a Swift extension,
// which is illegal.
func takesADerivedClass(_ x: MyDerivedClass) {
  x.derivedMember = Base()
}

func takesABaseClass(_ x: MyBaseClass) {
  x.derivedMember = Base()
  // expected-public-error@-1 {{has no member 'derivedMember'}}
}

// A category declared in a (private) header can introduce overrides of a
// property that has mismatched Swift naming conventions. If we see a
// non-__attribute__((swift_private)) decl, sometimes it comes in too.

extension Refinery {
  public enum RefinedSugar {
    case caster
    case grantulated
    case confectioners
    case cane
    case demerara
    case turbinado
  }

  public var sugar: Refinery.RefinedSugar {
    return .caster // RefinedSugar(self.__sugar)
  }
}

func takesARefinery(_ x: Refinery) {
  x.sugar = .caster
  // expected-error@-1 {{cannot assign to property: 'sugar' is a get-only property}}
}

func takesAnExtraRefinery(_ x: ExtraRefinery) {
  x.sugar = .caster
  // expected-error@-1 {{has no member 'sugar'}}
  // expected-error@-2 {{cannot infer contextual base in reference to member 'caster'}}
  x.setSugar(0)
  // expected-public-error@-1 {{has no member 'setSugar'}}
  // expected-private-error@-2 {{cannot convert value of type 'Int' to expected argument type '__RefinedSugar'}}
}

func nullabilityRefinementProto(_ x: MyBaseClass) {
  let _ : Base = x.requirement
  // expected-public-error@-1 {{has no member 'requirement'}}
}

func readwriteRefinementProto(_ x: MyDerivedClass) {
  if x.answer == 0 {
    // expected-public-error@-1 {{has no member 'answer'}}
    x.answer = 42
    // expected-public-error@-1 {{has no member 'answer'}}
  }
}

// CHECK-PUBLIC:      class Base {
// CHECK-PUBLIC-NEXT:   init()
// CHECK-PUBLIC-NEXT: }
// CHECK-PUBLIC-NEXT: struct SomeStruct_s {
// CHECK-PUBLIC-NEXT:   init()
// CHECK-PUBLIC-NEXT:   init(inner: CInt)
// CHECK-PUBLIC-NEXT:   var inner: CInt
// CHECK-PUBLIC-NEXT: }
// CHECK-PUBLIC-NEXT: typealias SomeStruct = SomeStruct_s
// CHECK-PUBLIC-NEXT: class MyColor : Base {
// CHECK-PUBLIC-NEXT:   class var systemRed: MyColor! { get }
// CHECK-PUBLIC-NEXT:   @available(swift, obsoleted: 3, renamed: "systemRed")
// CHECK-PUBLIC-NEXT:   class var systemRedColor: MyColor! { get }
// CHECK-PUBLIC-NEXT:   init()
// CHECK-PUBLIC-NEXT: }
// CHECK-PUBLIC-NEXT: class MyBaseClass : Base {
// CHECK-PUBLIC-NEXT:   var myStructure: SomeStruct { get }
// CHECK-PUBLIC-NEXT:   init()
// CHECK-PUBLIC-NEXT: }
// CHECK-PUBLIC-NEXT: class MyDerivedClass : MyBaseClass {
// CHECK-PUBLIC-NEXT:   var derivedMember: Base?
// CHECK-PUBLIC-NEXT:   init()
// CHECK-PUBLIC-NEXT: }
// CHECK-PUBLIC-NEXT: struct __RefinedSugar : Hashable, Equatable, RawRepresentable {
// CHECK-PUBLIC-NEXT:   init(_ rawValue: [[ENUM_RAW_VALUE_TYPE:.*]])
// CHECK-PUBLIC-NEXT:   init(rawValue: [[ENUM_RAW_VALUE_TYPE]])
// CHECK-PUBLIC-NEXT:   var rawValue: [[ENUM_RAW_VALUE_TYPE]]
// CHECK-PUBLIC-NEXT:   typealias RawValue = [[ENUM_RAW_VALUE_TYPE]]
// CHECK-PUBLIC-NEXT: }
// CHECK-PUBLIC-NEXT: var __Caster: __RefinedSugar { get }
// CHECK-PUBLIC-NEXT: var __Grantulated: __RefinedSugar { get }
// CHECK-PUBLIC-NEXT: var __Confectioners: __RefinedSugar { get }
// CHECK-PUBLIC-NEXT: var __Cane: __RefinedSugar { get }
// CHECK-PUBLIC-NEXT: var __Demerara: __RefinedSugar { get }
// CHECK-PUBLIC-NEXT: var __Turbinado: __RefinedSugar { get }
// CHECK-PUBLIC-NEXT: class Refinery : Base {
// CHECK-PUBLIC-NEXT:   var __sugar: __RefinedSugar { get }
// CHECK-PUBLIC-NEXT:   init()
// CHECK-PUBLIC-NEXT: }
// CHECK-PUBLIC-NEXT: class ExtraRefinery : Base {
// CHECK-PUBLIC-NEXT:   var __sugar: __RefinedSugar { get }
// CHECK-PUBLIC-NEXT:   init()
// CHECK-PUBLIC-NEXT: }
// CHECK-PUBLIC-NEXT: protocol NullableProtocol {
// CHECK-PUBLIC-NEXT:   var requirement: Base? { get }
// CHECK-PUBLIC-NEXT: }
// CHECK-PUBLIC-NEXT: protocol NonNullProtocol : NullableProtocol {
// CHECK-PUBLIC-NEXT:   var requirement: Base { get }
// CHECK-PUBLIC-NEXT: }
// CHECK-PUBLIC-NEXT: protocol ReadonlyProtocol {
// CHECK-PUBLIC-NEXT:   var answer: CInt { get }
// CHECK-PUBLIC-NEXT: }
// CHECK-PUBLIC-NEXT: protocol ReadwriteProtocol : ReadonlyProtocol {
// CHECK-PUBLIC-NEXT:   var answer: CInt { get set }
// CHECK-PUBLIC-NEXT: }

// CHECK-PRIVATE:      import CategoryOverrides
// CHECK-PRIVATE-NEXT: extension MyBaseClass {
// CHECK-PRIVATE-NEXT:   var derivedMember: Base?
// CHECK-PRIVATE-NEXT: }
// CHECK-PRIVATE-NEXT: extension MyColor {
// CHECK-PRIVATE-NEXT:   class func systemRed() -> MyColor!
// CHECK-PRIVATE-NEXT:   @available(swift, obsoleted: 3, renamed: "systemRed()")
// CHECK-PRIVATE-NEXT:   class func systemRedColor() -> MyColor!
// CHECK-PRIVATE-NEXT: }
// CHECK-PRIVATE-NEXT: protocol MyPrivateProtocol {
// CHECK-PRIVATE-NEXT:   func myStructure() -> SomeStruct
// CHECK-PRIVATE-NEXT: }
// CHECK-PRIVATE-NEXT: extension MyBaseClass : MyPrivateProtocol {
// CHECK-PRIVATE-NEXT:   func myStructure() -> SomeStruct
// CHECK-PRIVATE-NEXT: }
// CHECK-PRIVATE-NEXT: extension Refinery {
// CHECK-PRIVATE-NEXT: }
// CHECK-PRIVATE-NEXT: extension ExtraRefinery {
// CHECK-PRIVATE-NEXT:   func setSugar(_ sugar: __RefinedSugar)
// CHECK-PRIVATE-NEXT: }
// CHECK-PRIVATE-NEXT: extension MyBaseClass : NonNullProtocol {
// CHECK-PRIVATE-NEXT:   var requirement: Base { get }
// CHECK-PRIVATE-NEXT: }
// CHECK-PRIVATE-NEXT: extension MyDerivedClass : ReadwriteProtocol {
// CHECK-PRIVATE-NEXT:   var answer: CInt
// CHECK-PRIVATE-NEXT: }
