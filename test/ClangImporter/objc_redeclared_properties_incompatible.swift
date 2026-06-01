// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -typecheck -F %S/Inputs/frameworks -verify -verify-additional-prefix public- %s

// RUN: echo '#import <PrivatelyReadwrite/Private.h>' > %t.h
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -F %S/Inputs/frameworks -enable-objc-interop -import-objc-header %t.h -verify -verify-additional-prefix private- %s

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-pch -F %S/Inputs/frameworks -o %t.pch %t.h
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -F %S/Inputs/frameworks -enable-objc-interop -import-objc-header %t.pch -verify -verify-additional-prefix private- %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -F %S/Inputs/frameworks -enable-objc-interop -import-objc-header %t.h -pch-output-dir %t/pch -verify -verify-additional-prefix private- %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -enable-objc-interop -print-module -module-to-print PrivatelyReadwrite -F %S/Inputs/frameworks -source-filename %s | %FileCheck %s --check-prefix=CHECK-PUBLIC
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -enable-objc-interop -source-filename %s -print-header -header-to-print %S/Inputs/frameworks/PrivatelyReadwrite.framework/PrivateHeaders/Private.h --cc-args %target-cc-options -F %S/Inputs/frameworks -fsyntax-only %t.h | %FileCheck %s --check-prefix=CHECK-PRIVATE

// REQUIRES: objc_interop
// UNSUPPORTED: OS=windows-msvc

import PrivatelyReadwrite

// In the original bug, it made a difference whether the type was instantiated;
// it resulted in members being imported in a different order.
func testWithInitializer() {
  let obj = PropertiesInit()

  let _: Int = obj.nullabilityChange
  // expected-error @-1 {{cannot convert value of type 'Base' to specified type 'Int'}}

  let _: Int = obj.missingGenerics
  // expected-error @-1 {{cannot convert value of type 'GenericClass<Base>' to specified type 'Int'}}

  let _: Int = obj.typeChange
  // expected-error @-1 {{cannot convert value of type 'Base' to specified type 'Int'}}

  obj.readwriteChange = Base()
  // expected-public-error @-1 {{cannot assign to property: 'readwriteChange' is a get-only property}}
}

func testWithoutInitializer(obj: PropertiesNoInit) {
  let _: Int = obj.nullabilityChange
  // expected-error @-1 {{cannot convert value of type 'Base' to specified type 'Int'}}

  let _: Int = obj.missingGenerics
  // expected-error @-1 {{cannot convert value of type 'GenericClass<Base>' to specified type 'Int'}}

  let _: Int = obj.typeChange
  // expected-error @-1 {{cannot convert value of type 'Base' to specified type 'Int'}}

  obj.readwriteChange = Base()
  // expected-public-error @-1 {{cannot assign to property: 'readwriteChange' is a get-only property}}
}

func testGenericWithInitializer() {
  let obj = PropertiesInitGeneric<Base>()

  let _: Int = obj.nullabilityChange
  // expected-error @-1 {{cannot convert value of type 'Base' to specified type 'Int'}}

  let _: Int = obj.missingGenerics
  // expected-error @-1 {{cannot convert value of type 'GenericClass<Base>' to specified type 'Int'}}

  let _: Int = obj.typeChange
  // expected-error @-1 {{cannot convert value of type 'Base' to specified type 'Int'}}

  obj.readwriteChange = Base()
  // expected-public-error @-1 {{cannot assign to property: 'readwriteChange' is a get-only property}}
}

func testGenericWithoutInitializer(obj: PropertiesNoInitGeneric<Base>) {
  let _: Int = obj.nullabilityChange
  // expected-error @-1 {{cannot convert value of type 'Base' to specified type 'Int'}}

  let _: Int = obj.missingGenerics
  // expected-error @-1 {{cannot convert value of type 'GenericClass<Base>' to specified type 'Int'}}

  let _: Int = obj.typeChange
  // expected-error @-1 {{cannot convert value of type 'Base' to specified type 'Int'}}

  obj.readwriteChange = Base()
  // expected-public-error @-1 {{cannot assign to property: 'readwriteChange' is a get-only property}}
}

func testCategoryWithInitializer() {
  let obj = PropertiesInitCategory()

  let _: Int = obj.nullabilityChange
  // expected-public-error @-1 {{cannot convert value of type 'Base' to specified type 'Int'}}
  // expected-private-error @-2 {{cannot convert value of type 'Base?' to specified type 'Int'}}

  let _: Int = obj.missingGenerics
  // expected-public-error @-1 {{cannot convert value of type 'GenericClass<Base>' to specified type 'Int'}}
  // expected-private-error @-2 {{cannot convert value of type 'GenericClass<AnyObject>' to specified type 'Int'}}

  let _: Int = obj.typeChange
  // expected-public-error @-1 {{cannot convert value of type 'Base' to specified type 'Int'}}
  // expected-private-error @-2 {{cannot convert value of type 'PrivateSubclass' to specified type 'Int'}}

  obj.readwriteChange = Base()
  // expected-public-error @-1 {{cannot assign to property: 'readwriteChange' is a get-only property}}
}

func testCategoryWithoutInitializer(obj: PropertiesNoInitCategory) {
  let _: Int = obj.nullabilityChange
  // expected-public-error @-1 {{cannot convert value of type 'Base' to specified type 'Int'}}
  // expected-private-error @-2 {{cannot convert value of type 'Base?' to specified type 'Int'}}

  let _: Int = obj.missingGenerics
  // expected-public-error @-1 {{cannot convert value of type 'GenericClass<Base>' to specified type 'Int'}}
  // expected-private-error @-2 {{cannot convert value of type 'GenericClass<AnyObject>' to specified type 'Int'}}

  let _: Int = obj.typeChange
  // expected-public-error @-1 {{cannot convert value of type 'Base' to specified type 'Int'}}
  // expected-private-error @-2 {{cannot convert value of type 'PrivateSubclass' to specified type 'Int'}}

  obj.readwriteChange = Base()
  // expected-public-error @-1 {{cannot assign to property: 'readwriteChange' is a get-only property}}
}

// CHECK-PUBLIC:      class Base {
// CHECK-PUBLIC-NEXT:   init()
// CHECK-PUBLIC-NEXT: }
// CHECK-PUBLIC-NEXT: class GenericClass<T> : Base where T : AnyObject {
// CHECK-PUBLIC-NEXT:   init()
// CHECK-PUBLIC-NEXT: }
// CHECK-PUBLIC-NEXT: class PropertiesInit : Base {
// CHECK-PUBLIC-NEXT:   var readwriteChange: Base { get }
// CHECK-PUBLIC-NEXT:   var nullabilityChange: Base { get }
// CHECK-PUBLIC-NEXT:   var missingGenerics: GenericClass<Base> { get }
// CHECK-PUBLIC-NEXT:   var typeChange: Base { get }
// CHECK-PUBLIC-NEXT:   init()
// CHECK-PUBLIC-NEXT: }
// CHECK-PUBLIC-NEXT: class PropertiesNoInit : Base {
// CHECK-PUBLIC-NEXT:   var readwriteChange: Base { get }
// CHECK-PUBLIC-NEXT:   var nullabilityChange: Base { get }
// CHECK-PUBLIC-NEXT:   var missingGenerics: GenericClass<Base> { get }
// CHECK-PUBLIC-NEXT:   var typeChange: Base { get }
// CHECK-PUBLIC-NEXT:   init()
// CHECK-PUBLIC-NEXT: }
// CHECK-PUBLIC-NEXT: class PropertiesInitGeneric<T> : Base where T : AnyObject {
// CHECK-PUBLIC-NEXT:   var readwriteChange: Base { get }
// CHECK-PUBLIC-NEXT:   var nullabilityChange: Base { get }
// CHECK-PUBLIC-NEXT:   var missingGenerics: GenericClass<Base> { get }
// CHECK-PUBLIC-NEXT:   var typeChange: Base { get }
// CHECK-PUBLIC-NEXT:   init()
// CHECK-PUBLIC-NEXT: }
// CHECK-PUBLIC-NEXT: class PropertiesNoInitGeneric<T> : Base where T : AnyObject {
// CHECK-PUBLIC-NEXT:   var readwriteChange: Base { get }
// CHECK-PUBLIC-NEXT:   var nullabilityChange: Base { get }
// CHECK-PUBLIC-NEXT:   var missingGenerics: GenericClass<Base> { get }
// CHECK-PUBLIC-NEXT:   var typeChange: Base { get }
// CHECK-PUBLIC-NEXT:   init()
// CHECK-PUBLIC-NEXT: }
// CHECK-PUBLIC-NEXT: class PropertiesInitCategory : Base {
// CHECK-PUBLIC-NEXT:   init()
// CHECK-PUBLIC-NEXT: }
// CHECK-PUBLIC-NEXT: extension PropertiesInitCategory {
// CHECK-PUBLIC-NEXT:   var readwriteChange: Base { get }
// CHECK-PUBLIC-NEXT:   var nullabilityChange: Base { get }
// CHECK-PUBLIC-NEXT:   var missingGenerics: GenericClass<Base> { get }
// CHECK-PUBLIC-NEXT:   var typeChange: Base { get }
// CHECK-PUBLIC-NEXT: }
// CHECK-PUBLIC-NEXT: class PropertiesNoInitCategory : Base {
// CHECK-PUBLIC-NEXT:   init()
// CHECK-PUBLIC-NEXT: }
// CHECK-PUBLIC-NEXT: extension PropertiesNoInitCategory {
// CHECK-PUBLIC-NEXT:   var readwriteChange: Base { get }
// CHECK-PUBLIC-NEXT:   var nullabilityChange: Base { get }
// CHECK-PUBLIC-NEXT:   var missingGenerics: GenericClass<Base> { get }
// CHECK-PUBLIC-NEXT:   var typeChange: Base { get }
// CHECK-PUBLIC-NEXT: }

// CHECK-PRIVATE:      import PrivatelyReadwrite
// CHECK-PRIVATE-NEXT: class PrivateSubclass : Base {
// CHECK-PRIVATE-NEXT:   init()
// CHECK-PRIVATE-NEXT: }
// CHECK-PRIVATE-NEXT: extension PropertiesInit {
// CHECK-PRIVATE-NEXT: }
// CHECK-PRIVATE-NEXT: extension PropertiesNoInit {
// CHECK-PRIVATE-NEXT: }
// CHECK-PRIVATE-NEXT: extension PropertiesInitGeneric {
// CHECK-PRIVATE-NEXT: }
// CHECK-PRIVATE-NEXT: extension PropertiesNoInitGeneric {
// CHECK-PRIVATE-NEXT: }
// CHECK-PRIVATE-NEXT: extension PropertiesInitCategory {
// CHECK-PRIVATE-NEXT:   var readwriteChange: Base
// CHECK-PRIVATE-NEXT:   var nullabilityChange: Base?
// CHECK-PRIVATE-NEXT:   var missingGenerics: GenericClass<AnyObject>
// CHECK-PRIVATE-NEXT:   var typeChange: PrivateSubclass
// CHECK-PRIVATE-NEXT: }
// CHECK-PRIVATE-NEXT: extension PropertiesNoInitCategory {
// CHECK-PRIVATE-NEXT:   var readwriteChange: Base
// CHECK-PRIVATE-NEXT:   var nullabilityChange: Base?
// CHECK-PRIVATE-NEXT:   var missingGenerics: GenericClass<AnyObject>
// CHECK-PRIVATE-NEXT:   var typeChange: PrivateSubclass
// CHECK-PRIVATE-NEXT: }
