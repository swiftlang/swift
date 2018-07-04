// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -typecheck -I %S/Inputs/custom-modules -D ONE_MODULE %s -verify
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -typecheck -I %S/Inputs/custom-modules -D SUB_MODULE %s -verify
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -typecheck -I %S/Inputs/custom-modules -D TWO_MODULES %s -verify

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

  if let _ = obj.nullableToNonnull {} // okay
  obj.nullableToNonnull = obj // expected-error {{cannot assign to property: 'nullableToNonnull' is a get-only property}}

  let _: RPFoo = obj.typeChangeMoreSpecific // expected-error {{cannot convert value of type 'Any' to specified type 'RPFoo'}}
  obj.typeChangeMoreSpecific = obj // expected-error {{cannot assign to property: 'typeChangeMoreSpecific' is a get-only property}}

  let _: RPFoo = obj.typeChangeMoreGeneral
  obj.typeChangeMoreGeneral = obj // expected-error {{cannot assign to property: 'typeChangeMoreGeneral' is a get-only property}}

  if let _ = obj.accessorRedeclaredAsNullable {} // expected-error {{initializer for conditional binding must have Optional type}}
  if let _ = obj.accessorDeclaredFirstAsNullable {} // expected-error {{initializer for conditional binding must have Optional type}}
}
