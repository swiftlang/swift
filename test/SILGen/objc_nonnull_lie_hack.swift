// RUN: %target-swift-emit-silgen -sdk %S/Inputs -I %S/Inputs -I %S/Inputs/objc_nonnull_lie_hack/ -enable-source-import -primary-file -enable-sil-ownership %s | %FileCheck -check-prefix=SILGEN %s
// RUN: %target-swiftemit-sil -O -sdk %S/Inputs -I %S/Inputs -I %S/Inputs/objc_nonnull_lie_hack/ -enable-source-import -primary-file %s | %FileCheck -check-prefix=OPT %s

// REQUIRES: objc_interop
// REQUIRES: rdar33495516

import Foundation
import NonNilTest

func checkThatAPINotesAreBeingUsed() {
  let hopefullyNotOptional = NonNilTest.nonNilObject()
  let _: NonNilTest = hopefullyNotOptional
}

// SILGEN-LABEL: sil hidden @$s21objc_nonnull_lie_hack10makeObjectSo8NSObjectCSgyF
// SILGEN:         [[INIT:%.*]] = function_ref @$sSo8NSObjectCABycfC
// SILGEN:         [[NONOPTIONAL:%.*]] = apply [[INIT]]
// SILGEN:         [[OPTIONAL:%.*]] = unchecked_ref_cast [[NONOPTIONAL]]

// OPT-LABEL: sil hidden @$s21objc_nonnull_lie_hack10makeObjectSo8NSObjectCSgyF
// OPT:         [[OPT:%.*]] = unchecked_ref_cast
// OPT:         switch_enum [[OPT]] : $Optional<NSObject>, case #Optional.some!enumelt.1:
func makeObject() -> NSObject? {
  let foo: NSObject? = NSObject()
  if foo == nil {
    print("nil")
  }
  return foo
}

// OPT-LABEL: sil hidden @$s21objc_nonnull_lie_hack15callClassMethodSo10NonNilTestCSgyF
// OPT: [[METATYPE:%[0-9]+]] = metatype $@thick NonNilTest.Type
// OPT: [[METHOD:%[0-9]+]] = objc_method [[METATYPE]] : $@thick NonNilTest.Type, #NonNilTest.nonNilObject!1.foreign : (NonNilTest.Type) -> () -> NonNilTest, $@convention(objc_method) (@objc_metatype NonNilTest.Type) -> @autoreleased NonNilTest
// OPT: [[OBJC_METATYPE:%[0-9]+]] = metatype $@objc_metatype NonNilTest.Type
// OPT: [[NONOPTIONAL:%[0-9]+]] = apply [[METHOD]]([[OBJC_METATYPE]]) : $@convention(objc_method) (@objc_metatype NonNilTest.Type) -> @autoreleased NonNilTest
// OPT: [[OPTIONAL:%[0-9]+]] = unchecked_ref_cast [[NONOPTIONAL]] : $NonNilTest to $Optional<NonNilTest>
// OPT: switch_enum [[OPTIONAL]] : $Optional<NonNilTest>
func callClassMethod() -> NonNilTest? {
  let foo: NonNilTest? = NonNilTest.nonNilObject()
  if foo == nil {
    print("nil")
  }
  return foo  
}

// OPT-LABEL: sil shared @$s21objc_nonnull_lie_hack18callInstanceMethodSo10NonNilTestCSgAD3obj_tFTf4g_n
// OPT: [[METHOD:%[0-9]+]] = objc_method [[OBJ:%[0-9]+]] : $NonNilTest, #NonNilTest.nonNilObject!1.foreign : (NonNilTest) -> () -> NonNilTest, $@convention(objc_method) (NonNilTest) -> @autoreleased NonNilTest
// OPT: [[NONOPTIONAL:%[0-9]+]] = apply [[METHOD]]([[OBJ]]) : $@convention(objc_method) (NonNilTest) -> @autoreleased NonNilTest
// OPT: [[OPTIONAL:%[0-9]+]] = unchecked_ref_cast [[NONOPTIONAL]]
// OPT: switch_enum [[OPTIONAL]] : $Optional<NonNilTest>
func callInstanceMethod(obj: NonNilTest) -> NonNilTest? {
  let foo: NonNilTest? = obj.nonNilObject()

  if foo == nil {
    print("nil")
  }
  return foo
}

// OPT-LABEL: sil shared @$s21objc_nonnull_lie_hack12loadPropertySo10NonNilTestCSgAD3obj_tFTf4g_n
// OPT: [[GETTER:%[0-9]+]] = objc_method [[OBJ:%[0-9]+]] : $NonNilTest, #NonNilTest.nonNilObjectProperty!getter.1.foreign : (NonNilTest) -> () -> NonNilTest, $@convention(objc_method) (NonNilTest) -> @autoreleased NonNilTest
// OPT: [[NONOPTIONAL:%[0-9]+]] = apply [[GETTER]]([[OBJ]]) : $@convention(objc_method) (NonNilTest) -> @autoreleased NonNilTest
// OPT: [[OPTIONAL:%[0-9]+]] = unchecked_ref_cast [[NONOPTIONAL]] : $NonNilTest to $Optional<NonNilTest>
// OPT: switch_enum [[OPTIONAL]] : $Optional<NonNilTest>,
func loadProperty(obj: NonNilTest) -> NonNilTest? {
  let foo: NonNilTest? = obj.nonNilObjectProperty
  if foo == nil {
    print("nil")
  }
  return foo  
}

// OPT-LABEL: sil shared @$s21objc_nonnull_lie_hack19loadUnownedPropertySo10NonNilTestCSgAD3obj_tFTf4g_n
// OPT: [[GETTER:%[0-9]+]] = objc_method [[OBJ:%[0-9]+]] : $NonNilTest, #NonNilTest.unownedNonNilObjectProperty!getter.1.foreign : (NonNilTest) -> () -> NonNilTest, $@convention(objc_method) (NonNilTest) -> @autoreleased NonNilTest
// OPT: [[NONOPTIONAL:%[0-9]+]] = apply [[GETTER]]([[OBJ]]) : $@convention(objc_method) (NonNilTest) -> @autoreleased NonNilTest
// OPT: [[OPTIONAL:%[0-9]+]] = unchecked_ref_cast [[NONOPTIONAL]] : $NonNilTest to $Optional<NonNilTest>
// OPT: switch_enum [[OPTIONAL]] : $Optional<NonNilTest>
func loadUnownedProperty(obj: NonNilTest) -> NonNilTest? {
  let foo: NonNilTest? = obj.unownedNonNilObjectProperty
  if foo == nil {
    print("nil")
  }
  return foo  
}
