// RUN: %target-swift-frontend -use-native-super-method -emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck %s

class Parent {
  final class var finalProperty: String {
    return "Parent.finalProperty"
  }
  class var property: String {
    return "Parent.property"
  }
}

class Child : Parent {
  // CHECK-LABEL: sil hidden @_TZFC20super_class_property5Childg8propertySS
  // CHECK:         [[CASTED_SELF:%[0-9]+]] = upcast %0 : $@thick Child.Type to $@thick Parent.Type
  // CHECK:         [[SUPER_METHOD:%[0-9]+]] = super_method %0 : $@thick Child.Type, #Parent.property!getter.1
  override class var property: String {
    return super.property
  }

  // CHECK-LABEL: sil hidden @_TZFC20super_class_property5Childg13otherPropertySS
  // CHECK:         [[CASTED_SELF:%[0-9]+]] = upcast %0 : $@thick Child.Type to $@thick Parent.Type
  // CHECK:         [[SUPER_METHOD:%[0-9]+]] = function_ref @_TZFC20super_class_property6Parentg13finalPropertySS
  class var otherProperty: String {
    return super.finalProperty
  }
}
