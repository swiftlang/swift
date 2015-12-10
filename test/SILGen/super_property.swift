// RUN: %target-swift-frontend -use-native-super-method -emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck %s

class Parent {
  final var finalProperty: String {
    return "Parent.finalProperty"
  }
  var property: String {
    return "Parent.property"
  }
}

class Child : Parent {
  // CHECK-LABEL: sil hidden @_TFC14super_property5Childg8propertySS
  // CHECK:         [[CASTED_SELF:%[0-9]+]] = upcast %0 : $Child to $Parent
  // CHECK:         [[SUPER_METHOD:%[0-9]+]] = super_method %0 : $Child, #Parent.property!getter.1
  override var property: String {
    return super.property
  }

  // CHECK-LABEL: sil hidden @_TFC14super_property5Childg13otherPropertySS
  // CHECK:         [[CASTED_SELF:%[0-9]+]] = upcast %0 : $Child to $Parent
  // CHECK:         [[SUPER_METHOD:%[0-9]+]] = function_ref @_TFC14super_property6Parentg13finalPropertySS
  var otherProperty: String {
    return super.finalProperty
  }
}
