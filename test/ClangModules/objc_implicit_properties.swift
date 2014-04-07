// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -enable-objc-implicit-properties %s -verify

import Foundation

func implicitProperties(hive: Hive, bee: B) {
  hive.implicitProperty = bee // no-warning
  hive.implicitProperty.description() // no-warning

  hive.anotherImplicitProperty = 42 // no-warning
  println(Int(hive.anotherImplicitProperty)) // no-warning
}

func badImplicitProperties(obj: BadImplicitProperties) {
  println(obj.nonVoidReturn) // expected-error {{expression does not type-check}}
  println(obj.nonMatchingType) // expected-error {{expression does not type-check}}
  println(obj.wrongGetterArgs) // expected-error {{expression does not type-check}}
  println(obj.wrongSetterArgs) // expected-error {{expression does not type-check}}
  println(obj.wrongSetterArgs2) // expected-error {{expression does not type-check}}
  println(obj.getterOnly) // expected-error {{expression does not type-check}}
  println(obj.setterOnly) // expected-error {{'BadImplicitProperties' does not have a member named 'setterOnly'}}

  // But we should still import all of the methods as methods.
  let x: CInt = obj.setNonVoidReturn(obj.nonVoidReturn())
  let y: CInt = obj.nonMatchingType()
  obj.setNonMatchingType(obj)
  obj.setWrongGetterArgs(obj.wrongGetterArgs(42))
  obj.setWrongSetterArgs(obj.wrongSetterArgs(), extra: 42)
  let z: CInt = obj.wrongSetterArgs2()
  obj.setWrongSetterArgs2()
  obj.setSetterOnly(obj.getterOnly())
}
