// RUN: %target-swift-frontend -module-name main -I %t -emit-ir -primary-file %s %S/Inputs/ABIInaccessible.swift | %FileCheck %s

public struct AnotherType<T> {
  init(_ t: T) {
    p = Public<T>(t)
  }
  public var p : Public<T>
}

// Don't pass the metadata of Private<T> to AnotherType<T>'s outlined destroy.
// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$S4main4copyyAA11AnotherTypeVyxGAElF"(%T4main11AnotherTypeV* noalias nocapture sret, %T4main11AnotherTypeV* noalias nocapture, %swift.type* %T)
// CHECK:  [[MD:%.*]] = call swiftcc %swift.metadata_response @"$S4main11AnotherTypeVMa"(i{{.*}} 0, %swift.type* %T)
// CHECK:  [[MD1:%.*]] = extractvalue %swift.metadata_response [[MD]], 0
// CHECK:  [[MD2:%.*]] = call swiftcc %swift.metadata_response @"$S4main6PublicVMa"(i{{.*}} 0, %swift.type* %T)
// CHECK:  [[MD3:%.*]] = extractvalue %swift.metadata_response [[MD2]], 0
// CHECK:  call %T4main11AnotherTypeV* @"$S4main11AnotherTypeVyxGlWOc"(%T4main11AnotherTypeV* %1, %T4main11AnotherTypeV* {{.*}}, %swift.type* %T, %swift.type* [[MD3]], %swift.type* [[MD1]])
public func copy<T>(_ a: AnotherType<T>) -> AnotherType<T> {
  let copy = a
  return copy
}
