// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -enable-large-loadable-types %s -emit-ir  -parse-as-library | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize

public struct BigStruct {
  var i0 : Int32 = 0
  var i1 : Int32 = 1
  var i2 : Int32 = 2
  var i3 : Int32 = 3
  var i4 : Int32 = 4
  var i5 : Int32 = 5
  var i6 : Int32 = 6
  var i7 : Int32 = 7
  var i8 : Int32 = 8
}

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} linkonce_odr hidden %swift.opaque* @"$S33big_types_corner_cases_as_library9BigStructVwCP"
// CHECK: [[RETVAL:%.*]] = bitcast %T33big_types_corner_cases_as_library9BigStructV* {{.*}} to %swift.opaque*
// CHECK: ret %swift.opaque* [[RETVAL]]
let bigStructGlobalArray : [BigStruct] = [
  BigStruct()
]
