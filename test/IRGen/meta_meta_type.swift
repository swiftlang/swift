// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir | %FileCheck -check-prefix=CHECKIR %s
// REQUIRES: executable_test

protocol Proto {
}

struct Mystruct : Proto {
}

// CHECKIR-LABEL: define hidden {{.*}} @_T005meta_A5_type6testitAA5Proto_pXpXpAaC_pF
// CHECKIR: [[M1:%[0-9]+]] = call {{.*}} @swift_getDynamicType
// CHECKIR: [[M2:%[0-9]+]] = call {{.*}} @swift_getMetatypeMetadata(%swift.type* [[M1]])
// CHECKIR: [[R1:%[0-9]+]] = insertvalue {{.*}} [[M2]]
// CHECKIR: [[R2:%[0-9]+]] = insertvalue {{.*}} [[R1]]
// CHECKIR: ret { %swift.type*, i8** } [[R2]]
func testit(_ p: Proto) -> Proto.Type.Type {
  return type(of: type(of: p))
}

// CHECKIR-LABEL: define hidden {{.*}} @_T005meta_A5_type7testit2AA5Proto_pXpXpXpAaC_pF
// CHECKIR: [[M1:%[0-9]+]] = call {{.*}} @swift_getDynamicType
// CHECKIR: [[M2:%[0-9]+]] = call {{.*}} @swift_getMetatypeMetadata(%swift.type* [[M1]])
// CHECKIR: [[M3:%[0-9]+]] = call {{.*}} @swift_getMetatypeMetadata(%swift.type* [[M2]])
// CHECKIR: [[R1:%[0-9]+]] = insertvalue {{.*}} [[M3]]
// CHECKIR: [[R2:%[0-9]+]] = insertvalue {{.*}} [[R1]]
// CHECKIR: ret { %swift.type*, i8** } [[R2]]
func testit2(_ p: Proto) -> Proto.Type.Type.Type {
  return type(of: type(of: type(of: p)))
}

var tt = testit(Mystruct())
var tt2 = testit2(Mystruct())

// CHECK: a.Mystruct.Type
debugPrint(tt)

// CHECK: a.Mystruct.Type.Type
debugPrint(tt2)
