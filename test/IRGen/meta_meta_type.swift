// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s
// RUN: %target-swift-frontend -primary-file %s -emit-ir | FileCheck -check-prefix=CHECKIR %s

protocol Proto {
}

struct Mystruct : Proto {
}

// CHECKIR-LABEL: define hidden {{.*}} @_TF14meta_meta_type6testitFPS_5Proto_PMPMPS0__
// CHECKIR: [[M1:%[0-9]+]] = call {{.*}} @swift_getDynamicType
// CHECKIR: [[M2:%[0-9]+]] = call {{.*}} @swift_getMetatypeMetadata(%swift.type* [[M1]])
// CHECKIR: [[R1:%[0-9]+]] = insertvalue {{.*}} [[M2]]
// CHECKIR: [[R2:%[0-9]+]] = insertvalue {{.*}} [[R1]]
// CHECKIR: ret { %swift.type*, i8** } [[R2]]
func testit(p: Proto) -> Proto.Type.Type {
  return p.dynamicType.dynamicType
}

// CHECKIR-LABEL: define hidden {{.*}} @_TF14meta_meta_type7testit2FPS_5Proto_PMPMPMPS0__
// CHECKIR: [[M1:%[0-9]+]] = call {{.*}} @swift_getDynamicType
// CHECKIR: [[M2:%[0-9]+]] = call {{.*}} @swift_getMetatypeMetadata(%swift.type* [[M1]])
// CHECKIR: [[M3:%[0-9]+]] = call {{.*}} @swift_getMetatypeMetadata(%swift.type* [[M2]])
// CHECKIR: [[R1:%[0-9]+]] = insertvalue {{.*}} [[M3]]
// CHECKIR: [[R2:%[0-9]+]] = insertvalue {{.*}} [[R1]]
// CHECKIR: ret { %swift.type*, i8** } [[R2]]
func testit2(p: Proto) -> Proto.Type.Type.Type {
  return p.dynamicType.dynamicType.dynamicType
}

var tt = testit(Mystruct())
var tt2 = testit2(Mystruct())

// CHECK: a.Mystruct.Type
println(tt)

// CHECK: a.Mystruct.Type.Type
println(tt2)
