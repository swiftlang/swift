// RUN: %target-swift-emit-silgen %s -import-objc-header %S/Inputs/keypaths_import_as_member.h | %FileCheck %s

// CHECK-LABEL: sil {{.*}} @$s{{.*}}23keyPathToImportedMember
func keyPathToImportedMember() {
  // CHECK: keypath $KeyPath<Butt, Int32>, (root $Butt; gettable_property $Int32,  id @ButtSize
  _ = \Butt.size
}
