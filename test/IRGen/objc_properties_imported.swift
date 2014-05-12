// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -enable-source-import -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -emit-ir -o - %s | FileCheck %s

import Properties

// CHECK: @_INSTANCE_METHODS__TtC24objc_properties_imported21OverridesBoolProperty{{.*}}_selector_data(isEnabled){{.*}}L_selector_data(setIsEnabled:)
class OverridesBoolProperty : HasProperties {
  override var enabled : Bool {
    get {
      return super.enabled
    }
    set {
      super.enabled = newValue
    }
  }
}

// CHECK-LABEL: define void @_TF24objc_properties_imported16testBoolProperty
func testBoolProperty(hp: HasProperties) {
  // CHECK-NOT: ret void
  // CHECK: load i8** @"\01L_selector(isEnabled)"
  // CHECK-NOT: ret void
  // CHECK: load i8** @"\01L_selector(setIsEnabled:)"
  hp.enabled = !hp.enabled
  // CHECK: ret void
}
