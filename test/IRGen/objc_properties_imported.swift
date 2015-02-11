// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-source-import -emit-ir -o - -primary-file %s | FileCheck %s

// REQUIRES: CPU=x86_64
// REQUIRES: objc_interop

// FIXME: This test uses IRGen with -enable-source-import; it may fail with -g.

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

// CHECK-LABEL: define hidden void @_TF24objc_properties_imported16testBoolProperty
func testBoolProperty(hp: HasProperties) {
  // CHECK-NOT: ret void
  // CHECK: load i8** @"\01L_selector(isEnabled)"
  // CHECK-NOT: ret void
  // CHECK: load i8** @"\01L_selector(setIsEnabled:)"
  hp.enabled = !hp.enabled
  // CHECK: ret void
}
