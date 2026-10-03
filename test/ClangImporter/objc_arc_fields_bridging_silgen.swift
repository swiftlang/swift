// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil -enable-experimental-feature ImportCStructsWithArcFields %s | %FileCheck %s
// REQUIRES: objc_interop
// REQUIRES: swift_feature_ImportCStructsWithArcFields

import Foundation
import objc_structs

// The getter should bridge NSString → String via _unconditionallyBridgeFromObjectiveC.
// CHECK-LABEL: sil shared{{.*}} @$sSo17StrongNSStringArcV4nameSSvg
// CHECK: struct_extract %0, #StrongNSStringArc._name
// CHECK: function_ref @$sSS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ
// CHECK: return
func testBridgedGetter(_ s: StrongNSStringArc) -> String {
  return s.name
}

// The init should bridge String → NSString via _bridgeToObjectiveC before struct construction.
// CHECK-LABEL: sil shared [transparent]{{.*}} @$sSo17StrongNSStringArcV4name3tagABSS_s5Int32VtcfC
// CHECK: function_ref @$sSS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
// CHECK: struct $StrongNSStringArc
// CHECK: return
func testBridgedInit() -> StrongNSStringArc {
  return StrongNSStringArc(name: "hello", tag: 42)
}

// The setter should bridge String → NSString via _bridgeToObjectiveC before storing.
// CHECK-LABEL: sil shared{{.*}} @$sSo17StrongNSStringArcV4nameSSvs
// CHECK: function_ref @$sSS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
// CHECK: struct_element_addr %{{[0-9]+}}, #StrongNSStringArc._name
// CHECK: return
func testBridgedSetter() {
  var s = StrongNSStringArc(name: "hello", tag: 1)
  s.name = "world"
  _ = s
}
