// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t-scratch)
// RUN: %target-swift-frontend -emit-module -o %t-scratch/def_property_wrappers~partial.swiftmodule -primary-file %S/Inputs/def_property_wrappers.swift -module-name def_property_wrappers -enable-testing
// RUN: %target-swift-frontend -merge-modules -emit-module -parse-as-library -sil-merge-partial-modules -disable-diagnostic-passes -disable-sil-perf-optzns -enable-testing %t-scratch/def_property_wrappers~partial.swiftmodule -module-name def_property_wrappers -o %t/def_property_wrappers.swiftmodule
// RUN: %target-swift-frontend -typecheck -I%t -verify %s -verify-ignore-unknown

@testable import def_property_wrappers

// SR-10844
func testSR10844() {
  let holder = Holder()
  holder.b = 100
}

func useWrappers(hd: HasWrappers) {
  // Access the original properties
  let _: Int = hd.x

  let _: SomeWrapper<Int> = hd._x // expected-error{{'_x' is inaccessible due to 'private' protection level}}

  var mutableHD = hd
  mutableHD.x = 17

  // Access the projected properties
  let _: OtherWrapper<Int> = hd.$x

  mutableHD._x = SomeWrapper(wrappedValue: 42) // expected-error{{'_x' is inaccessible due to 'private' protection level}}
}
