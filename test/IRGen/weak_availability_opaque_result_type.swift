// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/weak_availability_opaque_result_type_helper.swiftmodule -parse-as-library %S/Inputs/weak_availability_opaque_result_type_helper.swift -enable-library-evolution
// RUN: %target-swift-frontend -disable-type-layout -primary-file %s -I %t -emit-ir | %FileCheck %s

// REQUIRES: OS=macosx

import weak_availability_opaque_result_type_helper

func useWeakImportedOpaqueResultType<T : P>(_ p: T) {
  if #available(macOS 100, *) {
    p.someAPI().blah()
  }
}

// CHECK-LABEL: @"$s43weak_availability_opaque_result_type_helper1PPAAE7someAPIQryFQOMQ" = extern_weak global %swift.type_descriptor
// CHECK-LABEL: declare extern_weak {{.+}} void @"$s43weak_availability_opaque_result_type_helper1PPAAE7someAPIQryF"
