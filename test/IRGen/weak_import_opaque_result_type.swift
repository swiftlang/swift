// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/weak_import_opaque_result_type_helper.swiftmodule -parse-as-library %S/Inputs/weak_import_opaque_result_type_helper.swift -enable-library-evolution -target %target-swift-5.1-abi-triple
// RUN: %target-swift-frontend -disable-type-layout -target %target-swift-5.1-abi-triple -primary-file %s -I %t -emit-ir | %FileCheck %s

// UNSUPPORTED: OS=windows-msvc

import weak_import_opaque_result_type_helper

func useWeakImportedOpaqueResultType<T : P>(_ p: T) {
  p.someAPI().blah()
}

// CHECK-LABEL: @"$s37weak_import_opaque_result_type_helper1PPAAE7someAPIQryFQOMQ" = extern_weak global %swift.type_descriptor
// CHECK-LABEL: declare extern_weak {{.+}} void @"$s37weak_import_opaque_result_type_helper1PPAAE7someAPIQryF"
