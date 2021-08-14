// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/weak_import_opaque_result_type_helper.swiftmodule -parse-as-library %S/Inputs/weak_import_opaque_result_type_helper.swift -enable-library-evolution -disable-availability-checking
// RUN: %target-swift-frontend -disable-type-layout -disable-availability-checking -primary-file %s -I %t -emit-ir | %FileCheck %s

// UNSUPPORTED: OS=windows-msvc

import weak_import_opaque_result_type_helper

func useWeakImportedOpaqueResultType<T : P>(_ p: T) {
  p.someAPI().blah()
}

// CHECK-LABEL: @"$s37weak_import_opaque_result_type_helper1PPAAE7someAPIQryFQOMQ" = extern_weak global %swift.type_descriptor
// CHECK-LABEL: declare extern_weak {{.+}} void @"$s37weak_import_opaque_result_type_helper1PPAAE7someAPIQryF"
