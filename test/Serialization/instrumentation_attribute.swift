// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/Lib.swiftmodule -module-name Lib \
// RUN:   -parse-as-library %S/Inputs/instrumentation_attribute_lib.swift
// RUN: %target-swift-frontend -emit-sil -O -I %t -module-name main -sanitize=address %s | %FileCheck %s --check-prefix=OPT


import Lib

// The attribute round-trips through the swiftmodule, as does the flag on the
// serialized SIL of the @inlinable function, so the optimizer still refuses
// to inline it into this module's instrumented code.
// OPT-LABEL: sil {{.*}}@$s4main6callerSiyF
// OPT: function_ref @$s3Lib11inlinableFnSiyF
public func caller() -> Int {
  return inlinableFn()
}
