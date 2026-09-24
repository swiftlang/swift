// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern

// RUN: %target-swift-frontend -emit-sil %s -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature Extern -enable-experimental-feature CodeGenerationModel=interface | %FileCheck %s

@_extern(c, "abort")
internal func _abort()

@_silgen_name("_named_internal_helper")
internal func _namedInternalHelper() {
  // Body present.
}

public func use() {
  _abort()
  _namedInternalHelper()
}

// @_extern(c) function has hidden_external linkage
// CHECK-DAG: sil hidden_external {{.*}}[asmname "abort"] @{{.*}}_abortyyFTo

// The @_silgen_name-with-body keeps its access-level-derived linkage.
// CHECK-DAG: sil hidden {{.*}}@_named_internal_helper

// The body-less stub must *not* appear as a non-external definition
// — the body comes from outside Swift.
// CHECK-NOT: sil hidden {{.*}}_abortyyFTo
// CHECK-NOT: sil private{{.*}}_abortyyFTo
