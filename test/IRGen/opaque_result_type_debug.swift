// RUN: %target-swift-frontend -emit-ir -enable-anonymous-context-mangled-names %s | %FileCheck %s

protocol P {}
extension Int: P {} 

// CHECK: [[DEFINER_NAME:@.*]] = {{.*}}constant [{{[0-9]+}} x i8] c"$s24opaque_result_type_debug3fooQryF\00"
// CHECK: @"$s24opaque_result_type_debug3fooQryFMXX" = {{.*}}constant{{.*}} [[DEFINER_NAME]]
// CHECK: @"$s24opaque_result_type_debug3fooQryFQOMQ" = {{.*}}constant{{.*}} @"$s24opaque_result_type_debug3fooQryFMXX"

// CHECK: @"\01l_type_metadata_table" = {{.*}} @"$s24opaque_result_type_debug3fooQryFQOMQ"

func foo() -> some P {
  return 0
}
