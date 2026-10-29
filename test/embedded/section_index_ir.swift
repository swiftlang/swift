// RUN: %target-swift-frontend -emit-ir -parse-as-library -enable-experimental-feature Embedded -wmo %s -o - | %FileCheck %s
// REQUIRES: swift_feature_Embedded

// Embedded Swift places variables for linker scripts and bare-metal images,
// which have no runtime to read an index; no index, anchor or bounds are
// emitted.

#if objectFormat(MachO)
@section("__DATA_CONST,__myplugins") @used var a: Int = 1
#elseif objectFormat(COFF)
@section(".myplugs$B") @used var a: Int = 1
#else
@section("myplugins") @used var a: Int = 1
#endif

// CHECK-NOT: swift5_sections
// CHECK-NOT: sw5sect
// CHECK-NOT: __swift5_sections_
// CHECK-NOT: __start_
// CHECK: @{{.*}}a{{.*}} = {{.*}} section "{{.*}}myplug
// CHECK-NOT: swift5_sections
// CHECK-NOT: sw5sect
// CHECK-NOT: __swift5_sections_
// CHECK-NOT: __start_
