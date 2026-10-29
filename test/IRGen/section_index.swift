// RUN: %target-swift-frontend -parse-as-library -emit-ir %s -o - | %FileCheck %s --check-prefix=CHECK-%target-os-binfmt

// UNSUPPORTED: OS_BINFMT=unknown

#if objectFormat(MachO)
@section("__DATA_CONST,__myplugins") @used var a: Int = 1
@section("__DATA_CONST,__myplugins") @used var b: Int = 2
@section("__DATA_CONST,__others") @used var c: Int = 3
#elseif objectFormat(COFF)
@section(".myplugs$B") @used var a: Int = 1
@section(".myplugs$B") @used var b: Int = 2
@section(".others$B") @used var c: Int = 3
#else
@section("myplugins") @used var a: Int = 1
@section("myplugins") @used var b: Int = 2
@section("others") @used var c: Int = 3
#endif

// ELF and Wasm: one linkonce_odr entry per distinct section, in its own COMDAT
// group, made of three self-relative offsets to the name and to the
// linker-defined encapsulation symbols. A zero-length retained anchor in the
// section itself keeps the output section, and so the symbols, defined even
// when every record in it is dead-stripped.
// CHECK-ELF-DAG: @__start_myplugins = external hidden constant i8
// CHECK-ELF-DAG: @__stop_myplugins = external hidden constant i8
// CHECK-ELF-DAG: @__swift5_sections_anchor_myplugins = linkonce_odr hidden constant [0 x i8] zeroinitializer, section "myplugins", no_sanitize_address, comdat, align 1
// CHECK-ELF-DAG: @__swift5_sections_entry_myplugins = linkonce_odr hidden constant {{.*}}sub ({{.*}}ptrtoint (ptr @__start_myplugins to {{.*}}sub ({{.*}}ptrtoint (ptr @__stop_myplugins to {{.*}}, section "swift5_sections", no_sanitize_address, comdat, align 4
// CHECK-ELF-DAG: @__swift5_sections_anchor_others = linkonce_odr hidden constant [0 x i8] zeroinitializer, section "others", no_sanitize_address, comdat, align 1
// CHECK-ELF-DAG: @__swift5_sections_entry_others = linkonce_odr hidden constant {{.*}}sub ({{.*}}ptrtoint (ptr @__start_others to {{.*}}sub ({{.*}}ptrtoint (ptr @__stop_others to {{.*}}, section "swift5_sections", no_sanitize_address, comdat, align 4
// CHECK-ELF-DAG: @llvm.used = appending global {{.*}}@__swift5_sections_anchor_myplugins{{.*}}@__swift5_sections_entry_myplugins

// Mach-O: section bounds are in the image header, so no index is emitted.
// CHECK-MACH-O-NOT: swift5_sections
// CHECK-MACH-O: section "__DATA_CONST,__myplugins"
// CHECK-MACH-O-NOT: swift5_sections

// COFF: the index names the section by the part before the "$". The start
// marker is 16 bytes in the "$A" group with its size in the first word; the
// stop marker is one zero word in the "$Z" group; the entry's offsets point at
// the two markers.
// CHECK-PE-COFF-DAG: @__swift5_sections_start_.myplugs = linkonce_odr constant { {{i64|i32}}, [{{8|12}} x i8] } { {{i64|i32}} 16, [{{8|12}} x i8] zeroinitializer }, section ".myplugs$A", no_sanitize_address, comdat, align 16
// CHECK-PE-COFF-DAG: @__swift5_sections_stop_.myplugs = linkonce_odr constant {{i64|i32}} 0, section ".myplugs$Z", no_sanitize_address, comdat
// CHECK-PE-COFF-DAG: @__swift5_sections_entry_.myplugs = linkonce_odr constant {{.*}}sub ({{.*}}ptrtoint (ptr @__swift5_sections_start_.myplugs to {{.*}}sub ({{.*}}ptrtoint (ptr @__swift5_sections_stop_.myplugs to {{.*}}, section ".sw5sect$B", no_sanitize_address, comdat, align 4
// CHECK-PE-COFF-DAG: @__swift5_sections_entry_.others = linkonce_odr constant {{.*}}sub ({{.*}}ptrtoint (ptr @__swift5_sections_start_.others to {{.*}}sub ({{.*}}ptrtoint (ptr @__swift5_sections_stop_.others to {{.*}}, section ".sw5sect$B", no_sanitize_address, comdat, align 4
