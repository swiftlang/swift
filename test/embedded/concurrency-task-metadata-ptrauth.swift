// Check that the embedded concurrency runtime signs the metadata pointer of the
// heap objects it creates itself (AsyncTask) the same way Embedded Swift code
// authenticates it on release: DA key, address-diversified, discriminator
// 0x6ae1 (see EmbeddedHeapObject in EmbeddedShims.h). Without that, releasing
// the last reference to a completed task (task group children, fire-and-forget
// tasks, a dropped Task handle) fails pointer authentication on arm64e
//
// The runtime used to get this only by way of __ptrauth_objc_isa_pointer, i.e.
// only when the clang building it happened to enable ObjC isa signing for the
// target. This test checks the built archive, not a compiler default

// RUN: llvm-objdump -dr --no-show-raw-insn --disassemble-symbols=_swift_task_create_common %swift_obj_root/lib/swift/embedded/arm64e-apple-none-macho/libswift_Concurrency.a | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: CODEGENERATOR=AArch64
// REQUIRES: optimized_stdlib
// REQUIRES: embedded_stdlib_cross_compiling

// CHECK-LABEL: <_swift_task_create_common>:
// CHECK:       ARM64_RELOC_PAGEOFF12 __ZN5swift19taskHeapMetadataPtrE
// CHECK:       movk {{x[0-9]+}}, #0x6ae1, lsl #48
// CHECK-NEXT:  pacda
