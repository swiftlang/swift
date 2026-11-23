// REQUIRES: no_asan
// REQUIRES: CODEGENERATOR=AArch64
// REQUIRES: ld.lld
// UNSUPPORTED: OS=windows-msvc

// Static inspection of ELF images whose dynamic relocations are in Android's
// packed format (lld's --pack-dyn-relocs=android), alone and combined with
// RELR. The images are built from -parse-stdlib modules so that the test
// doesn't need an Android SDK.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %clang -target aarch64-unknown-linux-gnu -ffreestanding -fPIC -c %t/sections.c -o %t/sections.o
// RUN: %swift_frontend_plain -target aarch64-unknown-linux-gnu -parse-stdlib -parse-as-library -disable-objc-interop -c %t/Core.swift -module-name Core -emit-module-path %t/Core.swiftmodule -o %t/Core.o
// RUN: %swift_frontend_plain -target aarch64-unknown-linux-gnu -parse-stdlib -parse-as-library -disable-objc-interop -c %t/User.swift -module-name User -I %t -o %t/User.o

// RUN: %empty-directory(%t/android)
// RUN: %clang -target aarch64-unknown-linux-gnu -fuse-ld=lld -nostdlib -shared -Wl,--pack-dyn-relocs=android %t/Core.o %t/sections.o -o %t/android/libCore.so
// RUN: %clang -target aarch64-unknown-linux-gnu -fuse-ld=lld -nostdlib -shared -Wl,--pack-dyn-relocs=android %t/User.o %t/sections.o %t/android/libCore.so -o %t/android/libUser.so
// RUN: %llvm-readelf --dynamic-table %t/android/libCore.so | %FileCheck %s --check-prefix=ANDROID
// RUN: %llvm-readelf --dynamic-table %t/android/libUser.so | %FileCheck %s --check-prefix=ANDROID
// ANDROID: (ANDROID_RELA)
// RUN: %target-swift-reflection-dump %t/android/libUser.so %t/android/libCore.so -dump-type-lowering < %t/types.txt | %FileCheck %s

// RUN: %empty-directory(%t/relr)
// RUN: %clang -target aarch64-unknown-linux-gnu -fuse-ld=lld -nostdlib -shared -Wl,--pack-dyn-relocs=android+relr %t/Core.o %t/sections.o -o %t/relr/libCore.so
// RUN: %clang -target aarch64-unknown-linux-gnu -fuse-ld=lld -nostdlib -shared -Wl,--pack-dyn-relocs=android+relr %t/User.o %t/sections.o %t/relr/libCore.so -o %t/relr/libUser.so
// RUN: %llvm-readelf --dynamic-table %t/relr/libCore.so | %FileCheck %s --check-prefix=RELR
// RUN: %llvm-readelf --dynamic-table %t/relr/libUser.so | %FileCheck %s --check-prefix=RELR
// RELR-DAG: (ANDROID_RELA)
// RELR-DAG: (RELR)
// RUN: %target-swift-reflection-dump %t/relr/libUser.so %t/relr/libCore.so -dump-type-lowering < %t/types.txt | %FileCheck %s

// CHECK:      (struct User.S)
// CHECK-NEXT: (struct size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-NEXT:   (field name=x offset=0
// CHECK-NEXT:     (struct size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-NEXT:       (field name=a offset=0
// CHECK-NEXT:         (struct size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-NEXT:       (field name=b offset=0
// CHECK-NEXT:         (struct size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-NEXT:   (field name=y offset=0
// CHECK-NEXT:     (struct size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1)))

//--- types.txt
4User1SV

//--- Core.swift
public struct Empty {}
public struct Pair {
  var a: Empty
  var b: Empty
}

//--- User.swift
import Core
public struct S {
  var x: Pair
  var y: Empty
}

//--- sections.c
// The reflection section table that swiftrt.o provides. The layout must match
// SwiftReflectionSections in SwiftRT-ELF.cpp.
#define SECTION(name)                                                          \
  __asm__(".pushsection " #name ",\"a\"\n.popsection\n");                      \
  extern const char __start_##name __attribute__((weak, visibility("hidden"))); \
  extern const char __stop_##name __attribute__((weak, visibility("hidden")));
SECTION(swift5_fieldmd)
SECTION(swift5_assocty)
SECTION(swift5_builtin)
SECTION(swift5_capture)
SECTION(swift5_typeref)
SECTION(swift5_reflstr)
SECTION(swift5_protocol_conformances)

#define BOUNDS(name) {&__start_##name, &__stop_##name}
struct Bounds {
  const void *start, *stop;
};
__attribute__((used, visibility("default"))) const struct {
  unsigned long version;
  struct Bounds sections[7];
} __swift5_reflection_sections = {
    1,
    {BOUNDS(swift5_fieldmd), BOUNDS(swift5_assocty), BOUNDS(swift5_builtin),
     BOUNDS(swift5_capture), BOUNDS(swift5_typeref), BOUNDS(swift5_reflstr),
     BOUNDS(swift5_protocol_conformances)}};
