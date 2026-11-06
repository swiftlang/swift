// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target armv7em-none-none-eabi -Osize -parse-as-library -enable-experimental-feature Embedded %s -c -o %t/a.o
// RUN: %clang -target armv7em-none-none-eabi -fuse-ld=lld -nostdlib -Wl,--gc-sections -Wl,--unresolved-symbols=ignore-all -Wl,-e,main %t/a.o %swift_obj_root/lib/swift/embedded/armv7em-none-none-eabi/libswiftUnicodeDataTables.a -o %t/a.out
// RUN: %llvm-nm --defined-only --format=just-symbols --demangle %t/a.out | grep swift_stdlib_ | sort | %FileCheck %s --check-prefix=INCLUDES
// RUN: %llvm-nm --defined-only --format=just-symbols --demangle %t/a.out | grep swift_stdlib_ | sort | %FileCheck %s --check-prefix=EXCLUDES

// REQUIRES: OS=macosx
// REQUIRES: optimized_stdlib
// REQUIRES: CODEGENERATOR=ARM
// REQUIRES: embedded_stdlib_cross_compiling
// REQUIRES: swift_feature_Embedded
// REQUIRES: ld.lld

@main
struct Main {
  static func main() {
    let e = "Café"
    print(e.uppercased())
  }
}

// The code uses String case conversion, should need the mapping tables, and not the others.
// EXCLUDES-NOT: swift_stdlib_case
// EXCLUDES-NOT: swift_stdlib_graphemeBreakProperties
// INCLUDES:     swift_stdlib_mappings
// EXCLUDES-NOT: swift_stdlib_names
// EXCLUDES-NOT: swift_stdlib_nfc
// EXCLUDES-NOT: swift_stdlib_nfd
// EXCLUDES-NOT: swift_stdlib_normData
// EXCLUDES-NOT: swift_stdlib_scripts
// INCLUDES:     swift_stdlib_special_mappings
// EXCLUDES-NOT: swift_stdlib_words
