// RUN: %target-swift-frontend -Osize -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature Extern %s -c -o %t/a.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.o %target-embedded-posix-shim -o %t/a.out -dead_strip %swift_obj_root/lib/swift/embedded/%module-target-triple/libswiftUnicodeDataTables.a
// RUN: %llvm-nm --defined-only --format=just-symbols --demangle %t/a.out | sort | %FileCheck %s --check-prefix=EXCLUDES

// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern

@main
struct Main {
  static func main() {
    let t = "true"
    let f = "false"
    let unrelated = "nope"
    let b1 = Bool(t)
    let b2 = Bool(f)
    let b3 = Bool(unrelated)

    print(b1 ?? false)
    print(b2 ?? false)
    print(b3 ?? false)

    let bi: StaticBigInt = 17
    print(bi.debugDescription)
  }
}

// The code avoides using any of the Unicode tables
// EXCLUDES-NOT: swift_stdlib_case
// EXCLUDES-NOT: swift_stdlib_graphemeBreakProperties
// EXCLUDES-NOT: swift_stdlib_mappings
// EXCLUDES-NOT: swift_stdlib_names
// EXCLUDES-NOT: swift_stdlib_nfc
// EXCLUDES-NOT: swift_stdlib_nfd
// EXCLUDES-NOT: swift_stdlib_normData
// EXCLUDES-NOT: swift_stdlib_scripts
// EXCLUDES-NOT: swift_stdlib_special_mappings
// EXCLUDES-NOT: swift_stdlib_words
