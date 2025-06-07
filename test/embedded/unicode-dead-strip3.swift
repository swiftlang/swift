// RUN: %target-swift-frontend -Osize -parse-as-library -enable-experimental-feature Embedded %s -c -o %t/a.o
// RUN: %target-clang %t/a.o -o %t/a.out -dead_strip %swift_obj_root/lib/swift/embedded/%target-cpu-apple-macos/libswiftUnicodeDataTables.a
// RUN: %llvm-nm --defined-only --format=just-symbols --demangle %t/a.out | grep swift_stdlib_ | sort | %FileCheck %s --check-prefix=INCLUDES
// RUN: %llvm-nm --defined-only --format=just-symbols --demangle %t/a.out | grep swift_stdlib_ | sort | %FileCheck %s --check-prefix=EXCLUDES

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

@main
struct Main {
  static func main() {
    let e = "Café\nMacs"
    for i in e.split(separator: "\n") {
      print(i)
    }
  }
}

// The code uses String splitting, should need the normalization, NFC, NFD, grapheme breaking, linking tables, and not the others.
// EXCLUDES-NOT: swift_stdlib_case
// INCLUDES:     swift_stdlib_graphemeBreakProperties
// EXCLUDES-NOT: swift_stdlib_mappings
// EXCLUDES-NOT: swift_stdlib_names
// INCLUDES:     swift_stdlib_nfc
// INCLUDES:     swift_stdlib_nfd
// INCLUDES:     swift_stdlib_normData
// EXCLUDES-NOT: swift_stdlib_scripts
// EXCLUDES-NOT: swift_stdlib_special_mappings
// EXCLUDES-NOT: swift_stdlib_words
