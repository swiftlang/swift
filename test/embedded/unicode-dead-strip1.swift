// RUN: %target-swift-frontend -Osize -parse-as-library -enable-experimental-feature Embedded %s -c -o %t/a.o
// RUN: %target-clang %t/a.o -o %t/a.out -dead_strip %swift_obj_root/lib/swift/embedded/%target-cpu-apple-macos/libswiftUnicodeDataTables.a
// RUN: %llvm-nm --defined-only --format=just-symbols --demangle %t/a.out | grep swift_stdlib_ | sort | %FileCheck %s --check-prefix=INCLUDES
// RUN: %llvm-nm --defined-only --format=just-symbols --demangle %t/a.out | grep swift_stdlib_ | sort | %FileCheck %s --check-prefix=EXCLUDES

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

@main
struct Main {
  static func main() {
    var dict: [String:String] = [:]
    let c = "Cafe\u{301}"
    let d = "Cafe\u{301}"
    let e = "Café"
    let f = "Caf\u{65}\u{301}"
    let g = "Caf\u{e9}"
    dict[c] = "x"
    dict[d] = "x"
    dict[e] = "x"
    dict[f] = "x"
    dict[g] = "x"
    print(dict.count)
    print(dict[f]!)
  }
}

// The code uses String equality and hashing, should need the normalization, NFC, NFD tables, and not the others.
// EXCLUDES-NOT: swift_stdlib_case
// EXCLUDES-NOT: swift_stdlib_graphemeBreakProperties
// EXCLUDES-NOT: swift_stdlib_mappings
// EXCLUDES-NOT: swift_stdlib_names
// INCLUDES:     swift_stdlib_nfc
// INCLUDES:     swift_stdlib_nfd
// INCLUDES:     swift_stdlib_normData
// EXCLUDES-NOT: swift_stdlib_scripts
// EXCLUDES-NOT: swift_stdlib_special_mappings
// EXCLUDES-NOT: swift_stdlib_words
