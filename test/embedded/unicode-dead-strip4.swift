// RUN: %target-swift-frontend -Osize -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature Extern %s -c -o %t/a.o
// RUN: %target-clang %target-clang-resource-dir-opt %t/a.o -o %t/a.out -dead_strip %swift_obj_root/lib/swift/embedded/%module-target-triple/libswiftUnicodeDataTables.a
// RUN: %llvm-nm --defined-only --format=just-symbols --demangle %t/a.out | sort | %FileCheck %s --check-prefix=EXCLUDES

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1
// XFAIL: OS=wasip1
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern

@_extern(c, "getline")
func getline(
  _ linePointer: UnsafeMutablePointer<UnsafeMutablePointer<UInt8>?>?
) -> Int

@c
func swift_stdlib_readLine_stdin(_ linePointer: UnsafeMutablePointer<UnsafeMutablePointer<UInt8>?>?) -> Int {
  return getline(linePointer)
}



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

    if let value = readLine(), let b4 = Bool(value) {
      print(b4)
    }
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
