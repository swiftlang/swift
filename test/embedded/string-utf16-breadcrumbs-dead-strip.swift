// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -Osize -parse-as-library -enable-experimental-feature Embedded %s -c -o %t/a.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.o %target-embedded-posix-shim -o %t/a.out -dead_strip %swift_obj_root/lib/swift/embedded/%module-target-triple/libswiftUnicodeDataTables.a
// RUN: %llvm-nm --defined-only --format=just-symbols --demangle %t/a.out | %FileCheck %s --check-prefix=INCLUDES
// RUN: %llvm-nm --defined-only --format=just-symbols --demangle %t/a.out | %FileCheck %s --check-prefix=EXCLUDES

// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

// Embedded Swift has no Objective-C runtime, so a `String` can never be
// foreign (UTF-16 / `NSString`-backed). The foreign `String.UTF8View`
// accessors and the UTF-16 `_StringBreadcrumbs` machinery they reach are
// therefore dead and must not survive `-dead_strip`. `_StringGuts.isFastUTF8`
// folds to `true` (and `isForeign` to `false`) when `!_runtime(_ObjC)`, and
// `_slowEnsureMatchingEncoding` -- which every String view index operation
// reaches via `ensureMatchingEncoding`, and which is the path that
// incidentally pulled the native UTF-16 breadcrumb machinery into a program
// that only uses `.utf8` -- is short-circuited there as well. (A program that
// genuinely uses `String.UTF16View` indexing still links that machinery, as it
// should.)

@inline(never)
func makeString(_ bytes: [UInt8]) -> String {
  String(decoding: bytes, as: UTF8.self)
}

// A pure UTF-8 byte compare over `String.UTF8View`, sourced from runtime bytes
// so it cannot be constant-folded away -- the kind of name-lookup comparison an
// embedded program does over string keys.
@inline(never)
func utf8Equal(_ a: String, _ b: String) -> Bool {
  a.utf8.count == b.utf8.count && a.utf8.elementsEqual(b.utf8)
}

@main
struct Main {
  static func main() {
    let a = makeString([0x72, 0x75, 0x6e])  // "run"
    let b = makeString([0x72, 0x75, 0x6e])  // "run"
    print(utf8Equal(a, b) ? 1 : 0)
  }
}

// Positive anchor: the linked executable really has symbols, so the EXCLUDES-NOT
// checks below are meaningful rather than vacuously passing on empty `nm` output.
// (`nm --format=just-symbols` yields nothing for a wasm object, which is why this
// test is restricted to macOS above.)
// INCLUDES: main

// None of the foreign-String / UTF-16 / breadcrumb machinery may be linked.
// EXCLUDES-NOT: getUTF16Count
// EXCLUDES-NOT: createAndLoadBreadcrumbs
// EXCLUDES-NOT: getBreadcrumb
// EXCLUDES-NOT: _foreignCount
// EXCLUDES-NOT: _foreignIndex
// EXCLUDES-NOT: _foreignDistance
// EXCLUDES-NOT: _nativeGetOffset
// EXCLUDES-NOT: _nativeGetIndex
// EXCLUDES-NOT: _utf16Distance
