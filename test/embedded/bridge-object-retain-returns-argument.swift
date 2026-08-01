// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -enable-experimental-feature Embedded -parse-as-library -c -o %t/main.o
// RUN: %target-clang -x c -c %S/Inputs/bridge-object-retain-caller.c -o %t/caller.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o %t/caller.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

// 'swift_bridgeObjectRetain' is declared to return its first argument unchanged
// (RuntimeFunctions.def declares it with 'FirstParamReturned', i.e. LLVM's
// 'returned' attribute, and IRGen stamps that attribute onto every call site it
// emits). The embedded implementation masks the bridge-object tag bits off in
// order to find the plain object to retain, and must not let that masking reach
// the value it returns.
//
// The tag bits are set on every small String, so a regression here hands back a
// word that no longer describes a small String: 'isSmall' is cleared, and the
// result reads as a large String whose storage pointer is the small String's
// count/discriminator payload.

@_silgen_name("call_swift_bridgeObjectRetain")
func call_swift_bridgeObjectRetain(_ object: UInt64) -> UInt64

/// The bridge-object word of a String's underlying _StringObject.
func objectWord(of s: String) -> UInt64 {
  unsafeBitCast(s, to: (UInt64, UInt64).self).1
}

func checkRoundTrip(_ label: StaticString, _ s: String) {
  let before = objectWord(of: s)
  let after = call_swift_bridgeObjectRetain(before)
  print(label, terminator: " ")
  print(before == after ? "unchanged" : "CHANGED")
}

@main
struct Main {
  static func main() {
    // Small strings set 'isSmall' (b61), and ASCII ones also set 'isASCII'
    // (b62); both are masked off to find the plain object.
    // CHECK: smallASCII unchanged
    checkRoundTrip("smallASCII", "hi")
    // CHECK: smallUTF8 unchanged
    checkRoundTrip("smallUTF8", "he\u{301}")
    // CHECK: empty unchanged
    checkRoundTrip("empty", "")

    // Large strings have no tag bits to lose, but check them anyway.
    // CHECK: largeLiteral unchanged
    checkRoundTrip("largeLiteral",
                   "this literal is definitely longer than fifteen bytes")
    var heap = "this literal is definitely longer than fifteen bytes"
    heap += "!"
    // CHECK: nativeHeap unchanged
    checkRoundTrip("nativeHeap", heap)
  }
}
