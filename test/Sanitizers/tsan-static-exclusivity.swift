// RUN: %target-swift-frontend -enforce-exclusivity=checked -sanitize=thread -emit-sil -primary-file %s -o /dev/null -verify
// REQUIRES: tsan_runtime

// FIXME: This should be covered by "tsan_runtime"; older versions of Apple OSs
// don't support TSan.
// UNSUPPORTED: remote_run


struct OtherStruct {
  mutating
  func mutableTakingClosure(_ c: () -> Void) { }
}

struct StructAndInt {
    var s = OtherStruct()
    var f = 42
}

func testStoredPropertyRelaxationInClosure() {
    var l = StructAndInt()
    l.s.mutableTakingClosure {
        _ = l.f // no-diagnostic
    }
}

func takesInouts(_ p1: inout Int, _ p2: inout OtherStruct) { }

func testStoredPropertyRelaxationInInout() {
    var l = StructAndInt()
    takesInouts(&l.f, &l.s) // no-diagnostic
}

