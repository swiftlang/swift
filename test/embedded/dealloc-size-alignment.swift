// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -Onone -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o
// RUN: %target-clang -x c -std=c11 -c %S/Inputs/dealloc-size-check.c -o %t/dealloc-size-check.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.o %t/dealloc-size-check.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// Test that weakly referenced objects still pass the correct size and alignment to swift_slowDealloc. The path those values take through the Embedded runtime is dicey, so we want to verify that it works.
//
// This test depends on interposing our own implementation of swift_slowAlloc and swift_slowDealloc implemented in dealloc-size-check.c. This only works when optimizations are off, otherwise the EmbeddedRuntime.swift implementations can get inlined.

// REQUIRES: executable_test
// Weak references need the 64-bit weak refcount field. On 32-bit
// swift_deallocClassInstance frees the object itself with the size it was handed,
// so there is no trip for the values to survive; check what a 32-bit target
// prints before widening this.
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

final class Small {
  var a: Int = 0
  deinit { print("deinit Small") }
}

final class Large {
  var a: Int = 0
  var b: Int = 0
  var c: Int = 0
  var d: Int = 0
  var e: Int = 0
  deinit { print("deinit Large") }
}

final class WeakHolder {
  weak var ref: Small?
  init(_ r: Small?) { self.ref = r }
}

final class LargeWeakHolder {
  weak var ref: Large?
  init(_ r: Large?) { self.ref = r }
}

@inline(never) func noWeakRefs() {
  print("noWeakRefs")
  // CHECK-LABEL: noWeakRefs
  var t: Small? = Small()
  _ = t
  // CHECK: alloc
  t = nil
  // CHECK: deinit Small
  // CHECK: dealloc MATCH
  print("noWeakRefs done")
  // CHECK: noWeakRefs done
}

@inline(never) func noWeakRefsLarge() {
  print("noWeakRefsLarge")
  // CHECK-LABEL: noWeakRefsLarge
  var t: Large? = Large()
  _ = t
  // CHECK: alloc
  t = nil
  // CHECK: deinit Large
  // CHECK: dealloc MATCH
  print("noWeakRefsLarge done")
  // CHECK: noWeakRefsLarge done
}

@inline(never) func weakOutlivesObject() {
  print("weakOutlives")
  // CHECK-LABEL: weakOutlives
  var t: Small? = Small()
  // CHECK: alloc
  let h = WeakHolder(t)
  // CHECK: alloc
  t = nil
  // CHECK: deinit Small
  print("strongGone")
  // CHECK: strongGone
  h.ref = nil
  // CHECK: dealloc MATCH
  print("weakOutlives done")
  // CHECK: weakOutlives done
}

@inline(never) func weakOutlivesObjectLarge() {
  print("weakOutlivesLarge")
  // CHECK-LABEL: weakOutlivesLarge
  var t: Large? = Large()
  // CHECK: alloc
  let h = LargeWeakHolder(t)
  // CHECK: alloc
  t = nil
  // CHECK: deinit Large
  print("strongGone")
  // CHECK: strongGone
  h.ref = nil
  // CHECK: dealloc MATCH
  print("weakOutlivesLarge done")
  // CHECK: weakOutlivesLarge done
}

@main
struct Main {
  static func main() {
    noWeakRefs()
    noWeakRefsLarge()
    weakOutlivesObject()
    weakOutlivesObjectLarge()
    print("end")
    // CHECK: end
  }
}
