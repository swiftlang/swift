// An object whose deinit lets a strong reference to `self` outlive the deinit
// must trap when the object is deallocated. deinit-self-no-escape.swift checks
// that legitimate uses of `self` in deinit don't.
//
// Each case escapes into a global so the optimizer cannot delete the store along
// with a dead local, and nothing reads the escaped reference afterwards, since a
// read of dangling memory could crash on its own and pass these tests for the
// wrong reason.

// RUN: %empty-directory(%t)

// A heap object: `self` escapes into a global's stored property.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -module-name test -DHEAP %s -c -o %t/heap.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/heap.o -o %t/heap.out -dead_strip
// RUN: %target-run not --crash %t/heap.out

// `self` escapes by being captured in a closure that outlives the deinit.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -module-name test -DCLOSURE %s -c -o %t/closure.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/closure.o -o %t/closure.out -dead_strip
// RUN: %target-run not --crash %t/closure.out

// A retain of `self` in deinit that is never balanced by a release.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -module-name test -DUNBALANCED %s -c -o %t/unbalanced.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/unbalanced.o -o %t/unbalanced.out -dead_strip
// RUN: %target-run not --crash %t/unbalanced.out

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded

// UNSUPPORTED: OS=wasip1
// UNSUPPORTED: OS=emscripten
// The RUN lines assert a trap via LLVM's `not --crash`, but emscripten-run.py
// passes its arguments to `node`, which then cannot load `not` as a JS module
// (`Error: Cannot find module 'not'`).

#if HEAP
final class Box {
  var strong: Escaper? = nil
}

var escapedBox: Box? = nil

final class Escaper {
  var holder: Box? = nil
  deinit {
    holder?.strong = self
  }
}

@inline(never) func run() {
  let box = Box()
  escapedBox = box
  var e: Escaper? = Escaper()
  e!.holder = box
  e = nil
}
#endif

#if CLOSURE
var escapedClosure: (() -> Int)? = nil

final class Capturer {
  var id: Int = 7
  deinit {
    escapedClosure = { self.id }
  }
}

@inline(never) func run() {
  var c: Capturer? = Capturer()
  c = nil
}
#endif

#if UNBALANCED
final class Unbalanced {
  deinit {
    _ = Unmanaged.passUnretained(self).retain()
  }
}

@inline(never) func run() {
  var u: Unbalanced? = Unbalanced()
  u = nil
}
#endif

@main
struct Main {
  static func main() {
    run()
    print("unreached")
  }
}
