// A tuple has no deinit of its own, so destroying one has to be decomposed into
// destroys of its elements. Without that, IRGen sees a destroy of a tuple whose
// element is a generic non-copyable type and reaches for the unspecialized
// generic deinit, which is illegal in Embedded Swift.

// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -enable-experimental-feature MoveOnlyTuples -O -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop %target-embedded-posix-shim) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_MoveOnlyTuples

var deinits = 0

struct Storage<T>: ~Copyable {
  var p: UnsafeMutablePointer<Int>
  init(_ value: Int) {
    p = .allocate(capacity: 1)
    p.pointee = value
  }
  deinit {
    deinits += 1
    p.deallocate()
  }
}

@main
struct Main {
  static func main() {
    do {
      let t = (Storage<Int>(7), 1)
      _ = t.1
    }
    print("after one element: \(deinits)")
    // CHECK: after one element: 1

    do {
      let t = (Storage<Int>(1), Storage<Int>(2))
      _ = t
    }
    print("after two elements: \(deinits)")
    // CHECK: after two elements: 3
  }
}
