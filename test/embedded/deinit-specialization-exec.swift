// Check that a specialized deinit created for the destroy value witness of a
// non-copyable type still runs the right deinits at runtime.

// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -O -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop %target-embedded-posix-shim) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded

var storageDeinits = 0

struct Storage<T>: ~Copyable {
  var p: UnsafeMutablePointer<Int>
  init(_ value: Int) {
    p = .allocate(capacity: 1)
    p.pointee = value
  }
  deinit {
    storageDeinits += 1
    p.deallocate()
  }
}

@export(interface)
struct Box: ~Copyable {
  var items: Storage<Int>
  init(_ value: Int) { items = Storage<Int>(value) }
}

@export(interface)
enum MaybeBox: ~Copyable {
  case none
  case some(Storage<Int>)
}

@main
struct Main {
  static func main() {
    do {
      let b = Box(42)
      print("value: \(b.items.p.pointee)")
      // CHECK: value: 42
    }
    print("after box: \(storageDeinits)")
    // CHECK: after box: 1

    do {
      let e = MaybeBox.some(Storage<Int>(17))
      if case .some(let s) = e {
        print("payload: \(s.p.pointee)")
        // CHECK: payload: 17
      }
    }
    print("after enum: \(storageDeinits)")
    // CHECK: after enum: 2

    do {
      let e = MaybeBox.none
      if case .none = e { print("empty case") }
      // CHECK: empty case
    }
    print("after empty: \(storageDeinits)")
    // CHECK: after empty: 2
  }
}
