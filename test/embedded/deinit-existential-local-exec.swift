// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -O -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop %target-embedded-posix-shim) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded

var deinits = 0

protocol P: ~Copyable {
  func f()
}

struct S<T>: ~Copyable, P {
  var p: UnsafeMutablePointer<Int>
  init(_ v: Int) { p = .allocate(capacity: 1); p.pointee = v }
  func f() { precondition(p.pointee == 7) }
  deinit {
    deinits += 1
    p.deallocate()
  }
}

@main
struct Main {
  static func main() {
    do {
      let e: any P & ~Copyable = S<Int>(7)
      e.f()
    }
    print("after one: \(deinits)")
    // CHECK: after one: 1

    do {
      let e: any P & ~Copyable = S<Int>(7)
      e.f()
      e.f()
    }
    print("after two: \(deinits)")
    // CHECK: after two: 2
  }
}
