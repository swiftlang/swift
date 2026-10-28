// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -O -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop %target-embedded-posix-shim) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded

var deinits = 0

struct S<T>: ~Copyable {
  var p: UnsafeMutablePointer<Int>
  init() { p = .allocate(capacity: 1); p.pointee = 7 }
  mutating func bump() { p.pointee += 1 }
  deinit {
    deinits += 1
    p.deallocate()
  }
}

struct Outer: ~Copyable {
  var inner: S<Int>
}

enum E: ~Copyable {
  case a(S<Int>)
  case b
  mutating func touch() {}
}

var escape: (() -> ())?

@main
struct Main {
  static func main() {
    do {
      var s = S<Int>()
      s.bump()
      escape = { s.bump() }
      escape!()
      escape = nil
    }
    print("after struct: \(deinits)")
    // CHECK: after struct: 1

    do {
      var o = Outer(inner: S<Int>())
      o.inner.bump()
      escape = { o.inner.bump() }
      escape!()
      escape = nil
    }
    print("after nested: \(deinits)")
    // CHECK: after nested: 2

    do {
      var e = E.a(S<Int>())
      e.touch()
      escape = { e.touch() }
      escape!()
      escape = nil
    }
    print("after enum: \(deinits)")
    // CHECK: after enum: 3
  }
}
