// RUN: %target-swift-emit-sil %s -enable-experimental-feature Embedded -wmo -o /dev/null

// REQUIRES: swift_feature_Embedded

// A coroutine accessor whose body binds dynamic self -- here by referring to
// `Self` from inside a closure, which gives the accessor a dynamic-self metadata
// argument -- must not be force-inlined into a caller that has no such metadata
// to supply.

class G<T> {
  static var meta: Int { 7 }
  var value: Int {
    _read {
      var x = 3
      let r = withUnsafePointer(to: &x) { p -> Int in
        return p.pointee + Self.meta
      }
      yield r
    }
  }
}

var g = G<Int>()
print(g.value)
