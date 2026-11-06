// RUN: %target-run-simple-swift(-enable-experimental-feature ObjCDirect) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature ObjCDirect) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect

import Foundation

protocol Counting {
  func count() -> Int
  var total: Int { get set }
}

// Direct members not written 'final', witnessing a Swift protocol.
class Tally: NSObject, Counting {
  var stored = 0

  @objcDirect var total: Int {
    get { stored }
    set { stored = newValue }
  }

  @objcDirect func count() -> Int {
    stored += 1
    return stored
  }
}

class SubTally: Tally {
  var extra: Int
  init(extra: Int) {
    self.extra = extra
    super.init()
  }
}

// A direct designated initializer requires a final class.
final class Counter: NSObject {
  var v: Int

  @objcDirect init(value: Int) {
    v = value
    super.init()
  }

  override convenience init() { self.init(value: 7) }
}

final class Tagged: NSObject {
  var tag: String
  var extra = 5
  init(tag: String) {
    self.tag = tag
    super.init()
  }
  @objcDirect convenience init(twice t: String) { self.init(tag: t + t) }
}

let c = Counter()
// CHECK: v=7
print("v=\(c.v)")

var w: Counting = Tally()
w.total = 10
// CHECK-NEXT: total=10 count=11
print("total=\(w.total) count=\(w.count())")

let s = SubTally(extra: 9)
// CHECK-NEXT: sub extra=9 count=1
print("sub extra=\(s.extra) count=\(s.count())")

var ws: Counting = s
ws.total = 4
// CHECK-NEXT: sub total=4 count=5
print("sub total=\(ws.total) count=\(ws.count())")

let t = Tagged(twice: "x")
// CHECK-NEXT: Tagged tag=xx extra=5
print("\(type(of: t)) tag=\(t.tag) extra=\(t.extra)")

// Direct members are invisible to the Objective-C runtime.
// CHECK-NEXT: false false false false
print(s.responds(to: NSSelectorFromString("count")),
      s.responds(to: NSSelectorFromString("total")),
      c.responds(to: NSSelectorFromString("initWithValue:")),
      t.responds(to: NSSelectorFromString("initWithTwice:")))

// A direct property's ivar has no type encoding, so KVC cannot write it behind
// the property's back; it raises instead.
class Ivars: NSObject {
  @objcDirect var direct: Int = 1
  @objcDirect var directObject: NSObject = NSObject()
  @objc var plain: Int = 2
}

func encoding(_ name: String) -> String {
  guard let ivar = class_getInstanceVariable(Ivars.self, name) else {
    return "<no ivar>"
  }
  guard let enc = ivar_getTypeEncoding(ivar) else { return "<null>" }
  return String(cString: enc)
}

// CHECK-NEXT: direct='' directObject='' plain='q'
print("direct='\(encoding("direct"))' directObject='\(encoding("directObject"))' plain='\(encoding("plain"))'")
