// RUN: %target-typecheck-verify-swift

// XFAIL: linux

// FIXME: Should go into the standard library.
public extension _ObjectiveCBridgeable {
  static func _unconditionallyBridgeFromObjectiveC(_ source: _ObjectiveCType?)
      -> Self {
    var result: Self?
    _forceBridgeFromObjectiveC(source!, result: &result)
    return result!
  }
}

class V {}
class U : V {}
class T : U {}

var v = V()
var u = U()
var t = T()

var va = [v]
var ua = [u]
var ta = [t]

va = ta

var va2: ([V])? = va as [V]
var v2: V = va2![0]

var ua2: ([U])? = va as? [U]
var u2: U = ua2![0]

var ta2: ([T])? = va as? [T]
var t2: T = ta2![0]

// Check downcasts that require bridging.
class A {
  var x = 0
}

struct B : _ObjectiveCBridgeable {
  func _bridgeToObjectiveC() -> A {
    return A()
  }
  static func _forceBridgeFromObjectiveC(
    _ x: A,
    result: inout B?
  ) {
  }
  static func _conditionallyBridgeFromObjectiveC(
    _ x: A,
    result: inout B?
  ) -> Bool {
    return true
  }
}

func testBridgedDowncastAnyObject(_ arr: [AnyObject], arrOpt: [AnyObject]?, 
                                  arrIUO: [AnyObject]!) {
  var b = B()

  if let bArr = arr as? [B] {
    b = bArr[0]
  }

  if let bArr = arrOpt as? [B] {
    b = bArr[0]
  }

  if let bArr = arrIUO as? [B] {
    b = bArr[0]
  }
  _ = b
}

func testBridgedIsAnyObject(_ arr: [AnyObject], arrOpt: [AnyObject]?, 
                             arrIUO: [AnyObject]!) -> Bool {
  let b = B()

  if arr is [B] { return arr is [B] }
  if arrOpt is [B] { return arrOpt is [B] }
  if arrIUO is [B] { return arrIUO is [B] }
  _ = b

  return false
}

