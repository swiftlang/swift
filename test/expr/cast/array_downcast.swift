// RUN: %target-parse-verify-swift

// XFAIL: linux

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
  static func _isBridgedToObjectiveC() -> Bool {
    return true
  }
  
  static func _getObjectiveCType() -> Any.Type {
    return A.self
  }
  func _bridgeToObjectiveC() -> A {
    return A()
  }
  static func _forceBridgeFromObjectiveC(
    x: A,
    inout result: B?
  ) {
  }
  static func _conditionallyBridgeFromObjectiveC(
    x: A,
    inout result: B?
  ) -> Bool {
    return true
  }
}

func testBridgedDowncastAnyObject(arr: [AnyObject], arrOpt: [AnyObject]?, 
                                  arrIUO: [AnyObject]!) {
  var b = B()

  if let bArr? = arr as? [B] {
    b = bArr[0]
  }

  if let bArr? = arrOpt as? [B] {
    b = bArr[0]
  }

  if let bArr? = arrIUO as? [B] {
    b = bArr[0]
  }
}

func testBridgedIsAnyObject(arr: [AnyObject], arrOpt: [AnyObject]?, 
                             arrIUO: [AnyObject]!) -> Bool {
  var b = B()

  if arr is [B] { return arr is [B] }
  if arrOpt is [B] { return arrOpt is [B] }
  if arrIUO is [B] { return arrIUO is [B] }

  return false
}

