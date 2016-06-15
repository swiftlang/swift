// RUN: %target-parse-verify-swift

// REQUIRES: objc_interop

// FIXME: Should go into the standard library.
public extension _ObjectiveCBridgeable {
  static func _unconditionallyBridgeFromObjectiveC(_ source: _ObjectiveCType?)
      -> Self {
    var result: Self? = nil
    _forceBridgeFromObjectiveC(source!, result: &result)
    return result!
  }
}

class Root : Hashable { 
  var hashValue: Int {
    return 0
  }
}

func ==(x: Root, y: Root) -> Bool { return true }

class ObjC : Root {
  var x = 0
}

class DerivesObjC : ObjC { }

struct BridgedToObjC : Hashable, _ObjectiveCBridgeable {
  static func _isBridgedToObjectiveC() -> Bool {
    return true
  }
  
  func _bridgeToObjectiveC() -> ObjC {
    return ObjC()
  }
  static func _forceBridgeFromObjectiveC(
    _ x: ObjC,
    result: inout BridgedToObjC?
  ) {
  }
  static func _conditionallyBridgeFromObjectiveC(
    _ x: ObjC,
    result: inout BridgedToObjC?
  ) -> Bool {
    return true
  }

  var hashValue: Int {
    return 0
  }
}

func ==(x: BridgedToObjC, y: BridgedToObjC) -> Bool { return true }

func testUpcastBridge() {
  var dictRR = Dictionary<Root, Root>()
  var dictRO = Dictionary<Root, ObjC>()
  var dictOR = Dictionary<ObjC, Root>()
  var dictOO = Dictionary<ObjC, ObjC>()
  var dictOD = Dictionary<ObjC, DerivesObjC>()
  var dictDO = Dictionary<DerivesObjC, ObjC>()
  var dictDD = Dictionary<DerivesObjC, DerivesObjC>()

  var dictBB = Dictionary<BridgedToObjC, BridgedToObjC>()
  var dictBO = Dictionary<BridgedToObjC, ObjC>()
  var dictOB = Dictionary<ObjC, BridgedToObjC>()

  // Upcast to object types.
  dictRR = dictBB
  dictRR = dictBO
  dictRR = dictOB

  dictRO = dictBB
  dictRO = dictBO
  dictRO = dictOB

  dictOR = dictBB
  dictOR = dictBO
  dictOR = dictOB

  dictOO = dictBB
  dictOO = dictBO
  dictOO = dictOB

  // Upcast key or value to object type (but not both)
  dictBO = dictBB
  dictOB = dictBB

  dictBB = dictBO // expected-error{{cannot assign value of type 'Dictionary<BridgedToObjC, ObjC>' to type 'Dictionary<BridgedToObjC, BridgedToObjC>'}}
  dictBB = dictOB // expected-error{{cannot assign value of type 'Dictionary<ObjC, BridgedToObjC>' to type 'Dictionary<BridgedToObjC, BridgedToObjC>'}}

  dictDO = dictBB // expected-error{{cannot assign value of type 'Dictionary<BridgedToObjC, BridgedToObjC>' to type 'Dictionary<DerivesObjC, ObjC>'}}
  dictOD = dictBB // expected-error{{cannot assign value of type 'Dictionary<BridgedToObjC, BridgedToObjC>' to type 'Dictionary<ObjC, DerivesObjC>'}}
  dictDD = dictBB // expected-error{{cannot assign value of type 'Dictionary<BridgedToObjC, BridgedToObjC>' to type 'Dictionary<DerivesObjC, DerivesObjC>'}}
  
  _ = dictDD; _ = dictDO; _ = dictOD; _ = dictOO; _ = dictOR; _ = dictOR; _ = dictRR; _ = dictRO
}

func testDowncastBridge() {
  let dictRR = Dictionary<Root, Root>()
  let dictRO = Dictionary<Root, ObjC>()
  _ = Dictionary<ObjC, Root>()
  _ = Dictionary<ObjC, ObjC>()
  _ = Dictionary<ObjC, DerivesObjC>()
  let dictDO = Dictionary<DerivesObjC, ObjC>()
  _ = Dictionary<DerivesObjC, DerivesObjC>()

  _ = Dictionary<BridgedToObjC, BridgedToObjC>()
  let dictBO = Dictionary<BridgedToObjC, ObjC>()
  let dictOB = Dictionary<ObjC, BridgedToObjC>()

  // Downcast to bridged value types.
  _ = dictRR as! Dictionary<BridgedToObjC, BridgedToObjC>
  _ = dictRR as! Dictionary<BridgedToObjC, ObjC>
  _ = dictRR as! Dictionary<ObjC, BridgedToObjC>

  _ = dictRO as! Dictionary<BridgedToObjC, BridgedToObjC>
  _ = dictRO as! Dictionary<BridgedToObjC, ObjC>
  _ = dictRO as! Dictionary<ObjC, BridgedToObjC>

  _ = dictBO as! Dictionary<BridgedToObjC, BridgedToObjC>
  _ = dictOB as! Dictionary<BridgedToObjC, BridgedToObjC>

  // We don't do mixed down/upcasts.
  _ = dictDO as! Dictionary<BridgedToObjC, BridgedToObjC> // expected-error{{'Dictionary<DerivesObjC, ObjC>' is not convertible to 'Dictionary<BridgedToObjC, BridgedToObjC>'}}
}

func testConditionalDowncastBridge() {
  var dictRR = Dictionary<Root, Root>()
  var dictRO = Dictionary<Root, ObjC>()
  var dictOR = Dictionary<ObjC, Root>()
  var dictOO = Dictionary<ObjC, ObjC>()
  var dictOD = Dictionary<ObjC, DerivesObjC>()
  var dictDO = Dictionary<DerivesObjC, ObjC>()
  var dictDD = Dictionary<DerivesObjC, DerivesObjC>()

  var dictBB = Dictionary<BridgedToObjC, BridgedToObjC>()
  var dictBO = Dictionary<BridgedToObjC, ObjC>()
  var dictOB = Dictionary<ObjC, BridgedToObjC>()

  // Downcast to bridged value types.
  if let d = dictRR as? Dictionary<BridgedToObjC, BridgedToObjC> { }
  if let d = dictRR as? Dictionary<BridgedToObjC, ObjC> { }
  if let d = dictRR as? Dictionary<ObjC, BridgedToObjC> { }

  if let d = dictRO as? Dictionary<BridgedToObjC, BridgedToObjC> { }
  if let d = dictRO as? Dictionary<BridgedToObjC, ObjC> { }
  if let d = dictRO as? Dictionary<ObjC, BridgedToObjC> { }

  if let d = dictBO as? Dictionary<BridgedToObjC, BridgedToObjC> { }
  if let d = dictOB as? Dictionary<BridgedToObjC, BridgedToObjC> { }

  // We don't do mixed down/upcasts.
  if let d = dictDO as? Dictionary<BridgedToObjC, BridgedToObjC> { } // expected-error{{'Dictionary<DerivesObjC, ObjC>' is not convertible to 'Dictionary<BridgedToObjC, BridgedToObjC>'}}
}




