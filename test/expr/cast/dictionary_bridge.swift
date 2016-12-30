// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop

// FIXME: Should go into the standard library.
public extension _ObjectiveCBridgeable {
  static func _unconditionallyBridgeFromObjectiveC(_ source: _ObjectiveCType?)
      -> Self {
    var result: Self?
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
  dictRR = dictBB as [Root: Root]
  dictRR = dictBO as [Root: Root]
  dictRR = dictOB as [Root: Root]

  dictRO = dictBB as [Root: ObjC]
  dictRO = dictBO as [Root: ObjC]
  dictRO = dictOB as [Root: ObjC]

  dictOR = dictBB as [ObjC: Root]
  dictOR = dictBO as [ObjC: Root]
  dictOR = dictOB as [ObjC: Root]

  dictOO = dictBB as [ObjC: ObjC]
  dictOO = dictBO as [ObjC: ObjC]
  dictOO = dictOB as [ObjC: ObjC]

  // Upcast key or value to object type (but not both)
  dictBO = dictBB as [BridgedToObjC: ObjC]
  dictOB = dictBB as [ObjC: BridgedToObjC]

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

  _ = dictBO as Dictionary<BridgedToObjC, BridgedToObjC>
  _ = dictOB as Dictionary<BridgedToObjC, BridgedToObjC>

  // We don't do mixed down/upcasts.
  _ = dictDO as! Dictionary<BridgedToObjC, BridgedToObjC> // expected-warning{{forced cast from 'Dictionary<DerivesObjC, ObjC>' to 'Dictionary<BridgedToObjC, BridgedToObjC>' always succeeds; did you mean to use 'as'?}}
}

func testConditionalDowncastBridge() {
  let dictRR = Dictionary<Root, Root>()
  let dictRO = Dictionary<Root, ObjC>()
  let dictOR = Dictionary<ObjC, Root>()
  let dictOO = Dictionary<ObjC, ObjC>()
  let dictOD = Dictionary<ObjC, DerivesObjC>()
  let dictDO = Dictionary<DerivesObjC, ObjC>()
  let dictDD = Dictionary<DerivesObjC, DerivesObjC>()

  let dictBB = Dictionary<BridgedToObjC, BridgedToObjC>()
  let dictBO = Dictionary<BridgedToObjC, ObjC>()
  let dictOB = Dictionary<ObjC, BridgedToObjC>()

  // Downcast to bridged value types.
  if let d = dictRR as? Dictionary<BridgedToObjC, BridgedToObjC> { _ = d }
  if let d = dictRR as? Dictionary<BridgedToObjC, ObjC> { _ = d }
  if let d = dictRR as? Dictionary<ObjC, BridgedToObjC> { _ = d }

  if let d = dictRO as? Dictionary<BridgedToObjC, BridgedToObjC> { _ = d }
  if let d = dictRO as? Dictionary<BridgedToObjC, ObjC> { _ = d }
  if let d = dictRO as? Dictionary<ObjC, BridgedToObjC> { _ = d }

  let d1 = dictBO as Dictionary<BridgedToObjC, BridgedToObjC>
  let d2 = dictOB as Dictionary<BridgedToObjC, BridgedToObjC>

  // Mixed down/upcasts.
  if let d = dictDO as? Dictionary<BridgedToObjC, BridgedToObjC> { _ = d }
  // expected-warning@-1{{conditional cast from 'Dictionary<DerivesObjC, ObjC>' to 'Dictionary<BridgedToObjC, BridgedToObjC>' always succeeds}}

  _ = dictRR
  _ = dictRO
  _ = dictOR
  _ = dictOO
  _ = dictOD
  _ = dictDO
  _ = dictDD
  _ = dictBB
  _ = dictBO
  _ = dictOB
  _ = d1
  _ = d2

}




