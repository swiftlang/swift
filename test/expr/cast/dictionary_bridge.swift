// RUN: %swift -parse %s -verify

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
  
  static func _getObjectiveCType() -> Any.Type {
    return ObjC.self
  }
  func _bridgeToObjectiveC() -> ObjC {
    return ObjC()
  }
  static func _forceBridgeFromObjectiveC(
    x: ObjC,
    inout result: BridgedToObjC?
  ) {
  }
  static func _conditionallyBridgeFromObjectiveC(
    x: ObjC,
    inout result: BridgedToObjC?
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

  dictBB = dictBO // expected-error{{'ObjC' is not identical to 'BridgedToObjC'}}
  dictBB = dictOB // expected-error{{'ObjC' is not identical to 'BridgedToObjC'}}

  dictDO = dictBB // expected-error{{'BridgedToObjC' is not identical to 'DerivesObjC'}}
  dictOD = dictBB // expected-error{{'BridgedToObjC' is not identical to 'ObjC'}}
  dictDD = dictBB // expected-error{{'BridgedToObjC' is not identical to 'DerivesObjC'}}
}

func testDowncastBridge() {
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
  dictRR as Dictionary<BridgedToObjC, BridgedToObjC>
  dictRR as Dictionary<BridgedToObjC, ObjC>
  dictRR as Dictionary<ObjC, BridgedToObjC>

  dictRO as Dictionary<BridgedToObjC, BridgedToObjC>
  dictRO as Dictionary<BridgedToObjC, ObjC>
  dictRO as Dictionary<ObjC, BridgedToObjC>

  dictBO as Dictionary<BridgedToObjC, BridgedToObjC>
  dictOB as Dictionary<BridgedToObjC, BridgedToObjC>

  // We don't do mixed down/upcasts.
  dictDO as Dictionary<BridgedToObjC, BridgedToObjC> // expected-error{{'DerivesObjC' is not identical to 'BridgedToObjC'}}
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
  if let d = dictDO as? Dictionary<BridgedToObjC, BridgedToObjC> { } // expected-error{{'ObjC' is not a subtype of 'DerivesObjC'}}
}




