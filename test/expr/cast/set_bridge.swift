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
  func hash(into hasher: inout Hasher) {}
}

func ==(x: Root, y: Root) -> Bool { return true }

class ObjC : Root {
  var x = 0
}

class DerivesObjC : ObjC { }

class Unrelated : Root { }

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

  func hash(into hasher: inout Hasher) {}
}

func ==(x: BridgedToObjC, y: BridgedToObjC) -> Bool { return true }

func testUpcastBridge() {
  var setR = Set<Root>()
  var setO = Set<ObjC>()
  var setD = Set<DerivesObjC>()
  var setB = Set<BridgedToObjC>()

  // Upcast to object types.
  setR = setB as Set<Root>; _ = setR
  setO = setB as Set<ObjC>; _ = setO

  // Upcast object to bridged type
  setB = setO // expected-error{{cannot assign value of type 'Set<ObjC>' to type 'Set<BridgedToObjC>'}}

  // Failed upcast
  setD = setB // expected-error{{cannot assign value of type 'Set<BridgedToObjC>' to type 'Set<DerivesObjC>'}}
  _ = setD
}

func testForcedDowncastBridge() {
  let setR = Set<Root>()
  let setO = Set<ObjC>()
  let setD = Set<DerivesObjC>()
  let setB = Set<BridgedToObjC>()

  _ = setR as! Set<BridgedToObjC>
  _ = setO as Set<BridgedToObjC>
  _ = setD as! Set<BridgedToObjC> // expected-warning{{forced cast from 'Set<DerivesObjC>' to 'Set<BridgedToObjC>' always succeeds; did you mean to use 'as'?}}

  _ = setB as! Set<Root> // expected-warning{{forced cast from 'Set<BridgedToObjC>' to 'Set<Root>' always succeeds; did you mean to use 'as'?}}
  _ = setB as! Set<ObjC> // expected-warning{{forced cast from 'Set<BridgedToObjC>' to 'Set<ObjC>' always succeeds; did you mean to use 'as'?}}
  _ = setB as! Set<DerivesObjC>
}

func testConditionalDowncastBridge() {
  let setR = Set<Root>()
  let setO = Set<ObjC>()
  let setD = Set<DerivesObjC>()
  let setB = Set<BridgedToObjC>()

  if let s = setR as? Set<BridgedToObjC> { _ = s }
  let s1 = setO as Set<BridgedToObjC>
  if let s = setD as? Set<BridgedToObjC> { _ = s } // expected-warning {{conditional cast from 'Set<DerivesObjC>' to 'Set<BridgedToObjC>' always succeeds}}

  if let s = setB as? Set<Root> { _ = s } // expected-warning{{conditional cast from 'Set<BridgedToObjC>' to 'Set<Root>' always succeeds}}
  if let s = setB as? Set<ObjC> { _ = s } // expected-warning{{conditional cast from 'Set<BridgedToObjC>' to 'Set<ObjC>' always succeeds}}
  if let s = setB as? Set<DerivesObjC> { _ = s }
  if let s = setB as? Set<Unrelated> { _ = s } // expected-warning {{cast from 'Set<BridgedToObjC>' to unrelated type 'Set<Unrelated>' always fails}}

  _ = setR
  _ = setO
  _ = setD
  _ = setB
  _ = s1
}




