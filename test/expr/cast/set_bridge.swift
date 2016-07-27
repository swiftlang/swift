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
  _ = setD as! Set<BridgedToObjC> // expected-error {{'ObjC' is not a subtype of 'DerivesObjC'}}
  // expected-note @-1 {{in cast from type 'Set<DerivesObjC>' to 'Set<BridgedToObjC>'}}

  // TODO: the diagnostic for the below two examples should indicate that 'as'
  // should be used instead of 'as!'
  _ = setB as! Set<Root> // expected-error {{'Root' is not a subtype of 'BridgedToObjC'}}
  // expected-note @-1 {{in cast from type 'Set<BridgedToObjC>' to 'Set<Root>'}}
  _ = setB as! Set<ObjC> // expected-error {{'ObjC' is not a subtype of 'BridgedToObjC'}}
  // expected-note @-1 {{in cast from type 'Set<BridgedToObjC>' to 'Set<ObjC>'}}
  _ = setB as! Set<DerivesObjC> // expected-error {{'DerivesObjC' is not a subtype of 'BridgedToObjC'}}
  // expected-note @-1 {{in cast from type 'Set<BridgedToObjC>' to 'Set<DerivesObjC>'}}
}

func testConditionalDowncastBridge() {
  var setR = Set<Root>()
  var setO = Set<ObjC>()
  var setD = Set<DerivesObjC>()
  var setB = Set<BridgedToObjC>()

  if let s = setR as? Set<BridgedToObjC> { }
  let s1 = setO as Set<BridgedToObjC>
  if let s = setD as? Set<BridgedToObjC> { } // expected-error {{'ObjC' is not a subtype of 'DerivesObjC'}}
  // expected-note @-1 {{in cast from type 'Set<DerivesObjC>' to 'Set<BridgedToObjC>'}}

  if let s = setB as? Set<Root> { } // expected-error {{'Root' is not a subtype of 'BridgedToObjC'}}
  // expected-note @-1 {{in cast from type 'Set<BridgedToObjC>' to 'Set<Root>'}}
  if let s = setB as? Set<ObjC> { } // expected-error {{'ObjC' is not a subtype of 'BridgedToObjC'}}
  // expected-note @-1 {{in cast from type 'Set<BridgedToObjC>' to 'Set<ObjC>'}}
  if let s = setB as? Set<DerivesObjC> { } // expected-error {{'DerivesObjC' is not a subtype of 'BridgedToObjC'}}
  // expected-note @-1 {{in cast from type 'Set<BridgedToObjC>' to 'Set<DerivesObjC>'}}
}




