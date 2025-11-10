// RUN: %target-swift-frontend -emit-sil -parse-as-library -target %target-swift-5.1-abi-triple -strict-concurrency=complete -verify -verify-additional-prefix ni- %s -o /dev/null -enable-upcoming-feature GlobalActorIsolatedTypesUsability
// RUN: %target-swift-frontend -emit-sil -parse-as-library -target %target-swift-5.1-abi-triple -strict-concurrency=complete -verify -verify-additional-prefix ni-ns- %s -o /dev/null -enable-upcoming-feature GlobalActorIsolatedTypesUsability -enable-upcoming-feature NonisolatedNonsendingByDefault

// REQUIRES: concurrency
// REQUIRES: swift_feature_GlobalActorIsolatedTypesUsability
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

////////////////////////
// MARK: Declarations //
////////////////////////

class NonSendableKlass {
  func use() {}
}

struct NonSendableStruct {
  var first = NonSendableKlass()
  var second = NonSendableKlass()
}

class KlassWithNonSendableStructPair {
  var ns1: NonSendableStruct
  var ns2: (NonSendableStruct, NonSendableStruct)

  init() {
    ns1 = NonSendableStruct()
    ns2 = (ns1, ns1)
  }
}

final class FinalKlassWithNonSendableStructPair {
  var ns1: NonSendableStruct
  var ns2: (NonSendableStruct, NonSendableStruct)

  init() {
    ns1 = NonSendableStruct()
    ns2 = (ns1, ns1)
  }
}

func useValue<T>(_ t: T) {}
func useValueAndReturnGeneric<T>(_ t: T) -> T { t }
func useNonSendableKlassAndReturn(_ t: NonSendableKlass) -> NonSendableKlass { t }
func getAny() -> Any { fatalError() }

actor Custom {
}

@globalActor
struct CustomActor {
  static var shared: Custom {
    return Custom()
  }
}

@MainActor func transferToMain<T>(_ t: T) {}
@CustomActor func transferToCustom<T>(_ t: T) {}

func throwingFunction() throws { fatalError() }

func getBool() -> Bool { false }

func transferArg(_ x: sending NonSendableKlass) {
}

func transferArgAsync(_ x: sending NonSendableKlass) async {
}

func transferArgWithOtherParam(_ x: sending NonSendableKlass, _ y: NonSendableKlass) {
}

func transferArgWithOtherParam2(_ x: NonSendableKlass, _ y: sending NonSendableKlass) {
}

func twoTransferArg(_ x: sending NonSendableKlass, _ y: sending NonSendableKlass) {}

@MainActor var globalKlass = NonSendableKlass()

struct MyError : Error {}

func takeClosure(_ x: sending () -> ()) {}
func takeClosureAndParam(_ x: NonSendableKlass, _ y: sending () -> ()) {}
func useInOutSending(_ x: inout sending NonSendableKlass) {}
func useInOutSending(_ x: inout sending NonSendableKlass,
                     _ y: inout sending NonSendableKlass) {}

///////////////////////////////
// MARK: InOut Sending Tests //
///////////////////////////////

func testInOutSendingReinit(_ x: inout sending NonSendableKlass) async {
  await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}
} // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}

func testInOutSendingReinit2(_ x: inout sending NonSendableKlass) async {
  await transferToMain(x)
  x = NonSendableKlass()
}

func testInOutSendingReinit3(_ x: inout sending NonSendableKlass) async throws {
  await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}

  try throwingFunction() // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}

  x = NonSendableKlass()
}

func testInOutSendingReinit4(_ x: inout sending NonSendableKlass) async throws {
  await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}

  do {
    try throwingFunction()
    x = NonSendableKlass()
  } catch {
    throw MyError() // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}
  }

  x = NonSendableKlass()
}

func testInOutSendingReinit5(_ x: inout sending NonSendableKlass) async throws {
  await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}

  do {
    try throwingFunction()
  } catch {
    throw MyError() // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}
  }

  x = NonSendableKlass()
}

func testInOutSendingReinit6(_ x: inout sending NonSendableKlass) async throws {
  await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}

  do {
    try throwingFunction()
  } catch {
    throw MyError() // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}
  }
} // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}

actor InOutSendingWrongIsolationActor {
  var ns = NonSendableKlass()
  func testWrongIsolation(_ x: inout sending NonSendableKlass) {
    x = ns
  } // expected-warning {{'inout sending' parameter 'x' cannot be 'self'-isolated at end of function}}
  // expected-note @-1 {{'self'-isolated 'x' risks causing races in between 'self'-isolated uses and caller uses since caller assumes value is not actor isolated}}

  func testWrongIsolation2(_ x: inout sending NonSendableKlass) {
    let z = ns
    x = z
  } // expected-warning {{'inout sending' parameter 'x' cannot be 'self'-isolated at end of function}}
  // expected-note @-1 {{'self'-isolated 'x' risks causing races in between 'self'-isolated uses and caller uses since caller assumes value is not actor isolated}}
}

@MainActor
func testWrongIsolationGlobalIsolation(_ x: inout sending NonSendableKlass) {
  x = globalKlass
} // expected-warning {{'inout sending' parameter 'x' cannot be main actor-isolated at end of function}}
// expected-note @-1 {{main actor-isolated 'x' risks causing races in between main actor-isolated uses and caller uses since caller assumes value is not actor isolated}}

func passInOutSendingToInOutSending(_ x: inout sending NonSendableKlass) {
  useInOutSending(&x)
}

func passInOutSendingMultipleTimes(_ x: inout NonSendableStruct) {
  useInOutSending(&x.first, &x.second) // expected-warning {{sending 'x.first' risks causing data races}}
  // expected-note @-1 {{task-isolated 'x.first' is passed as a 'sending' parameter}}
  // expected-warning @-2 {{sending 'x.second' risks causing data races}}
  // expected-note @-3 {{task-isolated 'x.second' is passed as a 'sending' parameter}}
}

func twoInoutSendingParamsInSameRegion(_ x: inout sending NonSendableKlass, _ y: inout sending NonSendableKlass) {
  x = y
} // expected-warning {{'inout sending' parameters 'x' and 'y' can be potentially accessed from each other at function return risking data races in caller; this is an error in the Swift 6 language mode}}
// expected-note @-1 {{caller function assumes on return that 'x' and 'y' cannot be used to access each other implying sending them to different isolation domains does not risk a data race}}

func twoInoutSendingParamsInSameRegion2(_ x: inout sending NonSendableKlass, _ y: inout sending NonSendableKlass) {
  let z = y
  x = z
} // expected-warning {{'inout sending' parameters 'x' and 'y' can be potentially accessed from each other at function return risking data races in caller; this is an error in the Swift 6 language mode}}
// expected-note @-1 {{caller function assumes on return that 'x' and 'y' cannot be used to access each other implying sending them to different isolation domains does not risk a data race}}

func twoInoutSendingParamsInSameRegion3(_ x: inout sending NonSendableKlass, _ y: inout sending NonSendableKlass) {
  let z = y
  let y = (x, z) // expected-warning {{initialization of immutable value 'y' was never used; consider replacing with assignment to '_' or removing it}}
} // expected-warning {{'inout sending' parameters 'x' and 'y' can be potentially accessed from each other at function return risking data races in caller; this is an error in the Swift 6 language mode}}
// expected-note @-1 {{caller function assumes on return that 'x' and 'y' cannot be used to access each other implying sending them to different isolation domains does not risk a data race}}

func twoInoutSendingParamsInSameRegion4(_ x: inout sending NonSendableKlass, _ y: inout sending NonSendableKlass) {
  let z = y
  let h = x
  func doSomething(_ x: NonSendableKlass, _ y: NonSendableKlass) {
  }
  doSomething(z, h)
} // expected-warning {{'inout sending' parameters 'x' and 'y' can be potentially accessed from each other at function return risking data races in caller; this is an error in the Swift 6 language mode}}
// expected-note @-1 {{caller function assumes on return that 'x' and 'y' cannot be used to access each other implying sending them to different isolation domains does not risk a data race}}

func multipleInOutSendingParamsInSameRegion1(_ x1: inout sending NonSendableKlass, _ x2: inout sending NonSendableKlass, _ x3: inout sending NonSendableKlass) {
  x1 = x2
  x2 = x3
} // expected-warning {{'inout sending' parameters 'x2' and 'x3' can be potentially accessed from each other at function return risking data races in caller; this is an error in the Swift 6 language mode}}
// expected-note @-1 {{caller function assumes on return that 'x2' and 'x3' cannot be used to access each other implying sending them to different isolation domains does not risk a data race}}

func multipleInOutSendingParamsInSameRegion2(_ x1: inout sending NonSendableKlass, _ x2: inout sending NonSendableKlass, _ x3: inout sending NonSendableKlass) {
  let y = (x1, x2, x3)
  _ = y
} // expected-warning {{'inout sending' parameters 'x2' and 'x3' can be potentially accessed from each other at function return risking data races in caller; this is an error in the Swift 6 language mode}}
// expected-note @-1 {{caller function assumes on return that 'x2' and 'x3' cannot be used to access each other implying sending them to different isolation domains does not risk a data race}}
// expected-warning @-2 {{'inout sending' parameters 'x1' and 'x3' can be potentially accessed from each other at function return risking data races in caller; this is an error in the Swift 6 language mode}}
// expected-note @-3 {{caller function assumes on return that 'x1' and 'x3' cannot be used to access each other implying sending them to different isolation domains does not risk a data race}}
// expected-warning @-4 {{'inout sending' parameters 'x1' and 'x2' can be potentially accessed from each other at function return risking data races in caller; this is an error in the Swift 6 language mode}}
// expected-note @-5 {{caller function assumes on return that 'x1' and 'x2' cannot be used to access each other implying sending them to different isolation domains does not risk a data race}}

func multipleInOutSendingParamsInSameRegion3(_ x1: inout sending NonSendableKlass, _ x2: inout sending NonSendableKlass, _ x3: inout sending NonSendableKlass) {
  let y = (x2, x3, x1)
  _ = y
} // expected-warning {{'inout sending' parameters 'x2' and 'x3' can be potentially accessed from each other at function return risking data races in caller; this is an error in the Swift 6 language mode}}
// expected-note @-1 {{caller function assumes on return that 'x2' and 'x3' cannot be used to access each other implying sending them to different isolation domains does not risk a data race}}
// expected-warning @-2 {{'inout sending' parameters 'x1' and 'x3' can be potentially accessed from each other at function return risking data races in caller; this is an error in the Swift 6 language mode}}
// expected-note @-3 {{caller function assumes on return that 'x1' and 'x3' cannot be used to access each other implying sending them to different isolation domains does not risk a data race}}
// expected-warning @-4 {{'inout sending' parameters 'x1' and 'x2' can be potentially accessed from each other at function return risking data races in caller; this is an error in the Swift 6 language mode}}
// expected-note @-5 {{caller function assumes on return that 'x1' and 'x2' cannot be used to access each other implying sending them to different isolation domains does not risk a data race}}

//////////////////////////////////////
// MARK: Return Inout Sending Tests //
//////////////////////////////////////

func returnInOutSendingDirectly(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingRegionLet(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  let y = x
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingRegionVar(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  var y = x
  y = x
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}


func returnInOutSendingViaHelper(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  let y = x
  return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
  // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingHelperDirect(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
  // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingDirectlyIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  if getBool() {
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  fatalError()
}

func returnInOutSendingRegionLetIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  let y = x
  if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  } else {
    print(y)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnInOutSendingRegionVarIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  var y = x
  y = x
  if getBool() {
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  fatalError()
}

func returnInOutSendingViaHelperIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  let y = x
  if getBool() {
    return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  } else {
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnInOutSendingHelperDirectIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  if getBool() {
    return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  } else {
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnInOutSendingViaHelperVarIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  var y = x
  y = x
  if getBool() {
    return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  } else {
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnInOutSendingDirectlyElse(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  if getBool() {
    print(x)
    fatalError()
  } else {
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnInOutSendingRegionLetElse(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  let y = x
  if getBool() {
    print(y)
    fatalError()
  } else {
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnInOutSendingRegionVarElse(_ x: inout sending NonSendableKlass, z: NonSendableKlass) -> NonSendableKlass {
  var y = x
  y = x
  if getBool() {
    print(y)
    return z
  } else {
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnInOutSendingViaHelperElse(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
  let y = x
  if getBool() {
    print(y)
    return z
  } else {
    return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnInOutSendingHelperDirectElse(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
  if getBool() {
    print(x)
    return z
  } else {
    return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnInOutSendingViaHelperVarElse(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
  var y = x
  y = x
  if getBool() {
    print(y)
    return z
  } else {
    return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnInOutSendingDirectlyFor(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
  for _ in 0..<1 {
    if getBool() {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  print(x)
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingDirectlyFor2(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
  for _ in 0..<1 {
    if getBool() {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  print(x)
  return z
}

func returnInOutSendingRegionLetFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  let y = x
  for _ in 0..<1 {
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingRegionLetFor2(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  let y = x
  for _ in 0..<1 {
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingRegionLetFor3(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
  let y = x
  for _ in 0..<1 {
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return z
}

func returnInOutSendingRegionVarFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  var y = x
  y = x
  for _ in 0..<1 {
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingRegionVarFor2(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  var y = x
  y = x
  for _ in 0..<1 {
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingRegionVarFor3(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
  var y = x
  y = x
  for _ in 0..<1 {
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return z
}

func returnInOutSendingViaHelperFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  let y = x
  for _ in 0..<1 {
    if getBool() {
      return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  print(y)
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingViaHelperFor2(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  let y = x
  for _ in 0..<1 {
    if getBool() {
      return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  print(y)
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingHelperDirectFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  for _ in 0..<1 {
    if getBool() {
      return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingViaHelperVarFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  var y = x
  y = x
  for _ in 0..<1 {
    if getBool() {
      return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  print(y)
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingDirectlyGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  guard getBool() else {
    print(x)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingRegionLetGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  let y = x
  guard getBool() else {
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingRegionVarGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  var y = x
  y = x
  guard getBool() else {
    print(y)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingViaHelperGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  let y = x
  guard getBool() else {
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
  // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingHelperDirectGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  guard getBool() else {
    print(x)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
  // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingViaHelperVarGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  var y = x
  y = x
  guard getBool() else {
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
  // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnInOutSendingViaHelperVar(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
  var y = x
  y = x
  return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
  // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingDirectly<T>(_ x: inout sending T) -> T {
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingRegionLet<T>(_ x: inout sending T) -> T {
  let y = x
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingRegionVar<T>(_ x: inout sending T) -> T {
  var y = x
  y = x
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingViaHelper<T>(_ x: inout sending T) -> T {
  let y = x
  return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
  // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingHelperDirect<T>(_ x: inout sending T) -> T {
  return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
  // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingViaHelperVar<T>(_ x: inout sending T) -> T {
  var y = x
  y = x
  return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
  // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingDirectlyIf<T>(_ x: inout sending T) -> T {
  if getBool() {
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  fatalError()
}

func returnGenericInOutSendingRegionLetIf<T>(_ x: inout sending T) -> T {
  let y = x
  if getBool() {
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  } else {
    print(y)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnGenericInOutSendingRegionVarIf<T>(_ x: inout sending T) -> T {
  var y = x
  y = x
  if getBool() {
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  fatalError()
}

func returnGenericInOutSendingViaHelperIf<T>(_ x: inout sending T) -> T {
  let y = x
  if getBool() {
    return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  } else {
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnGenericInOutSendingHelperDirectIf<T>(_ x: inout sending T) -> T {
  if getBool() {
    return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  } else {
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnGenericInOutSendingViaHelperVarIf<T>(_ x: inout sending T) -> T {
  var y = x
  y = x
  if getBool() {
    return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  } else {
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnGenericInOutSendingDirectlyElse<T>(_ x: inout sending T) -> T {
  if getBool() {
    print(x)
    fatalError()
  } else {
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnGenericInOutSendingRegionLetElse<T>(_ x: inout sending T) -> T {
  let y = x
  if getBool() {
    print(y)
    fatalError()
  } else {
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnGenericInOutSendingRegionVarElse<T>(_ x: inout sending T, z: T) -> T {
  var y = x
  y = x
  if getBool() {
    print(y)
    return z
  } else {
      return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnGenericInOutSendingViaHelperElse<T>(_ x: inout sending T, _ z: T) -> T {
  let y = x
  if getBool() {
    print(y)
    return z
  } else {
    return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnGenericInOutSendingHelperDirectElse<T>(_ x: inout sending T, _ z: T) -> T {
  if getBool() {
    print(x)
    return z
  } else {
    return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnGenericInOutSendingViaHelperVarElse<T>(_ x: inout sending T, _ z: T) -> T {
  var y = x
  y = x
  if getBool() {
    print(y)
    return z
  } else {
    return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

func returnGenericInOutSendingDirectlyFor<T>(_ x: inout sending T, _ z: T) -> T {
  for _ in 0..<1 {
    if getBool() {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  print(x)
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingDirectlyFor2<T>(_ x: inout sending T, _ z: T) -> T {
  for _ in 0..<1 {
    if getBool() {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  print(x)
  return z
}

func returnGenericInOutSendingRegionLetFor<T>(_ x: inout sending T) -> T {
  let y = x
  for _ in 0..<1 {
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingRegionLetFor2<T>(_ x: inout sending T) -> T {
  let y = x
  for _ in 0..<1 {
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingRegionLetFor3<T>(_ x: inout sending T, _ z: T) -> T {
  let y = x
  for _ in 0..<1 {
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return z
}

func returnGenericInOutSendingRegionVarFor<T>(_ x: inout sending T) -> T {
  var y = x
  y = x
  for _ in 0..<1 {
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingRegionVarFor2<T>(_ x: inout sending T) -> T {
  var y = x
  y = x
  for _ in 0..<1 {
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingRegionVarFor3<T>(_ x: inout sending T, _ z: T) -> T {
  var y = x
  y = x
  for _ in 0..<1 {
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return z
}

func returnGenericInOutSendingViaHelperFor<T>(_ x: inout sending T) -> T {
  let y = x
  for _ in 0..<1 {
    if getBool() {
      return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  print(y)
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingViaHelperFor2<T>(_ x: inout sending T) -> T {
  let y = x
  for _ in 0..<1 {
    if getBool() {
      return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  print(y)
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingHelperDirectFor<T>(_ x: inout sending T) -> T {
  for _ in 0..<1 {
    if getBool() {
      return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingViaHelperVarFor<T>(_ x: inout sending T) -> T {
  var y = x
  y = x
  for _ in 0..<1 {
    if getBool() {
      return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
    }
  }
  print(y)
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingDirectlyGuard<T>(_ x: inout sending T) -> T {
  guard getBool() else {
    print(x)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingRegionLetGuard<T>(_ x: inout sending T) -> T {
  let y = x
  guard getBool() else {
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingRegionVarGuard<T>(_ x: inout sending T) -> T {
  var y = x
  y = x
  guard getBool() else {
    print(y)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  return y // expected-warning {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingViaHelperGuard<T>(_ x: inout sending T) -> T {
  let y = x
  guard getBool() else {
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
  // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingHelperDirectGuard<T>(_ x: inout sending T) -> T {
  guard getBool() else {
    print(x)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
  // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

func returnGenericInOutSendingViaHelperVarGuard<T>(_ x: inout sending T) -> T {
  var y = x
  y = x
  guard getBool() else {
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
  return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
  // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

///////////////////////////////////////////////////
// MARK: ReturnSendingInOutTestActor Actor Tests //
///////////////////////////////////////////////////

actor ReturnSendingInOutTestActor {

  func testInOutSendingReinit(_ x: inout sending NonSendableKlass) async {
    await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
    // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local actor-isolated uses}}
  } // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}

  func testInOutSendingReinit2(_ x: inout sending NonSendableKlass) async {
    await transferToMain(x)
    x = NonSendableKlass()
  }

  func testInOutSendingReinit3(_ x: inout sending NonSendableKlass) async throws {
    await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
    // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local actor-isolated uses}}


    try throwingFunction() // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}

    x = NonSendableKlass()
  }

  func testInOutSendingReinit4(_ x: inout sending NonSendableKlass) async throws {
    await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
    // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local actor-isolated uses}}


    do {
      try throwingFunction()
      x = NonSendableKlass()
    } catch {
      throw MyError() // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}
    }

    x = NonSendableKlass()
  }

  func testInOutSendingReinit5(_ x: inout sending NonSendableKlass) async throws {
    await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
    // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local actor-isolated uses}}


    do {
      try throwingFunction()
    } catch {
      throw MyError() // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}
    }

    x = NonSendableKlass()
  }

  func testInOutSendingReinit6(_ x: inout sending NonSendableKlass) async throws {
    await transferToMain(x) // expected-warning {{sending 'x' risks causing data races}}
    // expected-note @-1 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local actor-isolated uses}}


    do {
      try throwingFunction()
    } catch {
      throw MyError() // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}
    }
  } // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}

  func returnInOutSendingDirectly(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingRegionLet(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingRegionVar(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingViaHelper(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingHelperDirect(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingDirectlyIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    if getBool() {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    fatalError()
  }

  func returnInOutSendingRegionLetIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    } else {
      print(y)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnInOutSendingRegionVarIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    fatalError()
  }

  func returnInOutSendingViaHelperIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    if getBool() {
      return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    } else {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnInOutSendingHelperDirectIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    if getBool() {
      return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    } else {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnInOutSendingViaHelperVarIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    if getBool() {
      return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    } else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnInOutSendingDirectlyElse(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    if getBool() {
      print(x)
      fatalError()
    } else {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnInOutSendingRegionLetElse(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    if getBool() {
      print(y)
      fatalError()
    } else {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnInOutSendingRegionVarElse(_ x: inout sending NonSendableKlass, z: NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    if getBool() {
      print(y)
      return z
    } else {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnInOutSendingViaHelperElse(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    let y = x
    if getBool() {
      print(y)
      return z
    } else {
      return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnInOutSendingHelperDirectElse(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    if getBool() {
      print(x)
      return z
    } else {
      return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnInOutSendingViaHelperVarElse(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    if getBool() {
      print(y)
      return z
    } else {
      return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnInOutSendingDirectlyFor(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    for _ in 0..<1 {
      if getBool() {
        return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
        // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    print(x)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingDirectlyFor2(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    for _ in 0..<1 {
      if getBool() {
        return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
        // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    print(x)
    return z
  }

  func returnInOutSendingRegionLetFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingRegionLetFor2(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingRegionLetFor3(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return z
  }

  func returnInOutSendingRegionVarFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingRegionVarFor2(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingRegionVarFor3(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return z
  }

  func returnInOutSendingViaHelperFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    print(y)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingViaHelperFor2(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingHelperDirectFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    for _ in 0..<1 {
      if getBool() {
        return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingViaHelperVarFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingDirectlyGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    guard getBool() else {
      print(x)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingRegionLetGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    guard getBool() else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingRegionVarGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    guard getBool() else {
      print(y)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingViaHelperGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    guard getBool() else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingHelperDirectGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    guard getBool() else {
      print(x)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingViaHelperVarGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    guard getBool() else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnInOutSendingViaHelperVar(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingDirectly<T>(_ x: inout sending T) -> T {
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingRegionLet<T>(_ x: inout sending T) -> T {
    let y = x
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingRegionVar<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingViaHelper<T>(_ x: inout sending T) -> T {
    let y = x
    return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingHelperDirect<T>(_ x: inout sending T) -> T {
    return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingViaHelperVar<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingDirectlyIf<T>(_ x: inout sending T) -> T {
    if getBool() {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    fatalError()
  }

  func returnGenericInOutSendingRegionLetIf<T>(_ x: inout sending T) -> T {
    let y = x
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    } else {
      print(y)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnGenericInOutSendingRegionVarIf<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    fatalError()
  }

  func returnGenericInOutSendingViaHelperIf<T>(_ x: inout sending T) -> T {
    let y = x
    if getBool() {
      return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    } else {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnGenericInOutSendingHelperDirectIf<T>(_ x: inout sending T) -> T {
    if getBool() {
      return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    } else {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnGenericInOutSendingViaHelperVarIf<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    if getBool() {
      return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    } else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnGenericInOutSendingDirectlyElse<T>(_ x: inout sending T) -> T {
    if getBool() {
      print(x)
      fatalError()
    } else {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnGenericInOutSendingRegionLetElse<T>(_ x: inout sending T) -> T {
    let y = x
    if getBool() {
      print(y)
      fatalError()
    } else {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnGenericInOutSendingRegionVarElse<T>(_ x: inout sending T, z: T) -> T {
    var y = x
    y = x
    if getBool() {
      print(y)
      return z
    } else {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnGenericInOutSendingViaHelperElse<T>(_ x: inout sending T, _ z: T) -> T {
    let y = x
    if getBool() {
      print(y)
      return z
    } else {
      return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnGenericInOutSendingHelperDirectElse<T>(_ x: inout sending T, _ z: T) -> T {
    if getBool() {
      print(x)
      return z
    } else {
      return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnGenericInOutSendingViaHelperVarElse<T>(_ x: inout sending T, _ z: T) -> T {
    var y = x
    y = x
    if getBool() {
      print(y)
      return z
    } else {
      return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
  }

  func returnGenericInOutSendingDirectlyFor<T>(_ x: inout sending T, _ z: T) -> T {
    for _ in 0..<1 {
      if getBool() {
        return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
        // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    print(x)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingDirectlyFor2<T>(_ x: inout sending T, _ z: T) -> T {
    for _ in 0..<1 {
      if getBool() {
        return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
        // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    print(x)
    return z
  }

  func returnGenericInOutSendingRegionLetFor<T>(_ x: inout sending T) -> T {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingRegionLetFor2<T>(_ x: inout sending T) -> T {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingRegionLetFor3<T>(_ x: inout sending T, _ z: T) -> T {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return z
  }

  func returnGenericInOutSendingRegionVarFor<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingRegionVarFor2<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingRegionVarFor3<T>(_ x: inout sending T, _ z: T) -> T {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return z
  }

  func returnGenericInOutSendingViaHelperFor<T>(_ x: inout sending T) -> T {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    print(y)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingViaHelperFor2<T>(_ x: inout sending T) -> T {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingHelperDirectFor<T>(_ x: inout sending T) -> T {
    for _ in 0..<1 {
      if getBool() {
        return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingViaHelperVarFor<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
      }
    }
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingDirectlyGuard<T>(_ x: inout sending T) -> T {
    guard getBool() else {
      print(x)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingRegionLetGuard<T>(_ x: inout sending T) -> T {
    let y = x
    guard getBool() else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingRegionVarGuard<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    guard getBool() else {
      print(y)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingViaHelperGuard<T>(_ x: inout sending T) -> T {
    let y = x
    guard getBool() else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingHelperDirectGuard<T>(_ x: inout sending T) -> T {
    guard getBool() else {
      print(x)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }

  func returnGenericInOutSendingViaHelperVarGuard<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    guard getBool() else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
    }
    return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is 'self'-isolated}}
  }
}

////////////////////////////////////////////////////////////////
// MARK: ReturnSendingInOutTestGlobalActorIsolatedClass Tests //
////////////////////////////////////////////////////////////////

@MainActor
class ReturnSendingInOutTestGlobalActorIsolatedClass {

  func testInOutSendingReinit(_ x: inout sending NonSendableKlass) async {
    await transferToCustom(x) // expected-warning {{sending 'x' risks causing data races}}
  } // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}
    // expected-note @-2 {{sending 'x' to global actor 'CustomActor'-isolated global function 'transferToCustom' risks causing data races between global actor 'CustomActor'-isolated and local main actor-isolated uses}}

  func testInOutSendingReinit2(_ x: inout sending NonSendableKlass) async {
    await transferToCustom(x)
    x = NonSendableKlass()
  }

  func testInOutSendingReinit3(_ x: inout sending NonSendableKlass) async throws {
    await transferToCustom(x) // expected-warning {{sending 'x' risks causing data races}}
    // expected-note @-1 {{sending 'x' to global actor 'CustomActor'-isolated global function 'transferToCustom' risks causing data races between global actor 'CustomActor'-isolated and local main actor-isolated uses}}

    try throwingFunction() // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}

    x = NonSendableKlass()
  }

  func testInOutSendingReinit4(_ x: inout sending NonSendableKlass) async throws {
    await transferToCustom(x) // expected-warning {{sending 'x' risks causing data races}}
    // expected-note @-1 {{sending 'x' to global actor 'CustomActor'-isolated global function 'transferToCustom' risks causing data races between global actor 'CustomActor'-isolated and local main actor-isolated uses}}

    do {
      try throwingFunction()
      x = NonSendableKlass()
    } catch {
      throw MyError() // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}
    }

    x = NonSendableKlass()
  }

  func testInOutSendingReinit5(_ x: inout sending NonSendableKlass) async throws {
    await transferToCustom(x) // expected-warning {{sending 'x' risks causing data races}}
    // expected-note @-1 {{sending 'x' to global actor 'CustomActor'-isolated global function 'transferToCustom' risks causing data races between global actor 'CustomActor'-isolated and local main actor-isolated uses}}

    do {
      try throwingFunction()
    } catch {
      throw MyError() // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}
    }

    x = NonSendableKlass()
  }

  func testInOutSendingReinit6(_ x: inout sending NonSendableKlass) async throws {
    await transferToCustom(x) // expected-warning {{sending 'x' risks causing data races}}
    // expected-note @-1 {{sending 'x' to global actor 'CustomActor'-isolated global function 'transferToCustom' risks causing data races between global actor 'CustomActor'-isolated and local main actor-isolated uses}}

    do {
      try throwingFunction()
    } catch {
      throw MyError() // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}
    }
  } // expected-note {{'inout sending' parameter must be reinitialized before function exit with a non-actor-isolated value}}

  func returnInOutSendingDirectly(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingRegionLet(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingRegionVar(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingViaHelper(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingHelperDirect(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingDirectlyIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    if getBool() {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    fatalError()
  }

  func returnInOutSendingRegionLetIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    } else {
      print(y)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnInOutSendingRegionVarIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    fatalError()
  }

  func returnInOutSendingViaHelperIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    if getBool() {
      return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    } else {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnInOutSendingHelperDirectIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    if getBool() {
      return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    } else {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnInOutSendingViaHelperVarIf(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    if getBool() {
      return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    } else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnInOutSendingDirectlyElse(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    if getBool() {
      print(x)
      fatalError()
    } else {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnInOutSendingRegionLetElse(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    if getBool() {
      print(y)
      fatalError()
    } else {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnInOutSendingRegionVarElse(_ x: inout sending NonSendableKlass, z: NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    if getBool() {
      print(y)
      return z
    } else {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnInOutSendingViaHelperElse(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    let y = x
    if getBool() {
      print(y)
      return z
    } else {
      return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnInOutSendingHelperDirectElse(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    if getBool() {
      print(x)
      return z
    } else {
      return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnInOutSendingViaHelperVarElse(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    if getBool() {
      print(y)
      return z
    } else {
      return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnInOutSendingDirectlyFor(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    for _ in 0..<1 {
      if getBool() {
        return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
        // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    print(x)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingDirectlyFor2(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    for _ in 0..<1 {
      if getBool() {
        return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
        // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    print(x)
    return z
  }

  func returnInOutSendingRegionLetFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingRegionLetFor2(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingRegionLetFor3(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return z
  }

  func returnInOutSendingRegionVarFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingRegionVarFor2(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingRegionVarFor3(_ x: inout sending NonSendableKlass, _ z: NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return z
  }

  func returnInOutSendingViaHelperFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    print(y)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingViaHelperFor2(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingHelperDirectFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    for _ in 0..<1 {
      if getBool() {
        return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingViaHelperVarFor(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingDirectlyGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    guard getBool() else {
      print(x)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingRegionLetGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    guard getBool() else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingRegionVarGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    guard getBool() else {
      print(y)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingViaHelperGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    let y = x
    guard getBool() else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingHelperDirectGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    guard getBool() else {
      print(x)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    return useNonSendableKlassAndReturn(x) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingViaHelperVarGuard(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    guard getBool() else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnInOutSendingViaHelperVar(_ x: inout sending NonSendableKlass) -> NonSendableKlass {
    var y = x
    y = x
    return useNonSendableKlassAndReturn(y) // expected-warning {{result of global function 'useNonSendableKlassAndReturn' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useNonSendableKlassAndReturn' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingDirectly<T>(_ x: inout sending T) -> T {
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingRegionLet<T>(_ x: inout sending T) -> T {
    let y = x
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingRegionVar<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingViaHelper<T>(_ x: inout sending T) -> T {
    let y = x
    return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingHelperDirect<T>(_ x: inout sending T) -> T {
    return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingViaHelperVar<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingDirectlyIf<T>(_ x: inout sending T) -> T {
    if getBool() {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    fatalError()
  }

  func returnGenericInOutSendingRegionLetIf<T>(_ x: inout sending T) -> T {
    let y = x
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    } else {
      print(y)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnGenericInOutSendingRegionVarIf<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    if getBool() {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    fatalError()
  }

  func returnGenericInOutSendingViaHelperIf<T>(_ x: inout sending T) -> T {
    let y = x
    if getBool() {
      return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    } else {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnGenericInOutSendingHelperDirectIf<T>(_ x: inout sending T) -> T {
    if getBool() {
      return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    } else {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnGenericInOutSendingViaHelperVarIf<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    if getBool() {
      return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    } else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnGenericInOutSendingDirectlyElse<T>(_ x: inout sending T) -> T {
    if getBool() {
      print(x)
      fatalError()
    } else {
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnGenericInOutSendingRegionLetElse<T>(_ x: inout sending T) -> T {
    let y = x
    if getBool() {
      print(y)
      fatalError()
    } else {
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnGenericInOutSendingRegionVarElse<T>(_ x: inout sending T, z: T) -> T {
    var y = x
    y = x
    if getBool() {
      print(y)
      return z
    } else {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnGenericInOutSendingViaHelperElse<T>(_ x: inout sending T, _ z: T) -> T {
    let y = x
    if getBool() {
      print(y)
      return z
    } else {
      return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnGenericInOutSendingHelperDirectElse<T>(_ x: inout sending T, _ z: T) -> T {
    if getBool() {
      print(x)
      return z
    } else {
      return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnGenericInOutSendingViaHelperVarElse<T>(_ x: inout sending T, _ z: T) -> T {
    var y = x
    y = x
    if getBool() {
      print(y)
      return z
    } else {
      return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
      // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
  }

  func returnGenericInOutSendingDirectlyFor<T>(_ x: inout sending T, _ z: T) -> T {
    for _ in 0..<1 {
      if getBool() {
        return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
        // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    print(x)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingDirectlyFor2<T>(_ x: inout sending T, _ z: T) -> T {
    for _ in 0..<1 {
      if getBool() {
        return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
        // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    print(x)
    return z
  }

  func returnGenericInOutSendingRegionLetFor<T>(_ x: inout sending T) -> T {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingRegionLetFor2<T>(_ x: inout sending T) -> T {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingRegionLetFor3<T>(_ x: inout sending T, _ z: T) -> T {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return z
  }

  func returnGenericInOutSendingRegionVarFor<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingRegionVarFor2<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingRegionVarFor3<T>(_ x: inout sending T, _ z: T) -> T {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return y // expected-warning {{'y' cannot be returned}}
        // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return z
  }

  func returnGenericInOutSendingViaHelperFor<T>(_ x: inout sending T) -> T {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    print(y)
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingViaHelperFor2<T>(_ x: inout sending T) -> T {
    let y = x
    for _ in 0..<1 {
      if getBool() {
        return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingHelperDirectFor<T>(_ x: inout sending T) -> T {
    for _ in 0..<1 {
      if getBool() {
        return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingViaHelperVarFor<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    for _ in 0..<1 {
      if getBool() {
        return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
        // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
      }
    }
    print(y)
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingDirectlyGuard<T>(_ x: inout sending T) -> T {
    guard getBool() else {
      print(x)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingRegionLetGuard<T>(_ x: inout sending T) -> T {
    let y = x
    guard getBool() else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingRegionVarGuard<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    guard getBool() else {
      print(y)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    return y // expected-warning {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingViaHelperGuard<T>(_ x: inout sending T) -> T {
    let y = x
    guard getBool() else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingHelperDirectGuard<T>(_ x: inout sending T) -> T {
    guard getBool() else {
      print(x)
      return x // expected-warning {{'inout sending' parameter 'x' cannot be returned}}
      // expected-note @-1 {{returning 'x' risks concurrent access as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    return useValueAndReturnGeneric(x) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }

  func returnGenericInOutSendingViaHelperVarGuard<T>(_ x: inout sending T) -> T {
    var y = x
    y = x
    guard getBool() else {
      print(y)
      return y // expected-warning {{'y' cannot be returned}}
      // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
    }
    return useValueAndReturnGeneric(y) // expected-warning {{result of global function 'useValueAndReturnGeneric' cannot be returned}}
    // expected-note @-1 {{returning result of global function 'useValueAndReturnGeneric' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' is not actor-isolated and result is main actor-isolated}}
  }
}
