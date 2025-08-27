// RUN: %target-swift-frontend -swift-version 6 %s -verify -c -Xllvm -sil-regionbasedisolation-emit-isolation-history

////////////////////////
// MARK: Declarations //
////////////////////////

class NS {}

@MainActor func sendToMainConcrete(_ t: NS) async {}
nonisolated func sendToNonisolatedConcrete(_ t: NS) async {}

@MainActor func sendToMainGeneric<T>(_ t: T) async {}
nonisolated func sendToNonisolatedGeneric<T>(_ t: T) async {}

@MainActor func sendToMainConcreteOptional(_ t: NS?) async {}

func getBool() -> Bool { false }

func merge<U, V>(_ x: U, _ y: V) {}

struct NonSendableStruct {
    var value: Int
    var ref: NS?
}

enum NonSendableEnum {
    case empty
    case withValue(Int)
    case withRef(NS)
}

/////////////////
// MARK: Tests //
/////////////////

func simpleChainConcreteLet(_ x: NS) async {
    let y = x // expected-note {{'x' merged with 'y'}}
    let z = y // expected-note {{'y' merged with 'z'}}
    await sendToMainConcrete(z) // expected-error {{sending 'z' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'z' to main actor-isolated global function 'sendToMainConcrete' risks causing data races between main actor-isolated and task-isolated uses}}
}

func simpleChainGenericLet(_ x: NS) async {
    let y = x // expected-note {{'x' merged with 'y'}}
    let z = y // expected-note {{'y' merged with 'z'}}
    await sendToMainGeneric(z) // expected-error {{sending 'z' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'z' to main actor-isolated global function 'sendToMainGeneric' risks causing data races between main actor-isolated and task-isolated uses}}
    // TODO: This is due to the temporary I think. Need to put in something that suppresses this.
    // expected-note @-3 {{'z' merged with 'z'}}
}

func simpleChainConcreteVar(_ x: NS) async {
    var y = x // expected-note {{'x' merged with 'y'}}
    y = x
    var z = y // expected-note {{'y' merged with 'z'}}
    z = y
    await sendToMainConcrete(z) // expected-error {{sending 'z' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'z' to main actor-isolated global function 'sendToMainConcrete' risks causing data races between main actor-isolated and task-isolated uses}}
}

func simpleChainGenericVar(_ x: NS) async {
    var y = x // expected-note {{'x' merged with 'y'}}
    y = x
    var z = y // expected-note {{'y' merged with 'z'}}
    z = y
    await sendToMainGeneric(z) // expected-error {{sending 'z' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'z' to main actor-isolated global function 'sendToMainGeneric' risks causing data races between main actor-isolated and task-isolated uses}}
    // TODO: This is due to the temporary I think. Need to put in something that suppresses this.
    // expected-note @-3 {{'z' merged with 'z'}}
}

func simpleChainConcreteGenericLet<T>(_ x: T) async {
    let y = x // expected-note {{'x' merged with 'y'}}
    let z = y // expected-note {{'y' merged with 'z'}}
    await sendToMainGeneric(z) // expected-error {{sending 'z' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'z' to main actor-isolated global function 'sendToMainGeneric' risks causing data races between main actor-isolated and task-isolated uses}}
}

func simpleChainGenericGenericLet<T>(_ x: T) async {
    let y = x // expected-note {{'x' merged with 'y'}}
    let z = y // expected-note {{'y' merged with 'z'}}
    await sendToMainGeneric(z) // expected-error {{sending 'z' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'z' to main actor-isolated global function 'sendToMainGeneric' risks causing data races between main actor-isolated and task-isolated uses}}
}

func simpleChainConcreteGenericVar<T>(_ x: T) async {
    var y = x // expected-note {{'x' merged with 'y'}}
    y = x
    var z = y // expected-note {{'y' merged with 'z'}}
    z = y
    await sendToMainGeneric(z) // expected-error {{sending 'z' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'z' to main actor-isolated global function 'sendToMainGeneric' risks causing data races between main actor-isolated and task-isolated uses}}
    // TODO: This is due to the temporary I think. Need to put in something that suppresses this.
    // expected-note @-3 {{'z' merged with 'z'}}
}

func simpleChainGenericGenericVar<T>(_ x: T) async {
    var y = x // expected-note {{'x' merged with 'y'}}
    y = x
    var z = y // expected-note {{'y' merged with 'z'}}
    z = y
    await sendToMainGeneric(z) // expected-error {{sending 'z' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'z' to main actor-isolated global function 'sendToMainGeneric' risks causing data races between main actor-isolated and task-isolated uses}}
    // TODO: This is due to the temporary I think. Need to put in something that suppresses this.
    // expected-note @-3 {{'z' merged with 'z'}}
}

///////////////////////
// MARK: Actor Tests //
///////////////////////

actor TestActor {
    func simpleChainConcreteLet(_ x: NS) async {
        let y = x // expected-note {{'x' merged with 'y'}}
        let z = y // expected-note {{'y' merged with 'z'}}
        await sendToMainConcrete(z) // expected-error {{sending 'z' risks causing data races}}
        // expected-note @-1 {{sending 'self'-isolated 'z' to main actor-isolated global function 'sendToMainConcrete' risks causing data races between main actor-isolated and 'self'-isolated uses}}
    }

    func simpleChainGenericLet(_ x: NS) async {
        let y = x // expected-note {{'x' merged with 'y'}}
        let z = y // expected-note {{'y' merged with 'z'}}
        await sendToMainGeneric(z) // expected-error {{sending 'z' risks causing data races}}
        // expected-note @-1 {{sending 'self'-isolated 'z' to main actor-isolated global function 'sendToMainGeneric' risks causing data races between main actor-isolated and 'self'-isolated uses}}
        // TODO: This is due to the temporary I think. Need to put in something that suppresses this.
        // expected-note @-3 {{'z' merged with 'z'}}
    }

    func simpleChainConcreteVar(_ x: NS) async {
        var y = x // expected-note {{'x' merged with 'y'}}
        y = x
        var z = y // expected-note {{'y' merged with 'z'}}
        z = y
        await sendToMainConcrete(z) // expected-error {{sending 'z' risks causing data races}}
        // expected-note @-1 {{sending 'self'-isolated 'z' to main actor-isolated global function 'sendToMainConcrete' risks causing data races between main actor-isolated and 'self'-isolated uses}}
    }

    func simpleChainGenericVar(_ x: NS) async {
        var y = x // expected-note {{'x' merged with 'y'}}
        y = x
        var z = y // expected-note {{'y' merged with 'z'}}
        z = y
        await sendToMainGeneric(z) // expected-error {{sending 'z' risks causing data races}}
        // expected-note @-1 {{sending 'self'-isolated 'z' to main actor-isolated global function 'sendToMainGeneric' risks causing data races between main actor-isolated and 'self'-isolated uses}}
        // TODO: This is due to the temporary I think. Need to put in something that suppresses this.
        // expected-note @-3 {{'z' merged with 'z'}}
    }

    func simpleChainConcreteGenericLet<T>(_ x: T) async {
        let y = x // expected-note {{'x' merged with 'y'}}
        let z = y // expected-note {{'y' merged with 'z'}}
        await sendToMainGeneric(z) // expected-error {{sending 'z' risks causing data races}}
        // expected-note @-1 {{sending 'self'-isolated 'z' to main actor-isolated global function 'sendToMainGeneric' risks causing data races between main actor-isolated and 'self'-isolated uses}}
    }

    func simpleChainGenericGenericLet<T>(_ x: T) async {
        let y = x // expected-note {{'x' merged with 'y'}}
        let z = y // expected-note {{'y' merged with 'z'}}
        await sendToMainGeneric(z) // expected-error {{sending 'z' risks causing data races}}
        // expected-note @-1 {{sending 'self'-isolated 'z' to main actor-isolated global function 'sendToMainGeneric' risks causing data races between main actor-isolated and 'self'-isolated uses}}
    }

    func simpleChainConcreteGenericVar<T>(_ x: T) async {
        var y = x // expected-note {{'x' merged with 'y'}}
        y = x
        var z = y // expected-note {{'y' merged with 'z'}}
        z = y
        await sendToMainGeneric(z) // expected-error {{sending 'z' risks causing data races}}
        // expected-note @-1 {{sending 'self'-isolated 'z' to main actor-isolated global function 'sendToMainGeneric' risks causing data races between main actor-isolated and 'self'-isolated uses}}
        // TODO: This is due to the temporary I think. Need to put in something that suppresses this.
        // expected-note @-3 {{'z' merged with 'z'}}
    }

    func simpleChainGenericGenericVar<T>(_ x: T) async {
        var y = x // expected-note {{'x' merged with 'y'}}
        y = x
        var z = y // expected-note {{'y' merged with 'z'}}
        z = y
        await sendToMainGeneric(z) // expected-error {{sending 'z' risks causing data races}}
        // expected-note @-1 {{sending 'self'-isolated 'z' to main actor-isolated global function 'sendToMainGeneric' risks causing data races between main actor-isolated and 'self'-isolated uses}}
        // TODO: This is due to the temporary I think. Need to put in something that suppresses this.
        // expected-note @-3 {{'z' merged with 'z'}}
    }
}

//////////////////////
// MARK: More Tests //
//////////////////////

// TODO: Once we represent enum as a look through, we should get the appropriate
// errors here.
//
// TODO: We need to distinguish in between x, x2. We need to have a notion that
// two regions are both isolated in the same way but are different. This means
// that we can merge them... but we can distinguish the elements. That is why we
// emit a diagnostic for x instead of x2 here.
func testConditionalAssignment(_ x: NS, _ x2: NS) async {
    var z: NS? = nil
    var y: NS = x // expected-note {{'x' merged with 'y'}}
    if getBool() {
        y = x2
        z = y // expected-note {{'y.some' merged with 'z'}}
        // expected-note @-1 {{'y' merged with 'y.some'}}
    }
    await sendToMainConcreteOptional(z) // expected-error {{sending 'z' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'z' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
}


func testOptionalVarAssignment(_ x: NS, _ x2: NS) async {
    var z: NS? = nil
    var y: NS? = x // expected-note {{'x.some' merged with 'y'}}
    // expected-note @-1 {{'x' merged with 'x.some'}}
    if getBool() {
        y = x2
        z = y // expected-note {{'y' merged with 'z'}}
    }
    await sendToMainConcreteOptional(z) // expected-error {{sending 'z' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'z' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
}

func testBranchedVarAssignment(_ x: NS, _ x2: NS) async {
    var z: NS? = nil
    var y: NS? = x // expected-note {{'x.some' merged with 'y'}}
    // expected-note @-1 {{'x' merged with 'x.some'}}
    if getBool() {
        y = x2
        z = y // expected-note {{'y' merged with 'z'}}
    } else {
        y = x
        z = y
    }
    await sendToMainConcreteOptional(z) // expected-error {{sending 'z' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'z' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
}

// NOTE: This example shows that we need to have some notion of different
// regions that are all in the same isolation domain... but are not the same
// region. We allow for them to be merged... but we do not consider them to be
// the same region. This will allow for us to distinguish in between x2 and
// x. Today they are in the same region so when we see the assignment to x2, we
// see an assignment that has both elements within the same region.
//
// If one changes y to be assigned initially to NS(), then we emit the correct
// diagnostic.
func testDirectVarAssignment(_ x: NS, _ x2: NS) async {
    var z: NS? = nil
    var y: NS? = x // expected-note {{'x' merged with 'x.some'}}
    // expected-note @-1 {{'x.some' merged with 'y'}}
    y = x2
    z = y // expected-note {{'y' merged with 'z'}}
    await sendToMainConcreteOptional(z) // expected-error {{sending 'z' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'z' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    }

func testMergeFromDifferentRegionsWithVar(_ x: NS) async {
    var y = NS()
    y = NS()
    var yhat = (y, y) // expected-note {{'y' merged with 'yhat'}}
    yhat = (y, y)
    var z = NS()
    z = NS()
    var zhat = (z, z)
    zhat = (z, z)

    if getBool() {
        zhat.0 = x // expected-note {{'x' merged with 'zhat'}}
    } else {
    }

    // Should be split into (y, yhat), (z, zhat)
    merge(yhat, zhat) // expected-note {{'zhat' merged with 'yhat'}}

    await sendToMainConcreteOptional(y) // expected-error {{sending 'y.some' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'y.some' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'y' merged with 'y.some'}}
}

func testMergeFromDifferentRegionsWithVar2(_ x: NS) async {
    var y = NS()
    y = NS()
    var yhat = (y, y) // expected-note {{'y' merged with 'yhat'}}
    yhat = (y, y)
    var z = NS()
    z = NS()
    var zhat = (z, z)
    zhat = (z, z)

    // TODO: We need to say here that zhat and x are merged here since that is
    // where x's merge comes from.
    if getBool() {
        zhat.0 = x
    }
    // Should be split into (y, yhat), (z, zhat)
    merge(yhat, zhat) // expected-note {{'zhat' merged with 'yhat'}}

    await sendToMainConcreteOptional(y) // expected-error {{sending 'y.some' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'y.some' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'y' merged with 'y.some'}}
}

func testMergeFromDifferentRegionsVariant2WithVar(_ x: NS) async {
    var y = NS()
    y = NS()
    var yhat = (y, y) // expected-note {{'y' merged with 'yhat'}}
    yhat = (y, y)
    var z = NS()
    z = NS()
    var zhat = (z, z)
    zhat = (z, z)

    if getBool() {
        zhat.0 = x
    }

    // Should be split into (y, yhat), (z, zhat)
    merge(yhat, zhat) // expected-note {{'zhat' merged with 'yhat'}}

    await sendToMainConcreteOptional(y) // expected-error {{sending 'y.some' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'y.some' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'y' merged with 'y.some'}}
}

// TODO: We should have a special diagnostic for 'x' passed to initializer.
// TODO: We should be placing the note on the second assignment to s1.
func testStructAssignmentWithVar(_ x: NS, _ y: NS) async {
    var s1 = NonSendableStruct(value: 1, ref: x) // expected-note {{'x' merged with 'x.some'}}
    s1 = NonSendableStruct(value: 1, ref: x)
    var s2 = NonSendableStruct(value: 2, ref: y)
    s2 = NonSendableStruct(value: 2, ref: y)
    s1 = s2
    await sendToMainConcreteOptional(s1.ref) // expected-error {{sending 's1.ref' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 's1.ref' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
}

// Another example of where we need to 
func testEnumAssignmentWithVar(_ x: NS, _ y: NS) async {
    var e1: NonSendableEnum = .withRef(x) // expected-note {{'x.withRef' merged with 'e1'}}
    // expected-note @-1 {{'x' merged with 'x.withRef'}}
    e1 = .withRef(x)
    var e2: NonSendableEnum = .withRef(y)
    e2 = .withRef(y)
    e1 = e2
    // TODO: We are most likely emitting 'ref' merged with 'ref' due to alloc_stack reabstraction.
    // we shouldn't do so.
    if case .withRef(let ref) = e1 { // expected-note {{'ref' merged with 'ref'}}
        // expected-note @-1 {{'e1' merged with 'ref'}}
        await sendToMainConcreteOptional(ref) // expected-error {{sending 'ref.some' risks causing data races}}
        // expected-note @-1 {{sending task-isolated 'ref.some' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
        // expected-note @-2 {{'ref' merged with 'ref.some'}}
    }
}

// TODO: We do not emit any isolation history here. We should emit special
// diagnostics perhaps for Array.
func testArrayElementsWithVar(_ x: NS, _ y: NS) async {
    var arr1 = [x, NS()]
    arr1 = [x, NS()]
    var arr2 = [y, NS()]
    arr2 = [y, NS()]
    arr1[0] = arr2[0]
    await sendToMainConcreteOptional(arr1[0]) // expected-error {{sending value of non-Sendable type 'NS' risks causing data races}}
    // expected-note @-1 {{sending task-isolated value of non-Sendable type 'NS' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing races in between task-isolated and main actor-isolated uses}}
}

func testTupleAssignmentWithVar(_ w: NS, _ x: NS, _ y: NS, _ z: NS) async {
    var tuple1 = (w, x) // expected-note {{'w' merged with 'tuple1'}}
    tuple1 = (w, x)
    var tuple2 = (y, z)
    tuple2 = (y, z)
    tuple1 = tuple2
    await sendToMainConcreteOptional(tuple1.0) // expected-error {{sending 'tuple1.0.some' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'tuple1.0.some' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'tuple1' merged with 'tuple1.0.some'}}
}

func testNestedStructsWithVar(_ x: NS, _ y: NS) async {
    struct Outer {
        var inner: NonSendableStruct
    }
    var outer1 = Outer(inner: NonSendableStruct(value: 1, ref: x)) // expected-note {{'x' merged with 'x.some'}}
    outer1 = Outer(inner: NonSendableStruct(value: 1, ref: x))
    var outer2 = Outer(inner: NonSendableStruct(value: 2, ref: y))
    outer2 = Outer(inner: NonSendableStruct(value: 2, ref: y))
    outer1.inner = outer2.inner
    await sendToMainConcreteOptional(outer1.inner.ref) // expected-error {{sending 'outer1.inner.ref' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'outer1.inner.ref' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
}

func testClosureCaptureWithVar(_ x: NS, _ y: NS) async {
    var captured = x // expected-note {{'x' merged with 'captured'}}
    let closure = {
        captured = y
        _ = captured
    }
    closure()
    await sendToMainConcreteOptional(captured) // expected-error {{sending 'captured.some' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'captured.some' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'captured' merged with 'captured.some'}}
}

func testClosureCaptureWithVar2(_ x: NS, _ y: NS) async {
    var captured = x // expected-note {{'x' merged with 'captured'}}
    let closure = { // expected-note {{'closure' merged with 'closure'}}
    // expected-note @-1 {{'captured' merged with 'closure'}}
        captured = y
        _ = captured
    }
    closure()
    await sendToMainGeneric(closure) // expected-error {{sending 'closure' risks causing data races}}
    // expected-note @-1 {{'closure' merged with 'closure'}}
    // expected-note @-2 {{sending task-isolated 'closure' to main actor-isolated global function 'sendToMainGeneric' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-3 {{'closure' merged with 'closure'}}
}

func testParameterPassing(_ a: NS, _ b: NS, _ c: NS) async {
    func helper(_ first: NS, _ second: NS) {
        merge(first, second)
    }
    helper(a, b)
    helper(b, c)
    await sendToMainConcreteOptional(a) // expected-error {{sending 'a.some' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'a.some' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'a' merged with 'a.some'}}
}

func testOptionalChainingWithVar(_ x: NS?) async {
    var container: NonSendableStruct? = NonSendableStruct(value: 0, ref: nil)
    container?.ref = x // expected-note {{'x' merged with 'container'}}
    await sendToMainConcreteOptional(container?.ref) // expected-error {{sending 'container.some.ref' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'container.some.ref' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'container' merged with 'container.some.ref'}}
}

func testPropertyAssignmentWithVar(_ x: NS, _ y: NS) async {
    class Container {
        var stored: NS?
        var computed: NS? {
            get { stored }
            set { stored = newValue }
        }
    }
    var c1 = Container()
    c1 = Container()
    var c2 = Container()
    c2 = Container()
    c1.stored = x // expected-note {{'x.some' merged with 'c1'}}
    // expected-note @-1 {{'x' merged with 'x.some'}}
    c2.stored = y
    c1.computed = c2.computed
    await sendToMainConcreteOptional(c1.stored) // expected-error {{sending 'c1.stored' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'c1.stored' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'c1' merged with 'c1.stored'}}
}

/*
 We crash here
func testWhileLoopWithVar(_ values: [NS]) async {
    var current: NS? = nil
    var index = 0
    while index < values.count {
        current = values[index]
        index += 1
    }
    await sendToMainConcreteOptional(current)
    }

func testForInLoopWithVar(_ values: [NS]) async {
    var last: NS? = nil
    for value in values {
        last = value
    }
    await sendToMainConcreteOptional(last)
}

 */


func testSwitchStatementWithVar(_ x: NS, _ y: NS, _ z: NS) async {
    enum Choice { case first, second, third }
    let choice = Choice.first
    var result: NS
    switch choice { // expected-warning {{switch condition evaluates to a constant}}
    case .first:
        result = x // expected-note {{'x' merged with 'result'}}
    case .second:
        result = y // expected-note {{will never be executed}}
    case .third:
        result = z
    }
    await sendToMainConcreteOptional(result) // expected-error {{sending 'result.some' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'result.some' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'result' merged with 'result.some'}}
}

func testNestedIfWithVar(_ a: NS, _ b: NS, _ c: NS, _ d: NS) async {
    var result: NS? = nil
    if getBool() {
        if getBool() {
            result = a // expected-note {{'a.some' merged with 'result'}}
            // expected-note @-1 {{'a' merged with 'a.some'}}
        } else {
            result = b
        }
    } else {
        if getBool() {
            result = c
        } else {
            result = d
        }
    }
    await sendToMainConcreteOptional(result) // expected-error {{sending 'result' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'result' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
}

func testGuardStatementWithVar(_ input: NS?) async {
    guard let unwrapped = input else { // expected-note {{'unwrapped' merged with 'unwrapped'}}
    // expected-note @-1 {{'input' merged with 'unwrapped'}}
        await sendToMainConcreteOptional(nil)
        return
    }
    var stored = unwrapped // expected-note {{'unwrapped' merged with 'stored'}}
    stored = unwrapped
    _ = stored
    await sendToMainConcreteOptional(stored) // expected-error {{sending 'stored.some' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'stored.some' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'stored' merged with 'stored.some'}}
}

func testDoTryCatchWithVar(_ x: NS, _ y: NS) async {
    enum TestError: Error { case failed }
    func mayThrow() throws -> Bool { throw TestError.failed }

    var result: NS
    do {
        let success = try mayThrow()
        result = success ? x : y
    } catch {
        result = y // expected-note {{'y' merged with 'result'}}
    }
    await sendToMainConcreteOptional(result) // expected-error {{sending 'result.some' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'result.some' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'result' merged with 'result.some'}}
}

func testDeferredAssignmentWithVar(_ x: NS, _ y: NS) async {
    var result = x // expected-note {{'x' merged with 'result'}}
    defer {
        result = y
        _ = result
    }
    await sendToMainConcreteOptional(result) // expected-error {{sending 'result.some' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'result.some' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'result' merged with 'result.some'}}
}

/*
func testRepeatWhileWithVar(_ values: [NS]) async {
    var current: NS? = nil
    var index = 0
    repeat {
        if index < values.count {
            current = values[index]
        }
        index += 1
    } while index < 3
    await sendToMainConcreteOptional(current)
    }
 */

func testFallthroughWithVar(_ x: NS, _ y: NS, _ z: NS) async {
    let value = 1
    var result: NS? = nil
    switch value { // expected-note {{condition always evaluates to true}}
    case 1:
        result = x
        fallthrough
    case 2:
        if result == nil {
            result = y
        }
    default:
        if result == nil { // expected-warning {{will never be executed}}
            result = z // expected-note {{'z.some' merged with 'result'}}
            // expected-note @-1 {{'z' merged with 'z.some'}}
        }
    }
    await sendToMainConcreteOptional(result) // expected-error {{sending 'result' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'result' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
}

func testTernaryOperatorWithVar(_ condition: Bool, _ x: NS, _ y: NS) async {
    var result = NS()
    result = condition ? x : y // expected-note {{'y' merged with 'result'}}
    // expected-note @-1 {{'x' merged with 'y'}}
    _ = result
    await sendToMainConcreteOptional(result) // expected-error {{sending 'result.some' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'result.some' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'result' merged with 'result.some'}}
}

/*
func testBreakContinueWithVar(_ values: [NS?]) async {
    var found: NS? = nil
    for value in values {
        guard let unwrapped = value else { continue }
        found = unwrapped
        break
    }
    await sendToMainConcreteOptional(found)
    }

func testComplexMixedScenarioWithVar(_ a: NS, _ b: NS, _ c: NS) async {
    struct ComplexContainer {
        var items: [NS?]
        var selected: NS?
    }
    var container = ComplexContainer(items: [a, nil, b], selected: nil)

    for i in 0..<container.items.count {
        if let item = container.items[i] {
            container.selected = item
            if getBool() {
                container.items[i] = c
                break
            }
        }
    }

    let finalChoice = container.selected ?? c
    await sendToMainConcreteOptional(finalChoice)
}
 */

func testMixedTypesWithClosuresWithVar(_ x: NS, _ y: NS) async {
    var dict: [String: NS] = ["first": x]
    var arr: [NS] = [y]

    let processingClosure = { (key: String, value: NS) in
        dict[key] = value
        arr.append(value)
    }

    processingClosure("second", y)

    if let first = dict["first"] {
        arr[0] = first
    }

    await sendToMainConcreteOptional(arr.first) // expected-error {{sending value of non-Sendable type 'NS?' risks causing data races}}
    // expected-note @-1 {{sending task-isolated value of non-Sendable type 'NS?' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing races in between task-isolated and main actor-isolated uses}}
}

func testEnumSwitchWithAssociatedValuesWithVar(_ inputs: [NonSendableEnum]) async {
    var accumulated: [NS] = []
    var lastRef: NS? = nil

    for enumValue in inputs {
        switch enumValue {
        case .empty:
            continue
        case .withValue(let int):
            if int > 0, let ref = lastRef {
                accumulated.append(ref)
            }
        case .withRef(let ref):
            lastRef = ref
            accumulated.append(ref)
        }
    }

    await sendToMainConcreteOptional(accumulated.first) // expected-error {{sending value of non-Sendable type 'NS?' risks causing data races}}
    // expected-note @-1 {{sending task-isolated value of non-Sendable type 'NS?' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing races in between task-isolated and main actor-isolated uses}}
}

func testNestedStructsWithControlFlowWithVar(_ p1: NS, _ p2: NS, _ p3: NS) async {
    struct Level1 {
        var level2: Level2
    }
    struct Level2 {
        var level3: Level3
    }
    struct Level3 {
        var values: (NS?, NS?)
    }

    var nested = Level1(level2: Level2(level3: Level3(values: (p1, nil)))) // expected-note {{'p1' merged with 'p1.some'}}

    if getBool() {
        nested.level2.level3.values.1 = p2
        if getBool() {
            nested.level2.level3.values = (p3, nested.level2.level3.values.0)
        }
    } else {
        nested.level2.level3.values.0 = p3
    }

    await sendToMainConcreteOptional(nested.level2.level3.values.0) // expected-error {{sending 'nested.level2.level3.values.0' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'nested.level2.level3.values.0' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
}

func testMultipleParameterInteractionsWithVar(_ a: NS, _ b: NS, _ c: NS, _ d: NS) async {
    func combineThree(_ first: NS, _ second: NS, _ third: NS) -> NS {
        merge(first, second)
        merge(second, third)
        return first
    }

    var result1 = NS()
    var result2 = NS()
    result1 = combineThree(a, b, c)
    _ = result1
    result2 = combineThree(b, c, d)
    _ = result2

    merge(result1, result2)
    await sendToMainConcreteOptional(result1) // expected-error {{sending 'result1.some' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'result1.some' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'result1' merged with 'result1.some'}}
}

func testOptionalChainingWithLoopsWithVar(_ containers: [NonSendableStruct?], _ replacement: NS) async {
    var modified: [NonSendableStruct?] = containers

    for i in 0..<modified.count {
        if modified[i]?.ref == nil {
            modified[i]?.ref = replacement
        } else {
            guard let existing = modified[i]?.ref else { continue }
            merge(existing, replacement)
        }
    }

    await sendToMainConcreteOptional(modified.first??.ref) // expected-error {{sending value of non-Sendable type 'NS?' risks causing data races}}
    // expected-note @-1 {{sending task-isolated value of non-Sendable type 'NS?' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing races in between task-isolated and main actor-isolated uses}}
}

func testAsyncClosureCapturesWithVar(_ external1: NS, _ external2: NS) async {
    var captured1 = external1 // expected-note {{'external1' merged with 'captured1'}}
    var captured2 = external2

    let asyncClosure = { @MainActor in
        captured1 = captured2
        captured2 = external1 // expected-error {{sending 'external1' risks causing data races}}
        _ = captured1 // expected-error {{sending 'captured1' risks causing data races}}
        _ = captured2 // expected-error {{sending 'captured2' risks causing data races}}
        // expected-note @-1 {{task-isolated 'captured2' is captured by a caller isolation inheriting-isolated closure. caller isolation inheriting-isolated uses in closure may race against later nonisolated uses}}
        // expected-note @-3 {{task-isolated 'captured1' is captured by a caller isolation inheriting-isolated closure. caller isolation inheriting-isolated uses in closure may race against later nonisolated uses}}
        // expected-note @-5 {{task-isolated 'external1' is captured by a caller isolation inheriting-isolated closure. caller isolation inheriting-isolated uses in closure may race against later nonisolated uses}}
    }

    await asyncClosure()
    await sendToMainConcreteOptional(captured1) // expected-error {{sending 'captured1.some' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'captured1.some' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'captured1' merged with 'captured1.some'}}
}

func testComplexTupleManipulationWithVar(_ v1: NS, _ v2: NS, _ v3: NS, _ v4: NS) async {
    var tuple1 = (first: v1, second: (nested: v2, other: v3)) // expected-note {{'v1' merged with 'tuple1'}}
    var tuple2 = (first: v4, second: (nested: v1, other: v2))

    if getBool() {
        tuple1.second = tuple2.second
    } else {
        tuple2.first = tuple1.first
        tuple1.first = tuple2.second.nested
    }

    let combined = (tuple1, tuple2) // expected-note {{'combined' merged with 'combined'}}
    await sendToMainConcreteOptional(combined.0.first) // expected-error {{sending 'combined.0.0.some' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'combined.0.0.some' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'combined' merged with 'combined.0.0.some'}}
}

func testSwitchStatementWithLet(_ x: NS, _ y: NS, _ z: NS) async {
    enum Choice { case first, second, third }
    let choice = Choice.first
    let result: NS
    switch choice { // expected-warning {{switch condition evaluates to a constant}}
    case .first:
        result = x // expected-note {{'x' merged with 'result'}}
    case .second:
        result = y // expected-note {{will never be executed}}
    case .third:
        result = z
    }
    await sendToMainConcreteOptional(result) // expected-error {{sending 'result.some' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'result.some' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'result' merged with 'result.some'}}
}

func testDoTryCatchWithLet(_ x: NS, _ y: NS) async {
    enum TestError: Error { case failed }
    func mayThrow() throws -> Bool { throw TestError.failed }

    let result: NS
    do {
        let success = try mayThrow()
        result = success ? x : y
    } catch {
        result = y // expected-note {{'y' merged with 'result'}}
    }
    await sendToMainConcreteOptional(result) // expected-error {{sending 'result.some' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'result.some' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'result' merged with 'result.some'}}
}

func testTernaryOperatorWithLet(_ condition: Bool, _ x: NS, _ y: NS) async {
    let result = condition ? x : y // expected-note {{'result' merged with 'result'}}
    await sendToMainConcreteOptional(result) // expected-error {{sending 'result.some' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'result.some' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'result' merged with 'result.some'}}
    // expected-note @-4 {{'x' merged with 'result'}}
}

func testComplexTupleManipulationWithLet(_ v1: NS, _ v2: NS, _ v3: NS, _ v4: NS) async {
    let tuple1 = (first: v1, second: (nested: v2, other: v3)) // expected-note {{'v1' merged with 'tuple1'}}
    // expected-note @-1 {{'tuple1' merged with 'tuple1'}}
    let tuple2 = (first: v4, second: (nested: v1, other: v2))
    let combined = (tuple1, tuple2) // expected-note {{'combined' merged with 'combined'}}
    await sendToMainConcreteOptional(combined.0.first) // expected-error {{sending 'combined.0.0.some' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'combined.0.0.some' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'combined' merged with 'combined.0.0.some'}}
}

///////////////////////////////
// MARK: Generic Actor Tests //
///////////////////////////////

actor GenericActor<T> {
    func testGenericChain(_ x: T) async {
        let y = x // expected-note {{'x' merged with 'y'}}
        let z = y // expected-note {{'y' merged with 'z'}}
        await sendToMainGeneric(z) // expected-error {{sending 'z' risks causing data races}}
        // expected-note @-1 {{sending 'self'-isolated 'z' to main actor-isolated global function 'sendToMainGeneric' risks causing data races between main actor-isolated and 'self'-isolated uses}}
    }

    func testGenericVar(_ x: T) async {
        var y = x // expected-note {{'x' merged with 'y'}}
        y = x
        var z = y // expected-note {{'y' merged with 'z'}}
        z = y
        await sendToMainGeneric(z) // expected-error {{sending 'z' risks causing data races}}
        // expected-note @-1 {{sending 'self'-isolated 'z' to main actor-isolated global function 'sendToMainGeneric' risks causing data races between main actor-isolated and 'self'-isolated uses}}
        // expected-note @-2 {{'z' merged with 'z'}}
    }
}

/////////////////////////////
// MARK: Class Inheritance //
/////////////////////////////

class BaseNS {
    var baseValue: NS?
}

class DerivedNS: BaseNS {
    var derivedValue: NS?
}

func testClassInheritance(_ x: NS, _ y: NS) async {
    var base = BaseNS()
    base = BaseNS()
    var derived = DerivedNS()
    derived = DerivedNS()
    base.baseValue = x
    derived.baseValue = y // expected-note {{'y.some' merged with 'derived'}}
    // expected-note @-1 {{'y' merged with 'y.some'}}
    derived.derivedValue = base.baseValue
    await sendToMainConcreteOptional(derived.derivedValue) // expected-error {{sending 'derived.derivedValue' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'derived.derivedValue' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'derived' merged with 'derived.derivedValue'}}
}

func testClassCast(_ x: NS) async {
    var base: BaseNS = DerivedNS()
    base = DerivedNS()
    base.baseValue = x // expected-note {{'x.some' merged with 'base'}}
    if let derived = base as? DerivedNS { // expected-note {{'derived' merged with 'derived'}}
    // expected-note @-2 {{'x' merged with 'x.some'}}
        derived.derivedValue = base.baseValue
        await sendToMainConcreteOptional(derived.derivedValue) // expected-error {{sending 'derived.derivedValue' risks causing data races}}
        // expected-note @-1 {{sending task-isolated 'derived.derivedValue' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
        // expected-note @-2 {{'derived' merged with 'derived.derivedValue'}}
    }
}

///////////////////////////////
// MARK: Computed Properties //
///////////////////////////////

struct ComputedContainer {
    private var _stored: NS?

    var computed: NS? {
        get { _stored }
        set { _stored = newValue }
    }

    var chained: NS? {
        get { computed }
        set { computed = newValue }
    }
}

func testComputedPropertyChain(_ x: NS, _ y: NS) async {
    var container1 = ComputedContainer()
    container1 = ComputedContainer()
    var container2 = ComputedContainer()
    container2 = ComputedContainer()
    container1.computed = x // expected-note {{'x.some' merged with 'container1'}}
    // expected-note @-1 {{'x' merged with 'x.some'}}
    container2.chained = y
    container1.chained = container2.computed
    await sendToMainConcreteOptional(container1.computed) // expected-error {{sending 'container1.computed' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'container1.computed' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'container1' merged with 'container1.computed'}}
}

func testComputedPropertyIndirect(_ x: NS) async {
    var container = ComputedContainer()
    container = ComputedContainer()
    container.computed = x // expected-note {{'x.some' merged with 'container'}}
    let indirect = container.chained // expected-note {{'indirect' merged with 'indirect'}}
    await sendToMainConcreteOptional(indirect) // expected-error {{sending 'indirect' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'indirect' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-3 {{'container' merged with 'indirect'}}
    // expected-note @-5 {{'x' merged with 'x.some'}}
}

////////////////////////////
// MARK: Inout Parameters //
////////////////////////////

func modifyInout(_ value: inout NS) {
    value = NS()
}

func testInoutModification(_ x: NS) async {
    var value = x // expected-note {{'x' merged with 'value'}}
    value = x
    modifyInout(&value)
    await sendToMainConcreteOptional(value) // expected-error {{sending 'value.some' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'value.some' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'value' merged with 'value.some'}}
}

func testInoutSwap(_ x: NS, _ y: NS) async {
    var first = x // expected-note {{'x' merged with 'first'}}
    first = x
    var second = y
    second = y
    swap(&first, &second)
    await sendToMainConcreteOptional(first) // expected-error {{sending 'first.some' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'first.some' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'first' merged with 'first.some'}}
}

//////////////////////////////
// MARK: Weak/Unowned Tests //
//////////////////////////////

class WeakContainer {
    weak var weakRef: NS?
    unowned var unownedRef: NS

    init(unowned: NS) {
        self.unownedRef = unowned
    }
}

func testWeakReference(_ x: NS, _ y: NS) async {
    var container = WeakContainer(unowned: x)
    container = WeakContainer(unowned: x)
    container.weakRef = y
    let extracted = container.weakRef // expected-note {{'container' merged with 'extracted'}}
    await sendToMainConcreteOptional(extracted) // expected-error {{sending 'extracted' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'extracted' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-3 {{'extracted' merged with 'extracted'}}
}

func testUnownedReference(_ x: NS, _ y: NS) async {
    var container = WeakContainer(unowned: x)
    container = WeakContainer(unowned: x)
    container.unownedRef = y
    let extracted = container.unownedRef // expected-note {{'extracted' merged with 'extracted'}}
    await sendToMainConcreteOptional(extracted) // expected-error {{sending 'extracted.some' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'extracted.some' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'extracted' merged with 'extracted.some'}}
    // expected-note @-4 {{'container' merged with 'extracted'}}
}

/////////////////////////////////
// MARK: Static Property Tests //
/////////////////////////////////

class StaticContainer {
    @MainActor static var sharedValue: NS?
    @MainActor static var computedShared: NS? {
        get { sharedValue }
        set { sharedValue = newValue }
    }
}

@MainActor func testStaticProperty(_ x: NS, _ y: NS) async {
    StaticContainer.sharedValue = x
    var local = StaticContainer.sharedValue
    local = StaticContainer.sharedValue
    StaticContainer.computedShared = y
    local = StaticContainer.computedShared
    await sendToMainConcreteOptional(local)
}

////////////////////////////////
// MARK: Protocol Conformance //
////////////////////////////////

protocol HasValue {
    var protocolValue: NS? { get set }
}

struct ConformsToProtocol: HasValue {
    var protocolValue: NS?
}

class ClassConformsToProtocol: HasValue {
    var protocolValue: NS?
}

func testProtocolConformance(_ x: NS, _ y: NS) async {
    var conforming: HasValue = ConformsToProtocol()
    conforming = ConformsToProtocol()
    conforming.protocolValue = x // expected-note {{'x.some' merged with 'conforming'}}
    // expected-note @-1 {{'x' merged with 'x.some'}}
    var classConforming: HasValue = ClassConformsToProtocol()
    classConforming = ClassConformsToProtocol()
    classConforming.protocolValue = y
    conforming.protocolValue = classConforming.protocolValue
    await sendToMainConcreteOptional(conforming.protocolValue) // expected-error {{sending 'conforming.protocolValue' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'conforming.protocolValue' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'conforming' merged with 'conforming.protocolValue'}}
}

func testExistentialContainer(_ containers: [HasValue], _ x: NS) async {
    var mutableContainers = containers // expected-note {{'containers' merged with 'mutableContainers'}}
    mutableContainers = containers
    if mutableContainers.count > 0 {
        mutableContainers[0].protocolValue = x
        let extracted = mutableContainers[0].protocolValue // expected-note {{'extracted' merged with 'extracted'}}
        await sendToMainConcreteOptional(extracted) // expected-error {{sending 'extracted' risks causing data races}}
        // expected-note @-1 {{sending task-isolated 'extracted' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    }
}

///////////////////////////
// MARK: Subscript Tests //
///////////////////////////

struct SubscriptContainer {
    private var storage: [String: NS] = [:]

    subscript(key: String) -> NS? {
        get { storage[key] }
        set { storage[key] = newValue }
    }

    subscript(index: Int) -> NS? {
        get {
            let keys = Array(storage.keys)
            guard index < keys.count else { return nil }
            return storage[keys[index]]
        }
        set {
            let keys = Array(storage.keys)
            guard index < keys.count else { return }
            storage[keys[index]] = newValue
        }
    }
}

func testCustomSubscript(_ x: NS, _ y: NS) async {
    var container = SubscriptContainer()
    container = SubscriptContainer()
    container["first"] = x // expected-note {{'x.some' merged with 'container'}}
    // expected-note @-1 {{'x' merged with 'x.some'}}
    container["second"] = y
    container[0] = container["second"]
    let result = container["first"] // expected-note {{'container' merged with 'result'}}
    await sendToMainConcreteOptional(result) // expected-error {{sending 'result' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'result' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-3 {{'result' merged with 'result'}}
}

///////////////////////////
// MARK: Lazy Properties //
///////////////////////////

struct LazyContainer {
    lazy var lazyValue: NS = NS()
    var stored: NS?
}

func testLazyProperty(_ x: NS) async {
    var container = LazyContainer()
    container = LazyContainer()
    container.stored = x // expected-note {{'x.some' merged with 'container'}}
    var lazy = container.lazyValue // expected-warning {{variable 'lazy' was written to, but never read}}
    // expected-note @-2 {{'x' merged with 'x.some'}}
    lazy = container.lazyValue
    container.lazyValue = container.stored ?? NS()
    await sendToMainConcreteOptional(container.lazyValue) // expected-error {{sending 'container.lazyValue.some' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'container.lazyValue.some' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'container.lazyValue' merged with 'container.lazyValue.some'}}
    // expected-note @-3 {{'container' merged with 'container.lazyValue'}}
}

/////////////////////////
// MARK: KeyPath Tests //
/////////////////////////

func testKeyPathAccess(_ x: NS, _ y: NS) async {
    var container = NonSendableStruct(value: 0, ref: x) // expected-note {{'x' merged with 'x.some'}}
    container = NonSendableStruct(value: 0, ref: x)
    let keyPath = \NonSendableStruct.ref
    var extracted = container[keyPath: keyPath] // expected-note {{'container' merged with 'container'}}
    extracted = container[keyPath: keyPath]
    container[keyPath: keyPath] = y
    extracted = container[keyPath: keyPath]
    await sendToMainConcreteOptional(extracted) // expected-error {{sending 'extracted' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'extracted' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
}

///////////////////////////////
// MARK: Generic Constraints //
///////////////////////////////

func testGenericConstraint<T: AnyObject>(_ x: T, _ y: T) async {
    var first = x // expected-note {{'x' merged with 'first'}}
    first = x
    var second = y
    second = y
    first = second
    await sendToMainGeneric(first) // expected-error {{sending 'first' risks causing data races}}
    // expected-note @-1 {{sending task-isolated 'first' to main actor-isolated global function 'sendToMainGeneric' risks causing data races between main actor-isolated and task-isolated uses}}
    // expected-note @-2 {{'first' merged with 'first'}}
}

class GenericClass<T> {
    var value: T
    init(_ value: T) {
        self.value = value
    }
}

func testGenericClassChain(_ x: NS, _ y: NS) async {
    var container1 = GenericClass(x)
    container1 = GenericClass(x)
    var container2 = GenericClass(y)
    container2 = GenericClass(y)
    container1.value = container2.value
    await sendToMainConcreteOptional(container1.value) // expected-error {{sending value of non-Sendable type 'NS' risks causing data races}}
    // expected-note @-1 {{sending task-isolated value of non-Sendable type 'NS' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing races in between task-isolated and main actor-isolated uses}}
}

/////////////////////////////
// MARK: Result Type Tests //
/////////////////////////////

enum TestResult<Success, Failure: Error> {
    case success(Success)
    case failure(Failure)
}

enum TestError: Error {
    case failed
}

func testResultType(_ x: NS, _ y: NS) async {
    var result1: TestResult<NS, TestError> = .success(x)
    result1 = .success(x)
    var result2: TestResult<NS, TestError> = .success(y) // expected-note {{'y.success' merged with 'result2'}}
    // expected-note @-1 {{'y' merged with 'y.success'}}
    result2 = .success(y)

    switch result1 {
    case .success(let value):
        result2 = .success(value)
    case .failure:
        break
    }

    if case .success(let final) = result2 { // expected-note {{'result2' merged with 'final'}}
        await sendToMainConcreteOptional(final) // expected-error {{sending 'final.some' risks causing data races}}
        // expected-note @-1 {{sending task-isolated 'final.some' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing data races between main actor-isolated and task-isolated uses}}
        // expected-note @-2 {{'final' merged with 'final.some'}}
    // expected-note @-4 {{'final' merged with 'final'}}
    }
}

////////////////////////////
// MARK: Optional Binding //
////////////////////////////

func testComplexOptionalBinding(_ inputs: [NS?]) async {
    var accumulated: [NS] = []
    accumulated = []
    var current: NS? = nil

    for input in inputs {
        if let unwrapped = input {
            current = unwrapped
            if let existing = current {
                accumulated.append(existing)
            }
        }
    }

    await sendToMainConcreteOptional(accumulated.first) // expected-error {{sending value of non-Sendable type 'NS?' risks causing data races}}
    // expected-note @-1 {{sending task-isolated value of non-Sendable type 'NS?' to main actor-isolated global function 'sendToMainConcreteOptional' risks causing races in between task-isolated and main actor-isolated uses}}
}
