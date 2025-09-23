// RUN: %target-typecheck-verify-swift

let x: _ = 0 // expected-error {{placeholder type may not appear as type of a variable}} expected-note {{replace the placeholder with the inferred type 'Int'}} {{8-9=Int}}
let x2 = x
var x3: _ { // expected-error {{type placeholder not allowed here}}
  get { 42 }
}
let dict1: [_: Int] = ["hi": 0]
let dict2: [Character: _] = ["h": 0]

let arr = [_](repeating: "hi", count: 3)

func foo(_ arr: [_] = [0]) {} // expected-error {{type placeholder not allowed here}}

let foo = _.foo // expected-error {{type placeholder not allowed here}}
let zero: _ = .zero // expected-error {{cannot infer contextual base in reference to member 'zero'}}

struct S<T> {
    var x: T
}

var s1: S<_> = .init(x: 0)
var s2 = S<_>(x: 0)

let losslessStringConverter = Double.init as (String) -> _?

let optInt: _? = 0
let implicitOptInt: _! = 0

let func1: (_) -> Double = { (x: Int) in 0.0 }
let func2: (Int) -> _ = { x in 0.0 }
let func3: (_) -> _ = { (x: Int) in 0.0 }
let func4: (_, String) -> _ = { (x: Int, y: String) in 0.0 }
let func5: (_, String) -> _ = { (x: Int, y: Double) in 0.0 } // expected-error {{cannot convert value of type '(Int, Double) -> Double' to specified type '(_, String) -> _'}}

let type: _.Type = Int.self
let type2: Int.Type.Type = _.Type.self

struct MyType1<T, U> {
    init(t: T, mt2: MyType2<T>) where U == MyType2<T> {}
}

struct MyType2<T> {
    init(t: T) {}
}

let _: MyType2<_> = .init(t: "c" as Character)
let _: MyType1<_, MyType2<_>> = .init(t: "s" as Character, mt2: .init(t: "c" as Character))

func dictionary<K, V>(ofType: [K: V].Type) -> [K: V] { [:] }

let _: [String: _] = dictionary(ofType: [_: Int].self)
let _: [_: _] = dictionary(ofType: [String: Int].self)
let _: [String: Int] = dictionary(ofType: _.self) // expected-error {{type placeholder not allowed here}}

let _: @convention(c) _ = { 0 } // expected-error {{'@convention' only applies to function types}}
let _: @convention(c) (_) -> _ = { (x: Double) in 0 }
let _: @convention(c) (_) -> Int = { (x: Double) in 0 }

struct NonObjc {}

let _: @convention(c) (_) -> Int = { (x: NonObjc) in 0 } // expected-error {{'(NonObjc) -> Int' is not representable in Objective-C, so it cannot be used with '@convention(c)'}}

func overload() -> Int? { 0 }
func overload() -> String { "" }

let _: _? = overload()
let _ = overload() as _?

struct Bar<T, U>
where T: ExpressibleByIntegerLiteral, U: ExpressibleByIntegerLiteral {
    var t: T
    var u: U
    func frobnicate() -> Bar {
        return Bar(t: 42, u: 42)
    }
}

extension Bar {
  func frobnicate2() -> Bar<_, _> { // expected-error {{type placeholder not allowed here}}
    return Bar(t: 42, u: 42)
  }
  func frobnicate3() -> Bar {
    return Bar<_, _>(t: 42, u: 42)
  }
  func frobnicate4() -> Bar<_, _> { // expected-error {{type placeholder not allowed here}}
    return Bar<_, _>(t: 42, u: 42)
  }
  func frobnicate5() -> Bar<_, U> { // expected-error {{type placeholder not allowed here}}
    return Bar(t: 42, u: 42)
  }
  func frobnicate6() -> Bar {
    return Bar<_, U>(t: 42, u: 42)
  }
  func frobnicate7() -> Bar<_, _> { // expected-error {{type placeholder not allowed here}}
    return Bar<_, U>(t: 42, u: 42)
  }
  func frobnicate8() -> Bar<_, U> { // expected-error {{type placeholder not allowed here}}
    return Bar<_, _>(t: 42, u: 42)
  }
}

// FIXME: We should probably have better diagnostics for these situations--the user probably meant to use implicit member syntax
let _: Int = _() // expected-error {{type placeholder not allowed here}}
let _: () -> Int = { _() } // expected-error {{type placeholder not allowed here}}
let _: Int = _.init() // expected-error {{type placeholder not allowed here}}
let _: () -> Int = { _.init() } // expected-error {{type placeholder not allowed here}}

func returnsInt() -> Int { _() } // expected-error {{type placeholder not allowed here}}
func returnsIntClosure() -> () -> Int { { _() } } // expected-error {{type placeholder not allowed here}}
func returnsInt2() -> Int { _.init() }  // expected-error {{type placeholder not allowed here}}
func returnsIntClosure2() -> () -> Int { { _.init() } } // expected-error {{type placeholder not allowed here}}

let _: Int.Type = _ // expected-error {{'_' can only appear in a pattern or on the left side of an assignment}}
let _: Int.Type = _.self // expected-error {{type placeholder not allowed here}}

struct SomeSuperLongAndComplexType {}
func getSomething() -> SomeSuperLongAndComplexType? { .init() }
let something: _! = getSomething()

extension Array where Element == Int {
    static var staticMember: Self { [] }
    static func staticFunc() -> Self { [] }

    var member: Self { [] }
    func method() -> Self { [] }
}

extension Array {
    static var otherStaticMember: Self { [] }
}

let _ = [_].staticMember
let _ = [_].staticFunc()
let _ = [_].otherStaticMember.member
let _ = [_].otherStaticMember.method()

func f(x: Any, arr: [Int]) {
    // TODO: Maybe we should suggest replacing placeholders with 'Any'?

    if x is _ {} // expected-error {{type placeholder not allowed here}}
    if x is [_] {} // expected-error {{could not infer type for placeholder}}
    if x is () -> _ {} // expected-error {{could not infer type for placeholder}}
    if let y = x as? _ {} // expected-error {{type placeholder not allowed here}}
    if let y = x as? [_] {} // expected-error {{could not infer type for placeholder}}
    if let y = x as? () -> _ {} // expected-error {{could not infer type for placeholder}}
    let y1 = x as! _ // expected-error {{type placeholder not allowed here}}
    let y2 = x as! [_] // expected-error {{could not infer type for placeholder}}
    let y3 = x as! () -> _ // expected-error {{could not infer type for placeholder}}

    switch x {
    case is _: break // expected-error {{type placeholder not allowed here}}
    case is [_]: break // expected-error {{type placeholder not allowed here}}
    case is () -> _: break // expected-error {{type placeholder not allowed here}}
    case let y as _: break // expected-error {{type placeholder not allowed here}}
    case let y as [_]: break // expected-error {{type placeholder not allowed here}}
    case let y as () -> _: break // expected-error {{type placeholder not allowed here}}
    }

    if case is _ = x {} // expected-error {{type placeholder not allowed here}}
    if case is [_] = x {} // expected-error {{could not infer type for placeholder}}
    if case is () -> _ = x {} // expected-error {{could not infer type for placeholder}}
    if case let y as _ = x {} // expected-error {{type placeholder not allowed here}}
    if case let y as [_] = x {} // expected-error {{could not infer type for placeholder}}
    if case let y as () -> _ = x {} // expected-error {{could not infer type for placeholder}}

    if arr is _ {} // expected-error {{type placeholder not allowed here}}
    if arr is [_] {} // expected-error {{could not infer type for placeholder}}
    if arr is () -> _ {} // expected-error {{could not infer type for placeholder}}
    if let y = arr as? _ {} // expected-error {{type placeholder not allowed here}}
    if let y = arr as? [_] {} // expected-error {{could not infer type for placeholder}}
    if let y = arr as? () -> _ {} // expected-error {{could not infer type for placeholder}}
    let y1 = arr as! _ // expected-error {{type placeholder not allowed here}}
    let y2 = arr as! [_] // expected-error {{could not infer type for placeholder}}
    let y3 = arr as! () -> _ // expected-error {{could not infer type for placeholder}}

    switch arr {
    case is _: break // expected-error {{type placeholder not allowed here}}
    case is [_]: break // expected-error {{type placeholder not allowed here}}
    case is () -> _: break // expected-error {{type placeholder not allowed here}}
    case let y as _: break // expected-error {{type placeholder not allowed here}}
    case let y as [_]: break // expected-error {{type placeholder not allowed here}}
    case let y as () -> _: break // expected-error {{type placeholder not allowed here}}
    }
}

protocol Publisher {
    associatedtype Output
    associatedtype Failure
}

struct Just<Output>: Publisher {
    typealias Failure = Never
}

struct SetFailureType<Output, Failure>: Publisher {}

extension Publisher {
    func setFailureType<T>(to: T.Type) -> SetFailureType<Output, T> { // expected-note 2 {{in call to function 'setFailureType(to:)'}}
        return .init()
    }
}

let _: SetFailureType<Int, String> = Just<Int>().setFailureType(to: _.self) // expected-error {{type placeholder not allowed here}}
let _: SetFailureType<Int, [String]> = Just<Int>().setFailureType(to: [_].self)
let _: SetFailureType<Int, (String) -> Double> = Just<Int>().setFailureType(to: ((_) -> _).self)
let _: SetFailureType<Int, (String, Double)> = Just<Int>().setFailureType(to: (_, _).self)

// TODO: Better error message here? Would be nice if we could point to the placeholder...
let _: SetFailureType<Int, String> = Just<Int>().setFailureType(to: _.self).setFailureType(to: String.self) // expected-error {{type placeholder not allowed here}} expected-error {{generic parameter 'T' could not be inferred}}

let _: (_) = 0 as Int // expected-error {{placeholder type may not appear as type of a variable}} expected-note {{replace the placeholder with the inferred type 'Int'}} {{9-10=Int}}
let _: Int = 0 as (_) // expected-error {{type placeholder not allowed here}}
let _: Int = 0 as (((((_))))) // expected-error {{type placeholder not allowed here}}

_ = (1...10)
    .map {
        (
            $0,
            (
                "\($0)",
                $0 > 5
            )
        )
    }
    .map { (intValue, x: (_, boolValue: _)) in
        x.boolValue ? intValue : 0
    }

let _: SetFailureType<Int, String> = Just<Int>().setFailureType(to: _.self).setFailureType(to: String.self) // expected-error {{type placeholder not allowed here}} expected-error {{generic parameter 'T' could not be inferred}}

// N.B. The parallel structure of the annotation and inferred default
// initializer types is all wrong. Plus, we do not trust
// the contextual type with placeholders in it so the result is a generic
// diagnostic.
func mismatchedDefault<T>(_ x: [_] = [String: T]()) {} // expected-error {{type placeholder not allowed here}}

func mismatchedReturnTypes() -> _ { // expected-error {{type placeholder not allowed here}}
  if true {
    return ""
  } else {
    return 0.5
  }
}

// FIXME: Opaque result types ought to be treated better than this. But it's
// tricky to know which type is intended in a lot of cases.
@available(iOS 13.0, OSX 10.15, tvOS 13.0, watchOS 6.0, *)
func opaque() -> some _ { // expected-error {{type placeholder not allowed here}}
  return Just<Int>().setFailureType(to: _.self)
  // expected-error@-1 {{type placeholder not allowed here}}
}

enum EnumWithPlaceholders {
  case topLevelPlaceholder(x: _) // expected-error {{type placeholder not allowed here}}
  case placeholderWithDefault(x: _ = 5) // expected-error {{type placeholder not allowed here}}
}

func deferredInit(_ c: Bool) {
  let x: _ // expected-error {{type placeholder not allowed here}}
  if c {
    x = "Hello"
  } else {
    x = "Goodbye"
  }
}

// https://github.com/apple/swift/issues/63130
let _: _  = nil // expected-error{{'nil' requires a contextual type}}
let _: _? = nil // expected-error{{'nil' requires a contextual type}}

// rdar://106621760 - failed to produce a diagnostic when placeholder type appears in editor placeholder
do {
  struct X<T> {
    init(content: () -> T) {}
  }

  func test(_: () -> Void) {}

  test {
    _ = X(content: <#T##() -> _#>) // expected-error {{editor placeholder in source file}}
  }
}

// Make sure we reject placeholders here.
protocol TestPlaceholderRequirement {
  func foo(_:_) // expected-error {{type placeholder not allowed here}}
  func bar() -> _ // expected-error {{type placeholder not allowed here}}
  func baz() -> [_] // expected-error {{type placeholder not allowed here}}
  func qux(_: [_]) // expected-error {{type placeholder not allowed here}}

  subscript(_: _) -> Void { get } // expected-error {{type placeholder not allowed here}}
  subscript() -> _ { get } // expected-error {{type placeholder not allowed here}}
}

func testPlaceholderFn1(_:_) {} // expected-error {{type placeholder not allowed here}}
func testPlaceholderFn2() -> _ {} // expected-error {{type placeholder not allowed here}}

var testPlaceholderComputed1: _ { 0 } // expected-error {{type placeholder not allowed here}}
var testPlaceholderComputed2: [_] { [0] } // expected-error {{type placeholder not allowed here}}

struct TestPlaceholderSubscript {
  subscript(_: _) -> Void { () } // expected-error {{type placeholder not allowed here}}
  subscript(_: [_]) -> Void { () } // expected-error {{type placeholder not allowed here}}
  subscript() -> _ { () } // expected-error {{type placeholder not allowed here}}
  subscript() -> [_] { [0] } // expected-error {{type placeholder not allowed here}}
}

enum TestPlaceholderInEnumElement {
  case a(_) // expected-error {{type placeholder not allowed here}}
  case b([_]) // expected-error {{type placeholder not allowed here}}
}

@freestanding(expression) macro testPlaceholderMacro(_ x: _) -> String = #file
// expected-error@-1 {{type placeholder not allowed here}}

@freestanding(expression) macro testPlaceholderMacro(_ x: [_]) -> String = #file
// expected-error@-1 {{type placeholder not allowed here}}

@freestanding(expression) macro testPlaceholderMacro() -> _ = #file
// expected-error@-1 {{type placeholder not allowed here}}

// Make sure we can use decls with placeholders in their interface type.
func usePlaceholderDecls(
  _ fromProto: some TestPlaceholderRequirement, _ hasSubscript: TestPlaceholderSubscript
) {
  _ = optInt

  _ = testPlaceholderComputed1
  _ = testPlaceholderComputed2

  fromProto.foo(0)
  _ = fromProto.bar()
  _ = fromProto.baz()
  fromProto.qux([])
  fromProto[0]

  _ = hasSubscript[0]

  _ = TestPlaceholderInEnumElement.a(0)
  _ = TestPlaceholderInEnumElement.b([])

  // There are marked invalid so get removed from the overload set.
  // FIXME: We ought to turn them into holes instead
  _ = #testPlaceholderMacro(0) // expected-error {{no macro named 'testPlaceholderMacro'}}
  _ = #testPlaceholderMacro([]) // expected-error {{no macro named 'testPlaceholderMacro'}}

  _ = testPlaceholderFn1(0)
  _ = testPlaceholderFn2()
}
