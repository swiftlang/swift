// RUN: %target-typecheck-verify-swift

let x: _ = 0
let x2 = x
let dict1: [_: Int] = ["hi": 0]
let dict2: [Character: _] = ["h": 0]

let arr = [_](repeating: "hi", count: 3)

func foo(_ arr: [_] = [0]) {} // expected-error {{placeholder type not allowed here}}

let foo = _.foo // expected-error {{could not infer type for placeholder}}
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
let _: [String: Int] = dictionary(ofType: _.self)

let _: @convention(c) _ = { 0 } // expected-error {{@convention attribute only applies to function types}}
let _: @convention(c) (_) -> _ = { (x: Double) in 0 }
let _: @convention(c) (_) -> Int = { (x: Double) in 0 }

struct NonObjc {}

let _: @convention(c) (_) -> Int = { (x: NonObjc) in 0 } // expected-error {{'(NonObjc) -> Int' is not representable in Objective-C, so it cannot be used with '@convention(c)'}}

func overload() -> Int? { 0 }
func overload() -> String { "" }

let _: _? = overload()
let _ = overload() as _?
