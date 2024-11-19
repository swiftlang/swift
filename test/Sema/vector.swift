// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature ValueGenerics

// REQUIRES: swift_feature_ValueGenerics

let a: Vector = [1, 2, 3] // Ok, Vector<3, Int>
let b: Vector<_, Int> = [1, 2, 3] // Ok, Vector<3, Int>
let c: Vector<3, _> = [1, 2, 3] // Ok, Vector<3, Int>

let d: Vector<2, _> = [1, 2, 3] // expected-error {{expected '2' elements in vector literal, but got '3'}}
let e: Vector<2, _> = [1] // expected-error {{expected '2' elements in vector literal, but got '1'}}

let f: Vector<_, Int> = ["hello"] // expected-error {{cannot convert value of type 'String' to expected element type 'Int'}}

func takeVectorOf2<T>(_: Vector<2, T>) {}

takeVectorOf2([1, 2]) // Ok
takeVectorOf2(["hello", "world"]) // Ok

takeVectorOf2([1]) // expected-error {{expected '2' elements in vector literal, but got '1'}}

takeVectorOf2([1, 2, 3]) // expected-error {{expected '2' elements in vector literal, but got '3'}}

takeVectorOf2(["hello"]) // expected-error {{expected '2' elements in vector literal, but got '1'}}

takeVectorOf2(["hello", "world", "!"]) // expected-error {{expected '2' elements in vector literal, but got '3'}}

func takeVectorOf2Int(_: Vector<2, Int>) {}

takeVectorOf2Int([1, 2]) // Ok

takeVectorOf2Int([1]) // expected-error {{expected '2' elements in vector literal, but got '1'}}

takeVectorOf2Int([1, 2, 3]) // expected-error {{expected '2' elements in vector literal, but got '3'}}

takeVectorOf2Int(["hello"]) // expected-error {{cannot convert value of type '[String]' to expected argument type 'Vector<2, Int>'}}

takeVectorOf2Int(["hello", "world"]) // expected-error {{cannot convert value of type 'String' to expected element type 'Int'}}
                                     // expected-error@-1 {{cannot convert value of type 'String' to expected element type 'Int'}}

takeVectorOf2Int(["hello", "world", "!"]) // expected-error {{cannot convert value of type '[String]' to expected argument type 'Vector<2, Int>'}}

struct X {
  var sprites: Vector<2, Int>
}

func foo(x: inout X) {
  x.sprites = [1, 2, 3] // expected-error {{cannot assign value of type '[Int]' to type 'Vector<2, Int>'}}
}

struct MySprites {
  var bricks: Vector<40, MySprite>
}

struct MySprite {
  var x = 42
}

nonisolated(unsafe)
var sprites: MySprites? = nil

func foo() {
  let bricks: Vector<1, MySprite> = [MySprite()]

  sprites = .init(bricks: bricks) // expected-error {{cannot convert value of type 'Vector<1, MySprite>' to expected argument type 'Vector<40, MySprite>'}}
                                  // expected-note@-1 {{arguments to generic parameter 'count' ('1' and '40') are expected to be equal}}
}
