// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature ValueGenerics

// REQUIRES: swift_feature_ValueGenerics

let a: Slab = [1, 2, 3] // Ok, Slab<3, Int>
let b: Slab<_, Int> = [1, 2, 3] // Ok, Slab<3, Int>
let c: Slab<3, _> = [1, 2, 3] // Ok, Slab<3, Int>

let d: Slab<2, _> = [1, 2, 3] // expected-error {{expected '2' elements in slab literal, but got '3'}}
let e: Slab<2, _> = [1] // expected-error {{expected '2' elements in slab literal, but got '1'}}

let f: Slab<_, Int> = ["hello"] // expected-error {{cannot convert value of type 'String' to expected element type 'Int'}}

func takeVectorOf2<T>(_: Slab<2, T>) {}

takeVectorOf2([1, 2]) // Ok
takeVectorOf2(["hello", "world"]) // Ok

takeVectorOf2([1]) // expected-error {{expected '2' elements in slab literal, but got '1'}}

takeVectorOf2([1, 2, 3]) // expected-error {{expected '2' elements in slab literal, but got '3'}}

takeVectorOf2(["hello"]) // expected-error {{expected '2' elements in slab literal, but got '1'}}

takeVectorOf2(["hello", "world", "!"]) // expected-error {{expected '2' elements in slab literal, but got '3'}}

func takeVectorOf2Int(_: Slab<2, Int>) {}

takeVectorOf2Int([1, 2]) // Ok

takeVectorOf2Int([1]) // expected-error {{expected '2' elements in slab literal, but got '1'}}

takeVectorOf2Int([1, 2, 3]) // expected-error {{expected '2' elements in slab literal, but got '3'}}

takeVectorOf2Int(["hello"]) // expected-error {{cannot convert value of type '[String]' to expected argument type 'Slab<2, Int>'}}

takeVectorOf2Int(["hello", "world"]) // expected-error {{cannot convert value of type 'String' to expected element type 'Int'}}
                                     // expected-error@-1 {{cannot convert value of type 'String' to expected element type 'Int'}}

takeVectorOf2Int(["hello", "world", "!"]) // expected-error {{cannot convert value of type '[String]' to expected argument type 'Slab<2, Int>'}}

struct X {
  var sprites: Slab<2, Int>
}

func foo(x: inout X) {
  x.sprites = [1, 2, 3] // expected-error {{cannot assign value of type '[Int]' to type 'Slab<2, Int>'}}
}

struct MySprites {
  var bricks: Slab<40, MySprite>
}

struct MySprite {
  var x = 42
}

nonisolated(unsafe)
var sprites: MySprites? = nil

func foo() {
  let bricks: Slab<1, MySprite> = [MySprite()]

  sprites = .init(bricks: bricks) // expected-error {{cannot convert value of type 'Slab<1, MySprite>' to expected argument type 'Slab<40, MySprite>'}}
                                  // expected-note@-1 {{arguments to generic parameter 'count' ('1' and '40') are expected to be equal}}
}

// Make sure the deserialized integer generic argument gets treated as an integer
// generic argument when we clone the generic param list for extensions.
extension Slab where Element: ~Copyable {
  func forEach(_ body: (borrowing Element) -> Void) {
    for i in 0 ..< count {
      body(self[i])
    }
  }

  func enumerated(_ body: (Int, borrowing Element) -> Void) {
    for i in 0 ..< count {
      body(i, self[i])
    }
  }
}
