// RUN: %target-swift-frontend -emit-sil -verify -enable-experimental-feature BorrowingSwitch -disable-experimental-parser-round-trip %s

struct Payload: ~Copyable {
    var x: Int
    var y: String
}

enum Foo: ~Copyable {
    case payload(Payload)
    case noPayload
}

enum Bar: ~Copyable {
    case payload(Foo)
    case noPayload
}

enum Bas: ~Copyable {
    case loadablePayload(Foo)
    case aoPayload(Any)
}

@_silgen_name("condition")
func condition(_: borrowing Payload) -> Bool

@_silgen_name("hungryCondition")
func hungryCondition(_: consuming Payload) -> Bool

func eat(payload: consuming Payload) {}
func nibble(payload: borrowing Payload) {}

func test(consuming foo: consuming Bar) { // expected-error{{'foo' used after consume}}
    switch foo {
    case .payload(.payload(_borrowing x))
      where condition(x):
        nibble(payload: x)
    // can't consume _borrowing bindings in either `where` condition 
    // or body
    case .payload(.payload(_borrowing x)) // expected-error{{cannot be consumed}}
      where hungryCondition(x): // expected-note{{consumed here}}
        eat(payload: x) // expected-note{{consumed here}}
    case .payload(.payload(_borrowing x)): // expected-warning{{}}
        break
    case .payload(.noPayload):
        ()
    case .noPayload:
        ()
    }

    switch foo { // expected-note{{consumed here}}
    case .payload(.payload(let x))
      where condition(x):
        nibble(payload: x)
    // can't consume in a `where` condition even if binding is consumable
    case .payload(.payload(let x)) // expected-error{{cannot be consumed}}
      where hungryCondition(x): // expected-note{{consumed here}}
        // consuming in the case block is OK though
        eat(payload: x)
    case .payload(.payload(let x)): // expected-warning{{}}
        break
    case .payload(.noPayload):
        ()
    case .noPayload:
        ()
    }

    switch foo { // expected-note{{used here}}
    case _borrowing x: // expected-warning{{}}
        break
    }
}

@_silgen_name("nibble_bar")
func nibble(bar: borrowing Bar) 

func test(borrowing foo: borrowing Bar) { // expected-error{{'foo' is borrowed and cannot be consumed}}
    // can't use consuming patterns on a borrow
    // TODO: improve diagnostic
    switch foo {
    case .payload(.payload(let x)): // expected-note{{consumed here}} expected-warning{{}}
        break
    case .payload(.noPayload): // expected-note{{consumed here}}
        ()
    case .noPayload:
        ()
    }

    switch foo {
    case .payload(.payload(_borrowing x))
      where condition(x):
        nibble(payload: x)
    case .payload(.payload(_borrowing x)) // expected-error{{'x' is borrowed and cannot be consumed}}
      where hungryCondition(x): // expected-note{{consumed here}}
        eat(payload: x) // expected-note{{consumed here}}
    case .payload(.payload(_borrowing x)): // expected-warning{{}}
        break
    case .payload(.noPayload):
        ()
    case .noPayload:
        ()
    }
}
