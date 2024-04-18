// RUN: %target-swift-frontend -enable-experimental-feature BorrowingSwitch -typecheck -verify %s

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

    var member: Bar { fatalError() }
}

struct SourceBreakTest {
    func foo() -> Bar {}

    func callAsFunction() -> Bar { fatalError() }
}

let borrowing = SourceBreakTest()

func ~=(_: borrowing Bar, _: borrowing Bar) -> Bool { fatalError() }

func useBorrowBar(_: borrowing Bar) { fatalError() }
func useBorrowFoo(_: borrowing Foo) { fatalError() }
func useBorrowPayload(_: borrowing Payload) { fatalError() }

func testBorrowingPatterns(bar: borrowing Bar) {
    switch bar {
    case borrowing .foo(): // parses as `borrowing.foo()` as before
        break
    case borrowing (): // parses as `borrowing()` as before
        break

    case borrowing x: 
        useBorrowBar(x)

    case .payload(borrowing x):
        useBorrowFoo(x)

    case borrowing x.member: // expected-error{{'borrowing' pattern modifier must be directly applied to pattern variable name}} expected-error{{cannot find 'x' in scope}}
        break

    case _borrowing x: // expected-warning{{'_borrowing' spelling is deprecated}} {{10-20=borrowing}}
        useBorrowBar(x)

    default:
        break
    }
}
