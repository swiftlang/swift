// RUN:  %target-swift-emit-sil %s -sil-verify-all -verify

////////////////////////
// MARK: Declarations //
////////////////////////

class T {}
final class S: T {}
enum Q { case one }
struct Z {}

/////////////////////////////////////////////
// MARK: Borrowed Class Casting Conversion //
/////////////////////////////////////////////

// reproduction of compiler crash: https://github.com/swiftlang/swift/issues/86458
func f1(_ s: borrowing S) { // expected-error {{'s' is borrowed and cannot be consumed}}
    _ = s as AnyObject // expected-note {{consumed here}}
}

// all examples below come from https://forums.swift.org/t/borrowed-value-with-as-casting/88236/2
// which originally crash the compiler
func f2(_ s: borrowing S) { // expected-error {{'s' is borrowed and cannot be consumed}}
    _ = s as? T // expected-warning {{conditional cast from 'S' to 'T' always succeeds}}
    // expected-note @-1 {{consumed here}}
}

func f3(_ s: borrowing S) { // expected-error {{'s' is borrowed and cannot be consumed}}
    let _: S? = s // expected-note {{consumed here}}
}

func f4(_ s: borrowing S) {
    _ = type(of: s)
}

func f5(_ s: borrowing S) { // expected-error {{'s' is borrowed and cannot be consumed}}
    let _: T = s // expected-note {{consumed here}}
}

////////////////////////////////////////////
// MARK: Borrowed Enum Casting Conversion //
////////////////////////////////////////////

func h1(_ q: borrowing Q) { // expected-error {{'q' is borrowed and cannot be consumed}}
    _ = q as AnyObject // expected-note {{consumed here}}
}

func h2(_ q: borrowing Q) { // expected-error {{'q' is borrowed and cannot be consumed}}
    _ = q as? Q // expected-warning {{conditional cast from 'Q' to 'Q' always succeeds}}
    // expected-note @-1 {{consumed here}}
}

func h3(_ q: borrowing Q) { // expected-error {{'q' is borrowed and cannot be consumed}}
    let _: Q? = q // expected-note {{consumed here}}
}

//////////////////////////////////////////////
// MARK: Borrowed Struct Casting Conversion //
//////////////////////////////////////////////

func g1(_ z: borrowing Z) { // expected-error {{'z' is borrowed and cannot be consumed}}
    _ = z as AnyObject // expected-note {{consumed here}}
}

func g2(_ z: borrowing Z) { // expected-error {{'z' is borrowed and cannot be consumed}}
    _ = z as? Z // expected-warning {{conditional cast from 'Z' to 'Z' always succeeds}}
    // expected-note @-1 {{consumed here}}
}

func g3(_ z: borrowing Z) { // expected-error {{'z' is borrowed and cannot be consumed}}
    let _: Z? = z // expected-note {{consumed here}}
}
