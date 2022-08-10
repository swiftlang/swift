// RUN: %target-typecheck-verify-swift  -disable-availability-checking -enable-experimental-move-only

class Klass {
    var k: Klass? = nil
}

var global: Int = 5
func testGlobal() {
    let _ = _move global
}

func testLet() {
    let t = String()
    let _ = _move t
}

func testVar() {
    var t = String()
    t = String()
    let _ = _move t
}

func testExprFailureLet() {
    let t = 5
    // Next line is parsed as move(t) + t
    let _ = _move t + t
    // Next line is parsed as move(t+t)
    let _ = _move (t+t) // expected-error {{'move' can only be applied to lvalues}}
}

func testExprFailureVar() {
    var t = 5
    t = 5
    // Next line is parsed as move(t) + t
    let _ = _move t + t
    // Next line is parsed as move(t+t)
    let _ = _move (t+t) // expected-error {{'move' can only be applied to lvalues}}
}

func letAddressOnly<T>(_ v: T) {
    let t = v
    let _ = _move t
}

struct StructWithField {
    var k: Klass? = nil
}

func testLetStructAccessField() {
    let t = StructWithField()
    let _ = _move t.k  // expected-error {{'move' can only be applied to lvalues}}
}

func testVarStructAccessField() {
    var t = StructWithField()
    t = StructWithField()
    let _ = _move t.k // expected-error {{'move' can only be applied to lvalues}}
}

func testLetClassAccessField() {
    let t = Klass()
    let _ = _move t.k  // expected-error {{'move' can only be applied to lvalues}}
}

func testVarClassAccessField() {
    var t = Klass()
    t = Klass()
    let _ = _move t.k // expected-error {{'move' can only be applied to lvalues}}
}
