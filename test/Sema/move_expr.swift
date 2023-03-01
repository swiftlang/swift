// RUN: %target-typecheck-verify-swift  -disable-availability-checking -enable-experimental-move-only

class Klass {
    var k: Klass? = nil
}

var global: Int = 5
func testGlobal() {
    let _ = consume global
}

func testLet() {
    let t = String()
    let _ = consume t
}

func testVar() {
    var t = String()
    t = String()
    let _ = consume t
}

func testExprFailureLet() {
    let t = 5
    // Next line is parsed as move(t) + t
    let _ = consume t + t
}

func testExprFailureVar() {
    var t = 5
    t = 5
    // Next line is parsed as move(t) + t
    let _ = consume t + t
}

func letAddressOnly<T>(_ v: T) {
    let t = v
    let _ = consume t
}

struct StructWithField {
    var k: Klass? = nil
}

func testLetStructAccessField() {
    let t = StructWithField()
    let _ = consume t.k  // expected-error {{'consume' can only be applied to lvalues}}
}

func testVarStructAccessField() {
    var t = StructWithField()
    t = StructWithField()
    let _ = consume t.k // expected-error {{'consume' can only be applied to lvalues}}
}

func testLetClassAccessField() {
    let t = Klass()
    let _ = consume t.k  // expected-error {{'consume' can only be applied to lvalues}}
}

func testVarClassAccessField() {
    var t = Klass()
    t = Klass()
    let _ = consume t.k // expected-error {{'consume' can only be applied to lvalues}}
}
