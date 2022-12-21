// RUN: %target-typecheck-verify-swift  -disable-availability-checking -enable-experimental-move-only

class Klass {
    var k: Klass? = nil
}

func useString(_ x: String) {}

var global: String = "5"
func testGlobal() {
    useString(_borrow global)
}

func testLet() {
    let t = String()
    useString(_borrow t)
}

func testVar() {
    var t = String()
    t = String()
    useString(_borrow t)
}
