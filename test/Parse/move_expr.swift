// RUN: %target-typecheck-verify-swift  -disable-availability-checking

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


