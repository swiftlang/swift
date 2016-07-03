// RUN: %target-swift-frontend %s -emit-silgen

func a() {
    func f(x: (Void) -> Any) { }
    var g = { 3 }
    f { g }
}
