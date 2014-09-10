// RUN: %swift %s -emit-ir
func a(x: Any, y: Any) -> (((Any, Any) -> Any) -> Any) {
    return {
        (m: (Any, Any) -> Any) -> Any in
        return m(x, y)
    }
}
func b(z: (((Any, Any) -> Any) -> Any)) -> Any {
    return z({
        (p: Any, q:Any) -> Any in
        return p
    })
}
b(a(1, a(2, 3)))
