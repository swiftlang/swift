import _Differentiation

struct Struct {
    @inlinable
    static func max<T: Comparable>(
        _ x: T,
        _ y: T
    ) -> T {
        if x > y
            return y
        else
            return x
    }
}
