@inlinable @inline(__always)
public func withWrapper<R>(_ body: @escaping (Int) -> R) -> R {
    _withInner { x in body(x) }
}

@usableFromInline
@inline(never)
internal func _withInner<R>(_ body: (Int) -> R) -> R {
    body(42)
}
