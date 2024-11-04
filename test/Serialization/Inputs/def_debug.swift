@inlinable @inline(__always)
public func foo(x: UInt64) -> UInt64 {
    if (x > 100) {
        return 100
    }
    return 1
}


/* Swift tries to lookup for
   `generic specialization <serialized, Swift.Int> of Swift.Hasher.combine<A
   where A: Swift.Hashable>(A) -> ()` which is only referenced by debug info in
   the stdlib.  Make sure compiler handles this correctly and does not crash
   when trying to compile this
 */

@inlinable
@inline(__always)
public func specializedGenericInlined() -> Int {
    var hasher = Hasher()
    hasher.combine(1)
    return hasher.finalize()
}
