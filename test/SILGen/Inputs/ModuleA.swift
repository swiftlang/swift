@usableFromInline
internal var gg = nonTrivialInit()

@inlinable
public func get_gg_a() -> Int {
  return gg
}

@inlinable
@usableFromInline
@inline(never)
internal func nonTrivialInit() -> Int {
  return 27
}
