@usableFromInline
internal var gg = nonTrivialInit()

@inlinable
public func get_gg_b() -> Int {
  return gg
}

@usableFromInline
@inlinable
@inline(never)
internal func nonTrivialInit() -> Int {
  return 28
}
