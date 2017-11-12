@_versioned
internal var gg = nonTrivialInit()

@_inlineable
public func get_gg_b() -> Int {
  return gg
}

@_versioned
@_inlineable
@inline(never)
internal func nonTrivialInit() -> Int {
  return 28
}
