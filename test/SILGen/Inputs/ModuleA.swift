@_versioned
internal var gg = nonTrivialInit()

@_inlineable
public func get_gg_a() -> Int {
  return gg
}

@_inlineable
@_versioned
@inline(never)
internal func nonTrivialInit() -> Int {
  return 27
}
