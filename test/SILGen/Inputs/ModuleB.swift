

private var gg = nonTrivialInit()

public func get_gg_b() -> Int {
  return gg
}

@inline(never)
private func nonTrivialInit() -> Int {
  return 28
}
