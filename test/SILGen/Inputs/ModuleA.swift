
private var gg = nonTrivialInit()

public func get_gg_a() -> Int {
  return gg
}

@inline(never)
private func nonTrivialInit() -> Int {
  return 27
}
