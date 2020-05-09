public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

#if AFTER
@_weakLinked public func topLevelFunction(_ x: Int) {}

@_weakLinked public var storedGlobal: Int = 0

@_weakLinked public var computedGlobal: Int {
  get {
    return 0
  }
  set {}
}
#endif
