
#if BEFORE

public func changeDefaultArgumentToMagic(name: String = "hello") {}

#else

public func changeDefaultArgumentToMagic(name: String = #file) {}

#endif
