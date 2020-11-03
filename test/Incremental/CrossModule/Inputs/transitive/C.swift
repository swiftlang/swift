#if OLD
public func fromC() {}
#elseif NEW
public func fromC(parameter: Int = 0) {}
#else
#error("test must define one of NEW or OLD macros")
#endif
