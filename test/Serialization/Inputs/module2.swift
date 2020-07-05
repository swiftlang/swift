import module1

public func module2Func() -> Int {
    return module1Func()
}

public func useP<T: P>(_ t: T) {
    t.memberMethod()
}
