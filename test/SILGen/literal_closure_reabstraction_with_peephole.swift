// RUN: %target-swift-emit-silgen -verify %s

fileprivate typealias Closure = () -> Void

func crash1() {
    let closure1: Closure? = nil
    let closure2: Closure? = nil
    let closure3: Closure? = nil
    print("Closures: \(String(describing: closure1)), \(String(describing: closure2)), \(String(describing: closure3))")
   
    let closure = closure1 ?? closure2 ?? closure3

    print("\(#line): \(String(describing: closure))")
    closure?() // <- EXC_BAD_ACCESS
    assert(closure == nil)
}

func crash2() {
    let closure1: Closure? = nil
    let closure2: Closure? = nil
    let closure3: Closure? = { }
    print("Closures: \(String(describing: closure1)), \(String(describing: closure2)), \(String(describing: closure3))")

    let closure = closure1 ?? closure2 ?? closure3

    print("\(#line): \(String(describing: closure))")
    closure?() // <- EXC_BAD_ACCESS
    assert(closure != nil)
}

crash1()
crash2()
