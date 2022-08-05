// RUN: %target-swift-emit-silgen -disable-availability-checking -verify %s

// rdar://94877954

// `dynamic` prevents SILGen from lowering away
// the opaque return type of `foo`
dynamic func foo<T: Sendable>(f: () -> T) -> some Sendable {
    if #available(macOS 11.0, *) {
        return f()
    } else {
        return ()
    }
}

func bar() {
    let x: Void = ()
    let y: () = ()
    var a = foo { x }
    a = foo { y }
    _ = a
}
