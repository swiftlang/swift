// A collection of small test cases that exercise different codegen
// patterns for nonescaping closures to make sure we process them all
// correctly.

// RUN: %target-swift-frontend -emit-sil -parse-stdlib -module-name Swift %s
// RUN: %target-swift-frontend -emit-sil -O -parse-stdlib -module-name Swift %s

@_marker protocol Copyable {}
precedencegroup CastingPrecedence {}
precedencegroup AssignmentPrecedence {}

public class X {}
public class Y {}

@_transparent
public func tbutt<T>(_: @autoclosure () -> (X, T)) {}

public func tubbt<T>(x: X, y: T) {
    tbutt((x, y))
}

public func zim<T>(_: (T) -> T) {}

public func zang(x: X, y: Y) {
    zim { _, _ in (x, y) }
}

public func zang<T, U>(x: T, y: U) {
    zim { _, _ in (x, y) }
}

public struct Butt {
    public init<T>(_: (T) -> T) {}
}

public func tubb(_: X) {}

public func butt(x: X, y: Y) {
    let _ = Butt { _, _ in (x, y) }
}

public func butt<T, U>(x: T, y: U) {
    let _ = Butt { _, _ in (x, y) }
}

public func oof<T>(_: @autoclosure () throws -> T) rethrows {}
public func ooff<T>(_: () throws -> T) rethrows {}

public func all<T>() -> T { return any() }
public func any<T>() -> T { return all() }

public func foof<T>(_: T) {
    return oof(any() as T)
}

public func foo<T>(_ b: () throws -> T) rethrows {
    return try ooff { try b() }
}

public func rab(_: () -> ()) {}

public func bar(_ b: () -> ()) {
    rab(b)
}

public func bar(_: C) {}

public class C {}

public func bas(x: C) {
    foo { bar(x) }
}

@_transparent
public func stackBalance(_ b: (Builtin.RawPointer) throws -> (), _ w: Builtin.Word) throws {
    let s = Builtin.stackAlloc(w, w, w)

    do {
        try b(s)
        Builtin.stackDealloc(s)
        return
    } catch {
        Builtin.stackDealloc(s)
        throw error
    }
}

public protocol Error {}

public func stackBalance2(_ w: Builtin.Word) throws {
    try stackBalance({ _ in let _ = w }, w)
}

public struct Z {
    subscript(_: () -> ()) -> Z {
        get { return self }
        _modify { yield &self }
    }
}

public enum Optional<Wrapped> {
    case some(Wrapped)
    case none
}

public func xx(x: inout Z, y: Z) {
    x[{ let _ = y }] = y
}
