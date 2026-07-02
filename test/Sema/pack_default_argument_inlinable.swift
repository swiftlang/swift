// RUN: %target-swift-frontend -typecheck %s

// Test that @inlinable functions with parameter pack default arguments
// work correctly. This tests SameShape requirement handling in
// TypeCheckConstraints.cpp for default argument expressions.

public protocol Plugin {
    func handle()
}

public struct DefaultPlugin: Plugin {
    public init() {}
    public func handle() {}
}

public struct OtherPlugin: Plugin {
    public init() {}
    public func handle() {}
}

@usableFromInline
struct Processor<each P: Plugin> {
    @usableFromInline let plugins: (repeat each P)
    @usableFromInline init(plugins: (repeat each P)) { self.plugins = plugins }
}

// Test @inlinable with parameter pack default arguments
@inlinable
public func process<each D: Plugin, each P: Plugin>(
    defaults: (repeat each D) = (DefaultPlugin()),
    plugins: (repeat each P) = ()
) {
    let _ = Processor(plugins: defaults)
    let _ = Processor(plugins: plugins)
}

// Test with multiple pack parameters having defaults
@inlinable
public func multiProcess<each A: Plugin, each B: Plugin, each C: Plugin>(
    first: (repeat each A) = (DefaultPlugin()),
    second: (repeat each B) = (DefaultPlugin(), OtherPlugin()),
    third: (repeat each C) = ()
) {
    let _ = Processor(plugins: first)
    let _ = Processor(plugins: second)
    let _ = Processor(plugins: third)
}

// Verify calling works
public func testCalls() {
    process()
    process(defaults: (DefaultPlugin(), OtherPlugin()))
    process(plugins: (DefaultPlugin(),))
    process(defaults: (DefaultPlugin(),), plugins: (OtherPlugin(),))

    multiProcess()
    multiProcess(first: (OtherPlugin(),))
    multiProcess(second: (DefaultPlugin(),))
}
