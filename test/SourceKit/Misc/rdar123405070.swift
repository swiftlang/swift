// RUN: %sourcekitd-test -req=open %s -- %s
// Test that this doesn't crash sourcekitd

@_backDeploy(before: macOS 14.0)
@inline(never)
public func foo() {}

@_extern(wasm, module: "b", name: "c")
@_extern(c)
private func bar()
