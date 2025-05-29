// RUN: %sourcekitd-test -req=open %s -- %s
// Test that this doesn't crash sourcekitd

@_backDeploy(before: macOS 14.0)
@inline(never)
public func foo() {}

@_extern(wasm, module: "b", name: "c")
@_extern(c)
private func bar()

@_originallyDefinedIn(module: "foo", macOS 5.0)
@available(macOS 5.0, *)
public func baz() {}

@_rawLayout(size: 5, alignment: 4)
@available(macOS 5.0, *)
struct Qux {}

@_rawLayout(like: Qux)
@available(macOS 5.0, *)
struct Flim {}

@_rawLayout(likeArrayOf: Qux, count: 5)
@available(macOS 5.0, *)
struct Flam {}
