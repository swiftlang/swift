// Checks that we don't crash.
// RUN: %sourcekitd-test -req=cursor -pos=8:19 %s -- %s | %FileCheck %s
// CHECK: source.lang.swift.decl.function.method.instance

public extension AnyIterator {
  @available(*, deprecated)
  public extension AnySequence {
    public func take(n: Int) -> AnySequence<Element> {
      return AnySequence([])
    }
}
