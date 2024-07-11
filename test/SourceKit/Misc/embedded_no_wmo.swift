// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// TODO: This test passes locally on my machine, but fails for an unknown reason in CI
// REQUIRES: rdar130167087

//--- secondary.swift

final public class X<T> {
  var x: T

  init(_ t: T) { x = t}

  public func foo() -> T { x }
}

//--- primary.swift

// RUN: %sourcekitd-test  -req=diags %t/primary.swift -- %t/primary.swift %t/secondary.swift -enable-experimental-feature Embedded

// check that SourceKit does not crash on this

public func testit() -> X<Int> {
  return X(27)
}

