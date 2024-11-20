// RUN: %empty-directory(%t)
// RUN: split-file %s %t

//--- secondary.swift

final public class X<T> {
  var x: T

  init(_ t: T) { x = t}

  public func foo() -> T { x }
}

//--- primary.swift

// RUN: %sourcekitd-test  -req=diags %t/primary.swift -- %t/primary.swift %t/secondary.swift -enable-experimental-feature Embedded -target %target-cpu-apple-macos14

// REQUIRES: swift_in_compiler
// REQUIRES: embedded_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

// check that SourceKit does not crash on this

public func testit() -> X<Int> {
  return X(27)
}

