// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Library.swiftmodule -parse-as-library %t/Library.swift -enable-experimental-feature OnceFunctions
// RUN: %target-swift-frontend -typecheck -verify -I %t %t/Client.swift -enable-experimental-feature OnceFunctions

// REQUIRES: swift_feature_OnceFunctions

//--- Library.swift

public func check(_ condition: @autoclosure @callable(once) () -> Bool) {
  _ = condition()
}

public func run(_ body: @once () -> Void) {
  body()
}

//--- Client.swift

import Library

// Declarations that only differ in the execution semantics of a closure
// parameter don't shadow each other. Overload resolution prefers the one with
// the plain closure parameter instead, unless the argument has execution
// semantics.
func check(_ condition: @autoclosure () -> Bool) -> Int {
  _ = condition()
  return 0
}

func run(_ body: () -> Void) -> Int {
  body()
  return 0
}

func useOverloads(_ exactlyOnce: @once () -> Void) {
  let checkResult = check(true)
  let _: Int = checkResult
  let runResult = run {}
  let _: Int = runResult
  run(exactlyOnce)
}
