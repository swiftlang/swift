// RUN: %empty-directory(%t)
// RUN: not --crash %target-build-swift -emit-module -module-name sr15871 -emit-module-path %t/sr15871.swiftmodule -swift-version 5 -c %S/sr15871-crash-while-compiling-pythonkit-in-release-mode.swift
// XFAIL: *

// sr15871: crash while compiling PythonKit in release mode

public func pythonObject() {
  _ = PyCapsule_New({ _ in })
}

let PyCapsule_New: (@convention(c) (UnsafeMutableRawPointer?) -> Void) -> Void =
  loadSymbol(name: "PyCapsule_New")

func loadSymbol<T>(name: String, type: T.Type = T.self) -> T {
  fatalError()
}
