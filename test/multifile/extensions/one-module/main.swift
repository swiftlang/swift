// RUN: %target-swift-frontend -typecheck -primary-file %s %S/Inputs/library.swift

public func foo(x: [String]) {
  x.split(separator: ":")
}

