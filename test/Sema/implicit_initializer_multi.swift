// RUN: %target-swift-frontend -emit-silgen -primary-file %s %S/Inputs/implicit_initializer_multi_other.swift -verify

func test() {
  let _ = DefaultInitializable()
  let _ = DefaultInitializableClass()
}
