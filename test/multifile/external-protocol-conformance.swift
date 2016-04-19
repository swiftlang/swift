// RUN: %target-swift-frontend -emit-ir -verify %s %S/Inputs/external-protocol-conformance/A.swift

func generic<T: P>(value: T) {}

// The goal here is to verify that a dependency on an invalid associated
// type in a protocol conformance in another file will prevent code
// generation.
func useGeneric() {
  generic(value: A())
}
