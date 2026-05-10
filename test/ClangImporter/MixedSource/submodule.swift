// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -I %S/Inputs/submodule -module-name Mixed %s

@_exported import Mixed
@_exported import Mixed.Submodule

// https://github.com/apple/swift/issues/54693
// Make sure we can perform a scoped import on a submodule in a mixed
// source target.
import func Mixed.Submodule.fromSubmodule

func test() {
  topLevel()
  fromSubmodule()
}
