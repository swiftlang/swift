// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -I %S/Inputs/submodule -module-name Mixed %s

@_exported import Mixed
@_exported import Mixed.Submodule

func test() {
  _ = topLevel()
  _ = fromSubmodule()
}
