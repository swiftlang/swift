// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -I %S/Inputs/custom-modules %s -import-underlying-module

// expected-warning@<unknown> * {{libc not found for }}

var _ : ScriptTy
print(())
