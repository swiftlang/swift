// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -I %S/Inputs/custom-modules %s -import-underlying-module

var _ : ScriptTy
print(())
