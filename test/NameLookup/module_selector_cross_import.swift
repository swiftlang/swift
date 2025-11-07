// RUN: %target-typecheck-verify-swift -sdk %clang-importer-sdk -module-name main -I %S/Inputs -enable-cross-import-overlays
// RUN: %target-typecheck-verify-swift -sdk %clang-importer-sdk -module-name main -I %S/Inputs -enable-experimental-feature ParserASTGen -enable-cross-import-overlays

// REQUIRES: swift_feature_ParserASTGen

import ModuleSelectorTestingKit
import ctypes

func crossImportLookups(
  _: E,
  _: ModuleSelectorTestingKit::E,
  _: _ModuleSelectorTestingKit_ctypes::E,
  _: Swift::E
  // expected-error@-1 {{'E' is not imported through module 'Swift'}}
  // expected-note@-2 {{did you mean module 'ModuleSelectorTestingKit'?}} {{6-11=ModuleSelectorTestingKit}}
) {
  e()
  ModuleSelectorTestingKit::e()
  _ModuleSelectorTestingKit_ctypes::e()
  Swift::e()
  // expected-error@-1 {{'e' is not imported through module 'Swift'}}
  // expected-note@-2 {{did you mean module 'ModuleSelectorTestingKit'?}} {{3-8=ModuleSelectorTestingKit}}
}
