// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil %s -verify

// REQUIRES: objc_interop

import Foundation
import user_objc

let aliasOriginal = NSAliasesEnum.ByName

switch aliasOriginal {
case .Original:
  break
} // expected-error {{switch must be exhaustive, consider adding a default clause}}

switch aliasOriginal {
case .BySameValue:
  break
} // expected-error {{switch must be exhaustive, consider adding a default clause}}
