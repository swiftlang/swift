// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil %s -verify

// REQUIRES: objc_interop

import Foundation
import user_objc

let aliasOriginal = NSAliasesEnum.byName

switch aliasOriginal {
case .original:
  break
} // expected-error {{switch must be exhaustive, consider adding a default clause}}

switch aliasOriginal {
case .bySameValue:
  break
} // expected-error {{switch must be exhaustive, consider adding a default clause}}
