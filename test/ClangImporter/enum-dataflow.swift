// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil %s -verify

// REQUIRES: objc_interop

import Foundation
import user_objc

let aliasOriginal = NSAliasesEnum.byName

switch aliasOriginal { // expected-error {{switch must be exhaustive, consider adding missing cases}}
case .original:
  break
}

switch aliasOriginal { // expected-error {{switch must be exhaustive, consider adding missing cases}}
case .bySameValue:
  break
}
