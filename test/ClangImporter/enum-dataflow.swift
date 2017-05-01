// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -verify

// REQUIRES: objc_interop

import Foundation
import user_objc

let aliasOriginal = NSAliasesEnum.byName

switch aliasOriginal { // expected-error {{switch must be exhaustive, consider adding missing cases:}}
// expected-note@-1 {{missing case: '.differentValue'}}
case .original:
  break
}

switch aliasOriginal { // expected-error {{switch must be exhaustive, consider adding missing cases:}}
// expected-note@-1 {{missing case: '.original'}}
// expected-note@-2 {{missing case: '.differentValue'}}
case .bySameValue:
  break
}
