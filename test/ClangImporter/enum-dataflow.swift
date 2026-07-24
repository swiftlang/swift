// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -verify -enable-objc-interop

import Foundation
import user_objc

let aliasOriginal = NSAliasesEnum.byName

switch aliasOriginal { // expected-error {{switch must be exhaustive}}
// expected-note@-1 {{add missing case: '.differentValue'}}
case .original:
  break
}

switch aliasOriginal { // expected-error {{switch must be exhaustive}}
// expected-note@-1 {{add missing cases: '.original', '.differentValue'}}
case .bySameValue:
  break
}
