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
// expected-note@-1 {{add missing case: '.original'}}
// expected-note@-2 {{add missing case: '.differentValue'}}
// expected-note@-3 {{add missing cases}}
case .bySameValue:
  break
}
