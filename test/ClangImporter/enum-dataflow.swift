// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -verify -enable-objc-interop

// expected-warning@<unknown> * {{libc not found for }}

import Foundation
import user_objc

let aliasOriginal = NSAliasesEnum.byName

switch aliasOriginal { // expected-error {{switch must be exhaustive}}
// expected-note@-1 {{add missing cases: '.differentValue', '@unknown default'}}
// expected-note@-2 {{handle unknown values using "@unknown default"}}
case .original:
  break
}

switch aliasOriginal { // expected-error {{switch must be exhaustive}}
// expected-note@-1 {{add missing cases: '.original', '.differentValue', '@unknown default'}}
// expected-note@-2 {{handle unknown values using "@unknown default"}}
case .bySameValue:
  break
}
