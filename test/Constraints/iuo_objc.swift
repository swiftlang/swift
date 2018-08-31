// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s
// REQUIRES: objc_interop

import Foundation

func iuo_error(prop: IUOProperty) {
  let _: Coat? = prop.iuo.optional()
  // expected-error@-1 {{value of optional type '(() -> Coat?)?' must be unwrapped}}
  // expected-note@-2{{coalesce}}
  // expected-note@-3{{force-unwrap}}
  let _: Coat? = prop.iuo.optional()!
  // expected-error@-1 {{cannot invoke 'optional' with no arguments}}
  let _: Coat? = prop.iuo.optional!()
  let _: Coat? = prop.iuo.optional!()!
  let _: Coat? = prop.iuo!.optional()
  // expected-error@-1 {{value of optional type '(() -> Coat?)?' must be unwrapped}}
  // expected-note@-2{{coalesce}}
  // expected-note@-3{{force-unwrap}}
  let _: Coat? = prop.iuo!.optional()!
  // expected-error@-1 {{cannot invoke 'optional' with no arguments}}
  let _: Coat? = prop.iuo!.optional!()
  let _: Coat? = prop.iuo!.optional!()!
  let _: Coat = prop.iuo.optional()
  // expected-error@-1 {{value of optional type '(() -> Coat)?' must be unwrapped}}
  // expected-note@-2{{coalesce}}
  // expected-note@-3{{force-unwrap}}
  let _: Coat = prop.iuo.optional()!
  // expected-error@-1 {{cannot invoke 'optional' with no arguments}}
  let _: Coat = prop.iuo.optional!()
  let _: Coat = prop.iuo.optional!()!
  let _: Coat = prop.iuo!.optional()
  // expected-error@-1 {{value of optional type '(() -> Coat)?' must be unwrapped}}
  // expected-note@-2{{coalesce}}
  // expected-note@-3{{force-unwrap}}
  
  let _: Coat = prop.iuo!.optional()!
  // expected-error@-1 {{cannot invoke 'optional' with no arguments}}
  let _: Coat = prop.iuo!.optional!()
  let _: Coat = prop.iuo!.optional!()!

  let _ = prop.iuo.name
}

protocol X {}

extension X where Self : OptionalRequirements {
  func test() {
    let _ = self.name
  }
}
