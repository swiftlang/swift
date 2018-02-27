// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s
// REQUIRES: objc_interop

import Foundation

func iuo_error(prop: IUOProperty) {
  let _: Coat? = prop.iuo.optional()
  // expected-error@-1 {{value of optional type '(() -> Coat?)?' not unwrapped; did you mean to use '!' or '?'?}}
  let _: Coat? = prop.iuo.optional()!
  // expected-error@-1 {{cannot invoke 'optional' with no arguments}}
  let _: Coat? = prop.iuo.optional!()
  let _: Coat? = prop.iuo.optional!()!
  let _: Coat? = prop.iuo!.optional()
  // expected-error@-1 {{value of optional type '(() -> Coat?)?' not unwrapped; did you mean to use '!' or '?'?}}
  let _: Coat? = prop.iuo!.optional()!
  // expected-error@-1 {{cannot invoke 'optional' with no arguments}}
  let _: Coat? = prop.iuo!.optional!()
  let _: Coat? = prop.iuo!.optional!()!
  let _: Coat = prop.iuo.optional()
  // expected-error@-1 {{value of optional type '(() -> Coat)?' not unwrapped; did you mean to use '!' or '?'?}}
  let _: Coat = prop.iuo.optional()!
  // expected-error@-1 {{cannot invoke 'optional' with no arguments}}
  let _: Coat = prop.iuo.optional!()
  let _: Coat = prop.iuo.optional!()!
  let _: Coat = prop.iuo!.optional()
  // expected-error@-1 {{value of optional type '(() -> Coat)?' not unwrapped; did you mean to use '!' or '?'?}}
  let _: Coat = prop.iuo!.optional()!
  // expected-error@-1 {{cannot invoke 'optional' with no arguments}}
  let _: Coat = prop.iuo!.optional!()
  let _: Coat = prop.iuo!.optional!()!
}
