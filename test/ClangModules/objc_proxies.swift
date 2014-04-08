// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -I %S/Inputs/custom-modules %s -verify

import AppKit

func testAnimatorProxy(constraint:NSLayoutConstraint) {
  // Rely on warning on 'AnyObject' as an inferred type to check
  // if the result of 'animator' is getting papered over.
  let x = constraint.animator()! // expected-warning {{constant 'x' inferred to have type 'AnyObject', which may be unexpected}} expected-note {{add an explicit type annotation to silence this warning}}
  let y = x.constant
}

