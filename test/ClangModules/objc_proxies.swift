// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -I %S/Inputs/custom-modules %s -verify

import AppKit

func testAnimatorProxy(constraint:NSLayoutConstraint) {
  // Rely on warning on 'AnyObject' as an inferred type to check
  // if the result of 'animator' is being treated as instancetype.
  let x = constraint.animator()! // no-warning
  let y = x.constant
}

