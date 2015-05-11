// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -I %S/Inputs/custom-modules %s -verify

// REQUIRES: objc_interop

import AppKit

func testAnimatorProxy(constraint:NSLayoutConstraint) {
  // Rely on warning on 'AnyObject' as an inferred type to check
  // if the result of 'animator' is being treated as instancetype.
  let x = constraint.animator()
  let _ = x.constant
}

