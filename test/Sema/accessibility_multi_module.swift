// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -primary-file %S/Inputs/accessibility_multi_other_module.swift -emit-module-path %t/accessibility_multi_other_module.swiftmodule
// RUN: %target-swift-frontend -typecheck -I %t -primary-file %s -verify -verify-ignore-unknown

import accessibility_multi_other_module

func testPrivateConformance(_ instance: PrivateConformance) {
  instance.publicExtensionMember()
  // expected-error@-1 {{'publicExtensionMember' is inaccessible due to 'fileprivate' protection level}}

  instance.internalExtensionMember()
  // expected-error@-1 {{'internalExtensionMember' is inaccessible due to 'fileprivate' protection level}}
}

func testInternalConformance(_ instance: InternalConformance) {
  instance.publicExtensionMember()
  // expected-error@-1 {{'publicExtensionMember' is inaccessible due to 'internal' protection level}}

  instance.internalExtensionMember()
  // expected-error@-1 {{'internalExtensionMember' is inaccessible due to 'internal' protection level}}
}
