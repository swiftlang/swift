// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Library.swiftmodule -parse-as-library %t/Library.swift

// RUN: %target-swift-frontend -typecheck -swift-version 5 -verify -I %t %t/Client.swift
// RUN: %target-swift-frontend -typecheck -swift-version 5 -verify -I %t %t/Client.swift -enable-upcoming-feature MemberImportVisibility -verify-additional-prefix member-visibility-

// REQUIRES: objc_interop
// REQUIRES: swift_feature_MemberImportVisibility

//--- Library.swift

import CoreGraphics

public func takesDouble(_ d: Double) { }
public func takesCGFloat(_ c: CGFloat) { }
public func returnsDouble() -> Double { return 3.14 }
public func returnsCGFloat() -> CGFloat { return 3.14 }

//--- Client.swift

import Library

// expected-member-visibility-note@-1 4 {{add import of module 'CoreFoundation'}}

func testImplicitConversions() {
  // Pass Double-typed value where CGFloat is expected (Double -> CGFloat)
  takesCGFloat(returnsDouble())
  // expected-member-visibility-error@-1 {{initializer 'init(_:)' is not available due to missing import of defining module 'CoreFoundation'}}

  // Pass CGFloat-typed value where Double is expected (CGFloat -> Double)
  takesDouble(returnsCGFloat())
  // expected-member-visibility-error@-1 {{initializer 'init(_:)' is not available due to missing import of defining module 'CoreFoundation'}}

  // Same conversions via stored intermediates
  let d = returnsDouble()
  takesCGFloat(d)
  // expected-member-visibility-error@-1 {{initializer 'init(_:)' is not available due to missing import of defining module 'CoreFoundation'}}

  let cgf = returnsCGFloat()
  takesDouble(cgf)
  // expected-member-visibility-error@-1 {{initializer 'init(_:)' is not available due to missing import of defining module 'CoreFoundation'}}
}
