// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %t/main.swift \
// RUN:   -import-objc-header %t/header.h \
// RUN:   -target %target-stable-abi-triple -Xcc -Wno-nullability-completeness \
// RUN:   -require-explicit-availability=warn

// REQUIRES: objc_interop

//--- header.h

@import Foundation;

@interface AlwaysAvailableClass : NSObject
- (void)alwaysAvailableMethod;
@property int alwaysAvailableProperty;
@end

API_AVAILABLE(macos(10.10), ios(8.0), watchos(2.0), tvos(9.0), visionos(1.0))
@interface HasAvailabilityClass : NSObject
- (void)hasAvailabilityMethod;
@end

void alwaysAvailableCDeclFunc(int param);
void alwaysAvailableCFunc(int param);

void hasAvailabilityCFunc(int param)
    API_AVAILABLE(macos(10.10), ios(8.0), watchos(2.0), tvos(9.0),
                  visionos(1.0));

//--- main.swift

// An '@implementation' declaration takes its availability from the imported
// declaration that it implements, so it never needs an availability attribute
// of its own.

@implementation @c
public func alwaysAvailableCFunc(_: Int32) { }

@implementation @_cdecl("alwaysAvailableCDeclFunc")
public func alwaysAvailableCDeclFunc(_: Int32) { }

@implementation @c
public func hasAvailabilityCFunc(_: Int32) { }

@objc @implementation
extension AlwaysAvailableClass {
  public func alwaysAvailableMethod() { }

  public var alwaysAvailableProperty: Int32 {
    get { 0 }
    set { }
  }
}

@objc @implementation
extension HasAvailabilityClass {
  public func hasAvailabilityMethod() { }
}

// A declaration that isn't an implementation still needs an attribute.

public func regularFunc() { } // expected-warning {{public declarations should have an availability attribute with an introduction version}}
