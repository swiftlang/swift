// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// internal import
// RUN: %target-swift-frontend -typecheck -I %t %t/Client.swift \
// RUN:   -sdk %clang-importer-sdk -swift-version 6 -enable-library-evolution \
// RUN:   -disable-objc-attr-requires-foundation-module \
// RUN:   -emit-module-interface-path %t/Client.swiftinterface \
// RUN:   -emit-objc-header-path %t/Client-Swift.h
// RUN: %target-swift-typecheck-module-from-interface(%t/Client.swiftinterface)
// RUN: %target-clang -fsyntax-only %t/Client-Swift.h
// RUN: %FileCheck %s --input-file %t/Client-Swift.h --check-prefix CHECK-COMPAT
// CHECK-COMPAT-NOT: MyDouble
// CHECK-COMPAT-NOT: MyPoint

/// @_implementationOnly import
// RUN: %target-swift-frontend -typecheck -I %t %t/Client.swift -D IOI \
// RUN:   -sdk %clang-importer-sdk -swift-version 6 -enable-library-evolution \
// RUN:   -disable-objc-attr-requires-foundation-module \
// RUN:   -emit-module-interface-path %t/Client.swiftinterface \
// RUN:   -emit-objc-header-path %t/Client-Swift.h
// RUN: %target-swift-typecheck-module-from-interface(%t/Client.swiftinterface)
// RUN: %target-clang -fsyntax-only %t/Client-Swift.h
// RUN: %FileCheck %s --input-file %t/Client-Swift.h --check-prefix CHECK-COMPAT

/// Check where we still raise errors
// RUN: %target-swift-frontend -typecheck -I %t %t/Client.swift -D ERRORS \
// RUN:   -sdk %clang-importer-sdk -swift-version 6 -enable-library-evolution \
// RUN:   -disable-objc-attr-requires-foundation-module \
// RUN:   -emit-module-interface-path %t/Client.swiftinterface \
// RUN:   -emit-objc-header-path %t/Client-Swift.h \
// RUN:   -verify -verify-additional-file %t/Lib.h

// REQUIRES: objc_interop

//--- module.modulemap
module Lib {
    header "Lib.h"
}

//--- Lib.h
@import Foundation;

typedef struct {
  double x, y;
} MyPoint;

typedef double MyDouble; // expected-note {{type declared here}}

void canReferenceHiddenDependency(MyPoint, MyDouble);
void stillUnusableFromInlinable(); // expected-note {{global function 'stillUnusableFromInlinable()' is not '@usableFromInline' or public}}

@interface ObjCImplClass: NSObject // expected-note {{class declared here}}
@property MyPoint point;
- (nonnull instancetype)initWithPoint:(MyPoint)point;
- (void)method:(MyDouble)a;
@end

//--- Client.swift

#if IOI
  @_implementationOnly import Lib
#else
  internal import Lib // expected-note {{type alias 'MyDouble' imported as 'internal' from 'Lib' here}}
  // expected-note @-1 {{global function 'stillUnusableFromInlinable()' imported as 'internal' from 'Lib' here}}
  // expected-note @-2 {{class 'ObjCImplClass' imported as 'internal' from 'Lib' here}}
#endif

@c @implementation
public func canReferenceHiddenDependency(_ a: MyPoint, _ b: MyDouble) { }

@c @implementation
public func stillUnusableFromInlinable() { }

@objc @implementation
extension ObjCImplClass { // expected-error {{cannot use class 'ObjCImplClass' in an extension with public or '@usableFromInline' members; 'Lib' was not imported publicly}}
  var point: MyPoint

  public init(point: MyPoint) {
    self.point = point
  }

  public func method(_ a: MyDouble) {
    func localFunc(_ b: MyDouble) {}
    localFunc(a)
  }

#if ERRORS
  final public func notAnImplementation(a: MyDouble) {} // expected-error {{method cannot be declared public because its parameter uses an internal type}}
  // expected-note @-1 {{type alias 'MyDouble' is imported by this file as 'internal' from 'Lib'}}

  final public func notAnImplementationLocal(a: SomeInternalType) {} // expected-error {{method cannot be declared public because its parameter uses an internal type}}
#endif
}

internal struct SomeInternalType {} // expected-note {{type declared here}}

#if ERRORS
@inlinable public func inlinable() {
    stillUnusableFromInlinable() // expected-error {{global function 'stillUnusableFromInlinable()' is internal and cannot be referenced from an '@inlinable' function}}
}
#endif
