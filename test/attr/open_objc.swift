// RUN: %empty-directory(%t)
// RUN: %target-typecheck-verify-swift -sdk %clang-importer-sdk -I %t
// REQUIRES: objc_interop

import Foundation

open class InvalidOpenExtension {
  open func openMethod() { } // No warning: Pure Swift methods can be open inside the class declaration.
  open var openVar: Int { 3 } // No warning: Pure Swift properties can be open inside the class declaration.
}
extension InvalidOpenExtension {
  open func nonObjcOpenMethod() { } // expected-warning {{non-'@objc' methods in extensions cannot be overridden; use 'public' instead}} {{3-7=public}}
  open var nonObjcOpenVar: Int { 3 }  // expected-warning {{non-'@objc' properties in extensions cannot be overridden; use 'public' instead}} {{3-7=public}}
  @objc open func objcOpenMethod() { } // No warning: @objc methods can be open inside extensions. 
  @objc open var objcOpenVar: Int { 3 } // No warning: @objc methods can be open inside extensions. 
}

// For extensions with access level less than open, '`open` modifier conflicts with...' warning will always precede the 'non-@objc member...' warning. 
// For extensions with open access level, invalid open extension error will precede the 'non-@objc member...' warning. 
public extension InvalidOpenExtension { 
    open func nonObjcOpenMethod2() { } // expected-warning {{'open' modifier conflicts with extension's default access of 'public'}}
    open var nonObjcOpenVar2: Int { 3 } // expected-warning {{'open' modifier conflicts with extension's default access of 'public'}}
    @objc open func objcOpenMethod2() { } // expected-warning {{'open' modifier conflicts with extension's default access of 'public'}}
    @objc open var objcOpenVar2: Int { 3 } // expected-warning {{'open' modifier conflicts with extension's default access of 'public'}}
}

@objcMembers
open class ValidOpenExtension { }
extension ValidOpenExtension {
    open func objcOpenMethod() { } // No warning: This declaration is implicitly @objc
    open var objcOpenVar: Int { 3 } // No warning: This declaration is implicitly @objc
}

public extension ValidOpenExtension { 
    open func objcOpenMethod2() { } // expected-warning {{'open' modifier conflicts with extension's default access of 'public'}}
    open var objcOpenVar2: Int { 3 } // expected-warning {{'open' modifier conflicts with extension's default access of 'public'}}
}