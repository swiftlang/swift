// RUN: %empty-directory(%t)
// RUN: %target-typecheck-verify-swift -sdk %clang-importer-sdk -I %t
// REQUIRES: objc_interop

import Foundation

open class InvalidOpenExtension {
  open func openMethod() { } // No warning: Pure Swift methods can be open inside the class declaration.
  open var openVar: Int { 3 } // No warning: Pure Swift properties can be open inside the class declaration.
}
extension InvalidOpenExtension {
  open func nonObjcOpenMethod() { } // expected-warning {{non-'@objc' instance method declared in extension cannot be overridden; use 'public' instead; this will be an error in a future Swift language mode}} {{3-7=public}}
  open class func nonObjcOpenMethod() { } // expected-warning {{non-'@objc' class method declared in extension cannot be overridden; use 'public' instead; this will be an error in a future Swift language mode}} {{3-7=public}}
  open var nonObjcOpenVar: Int { 3 }  // expected-warning {{non-'@objc' property declared in extension cannot be overridden; use 'public' instead; this will be an error in a future Swift language mode}} {{3-7=public}}
  open subscript(_ index: Int) -> Int { 3 } // expected-warning {{non-'@objc' subscript declared in extension cannot be overridden; use 'public' instead; this will be an error in a future Swift language mode}} {{3-7=public}}
  open convenience init(index: Int) { self.init() } // expected-error {{only classes and overridable class members can be declared 'open'; use 'public'}} // expected-warning {{non-'@objc' initializer declared in extension cannot be overridden; use 'public' instead; this will be an error in a future Swift language mode}}

  @objc open func objcOpenMethod() { } // No warning: @objc methods can be open inside extensions. 
  @objc open var objcOpenVar: Int { 3 } // No warning: @objc methods can be open inside extensions. 

  open class nonObjcClassDecl  { }
  open enum nonObjcEnumDecl { case one } // expected-error {{only classes and overridable class members can be declared 'open'; use 'public'}} {{3-7=public}}
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
    open class func objcOpenMethod() { } // No warning: This declaration is implicitly @objc
    open var objcOpenVar: Int { 3 } // No warning: This declaration is implicitly @objc
    open subscript(_ index: Int) -> Int { 3 } // No warning: This declaration is implicitly @objc
    open convenience init(index: Int) { self.init() } // expected-error {{only classes and overridable class members can be declared 'open'; use 'public'}}
}

public extension ValidOpenExtension { 
    open func objcOpenMethod2() { } // expected-warning {{'open' modifier conflicts with extension's default access of 'public'}}
    open var objcOpenVar2: Int { 3 } // expected-warning {{'open' modifier conflicts with extension's default access of 'public'}}
}

class InvalidOpenExtensionClass { }

open extension InvalidOpenExtensionClass {  // expected-error {{extensions cannot be declared 'open'; declare individual members as 'open' instead}} {{1-6=}} {{+2:9-9=open }} {{+3:3-3=public }} {{+5:9-9=open }} {{+6:3-3=public }}
  private func A() { } // OK
  @objc func B() { } // Insert open 
  func C() { } // Insert public
  @objc private var D: Int { 3 } // Okay
  @objc var E: Int { 3 } // Insert open
  var F: Int { 3 } // Insert public
  private var G: Int { 3 } // Okay
}

@objc
@objcMembers
class InvalidOpenExtensionObjcClass: NSObject { }

open extension InvalidOpenExtensionObjcClass {  // expected-error {{extensions cannot be declared 'open'; declare individual members as 'open' instead}} {{1-6=}} {{+2:9-9=open }} {{+3:3-3=open }} {{+5:9-9=open }} {{+6:3-3=open }}
  private func A() { } // OK
  @objc func B() { } // Insert open 
  func C() { } // Insert open
  @objc private var D: Int { 3 } // Okay
  @objc var E: Int { 3 } // Insert open
  var F: Int { 3 } // Insert open
  private var G: Int { 3 } // Okay
}
