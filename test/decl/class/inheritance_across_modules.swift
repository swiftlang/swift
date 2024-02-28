// RUN: rm -rf %t
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module-path %t/MyModule.swiftmodule %t/Inputs/MyModule.swift

// RUN: %target-swift-frontend -typecheck -verify -I %t %t/test.swift

//--- Inputs/MyModule.swift
open class MySuperclassA {
  required public init() { }
  internal init(boop: Bool) {}
}

open class MySuperclassB {
}

//--- test.swift
import MyModule

class MySubclassA: MySuperclassA {
  // expected-warning{{'required' initializer 'init()' must be provided by subclass of 'MySuperclassA'; this is an error in Swift 6}}
  // expected-warning@-2{{class 'MySubclassA' has no initializers; this is an error in Swift 6}}
  var hi: String
  // expected-note@-1{{stored property 'hi' without initial value prevents synthesized initializers}}
}

class MySubclassB: MySuperclassB {
  // expected-warning@-1{{class 'MySubclassB' has no initializers; this is an error in Swift 6}}
  var hi: String
  // expected-note@-1{{stored property 'hi' without initial value prevents synthesized initializers}}
}
