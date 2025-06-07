// RUN: rm -rf %t
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module-path %t/MyModule.swiftmodule %t/Inputs/MyModule.swift

// RUN: %target-swift-frontend -typecheck -verify -I %t %t/test.swift

//--- Inputs/MyModule.swift
open class MySuperclassA {
  required public init() { }
  internal init(boop: Bool) {}
}

//--- test.swift
import MyModule

class MySubclassA: MySuperclassA {
// expected-warning{{'required' initializer 'init()' must be provided by subclass of 'MySuperclassA'; this is an error in the Swift 6 language mode}}
  var hi: String
}
