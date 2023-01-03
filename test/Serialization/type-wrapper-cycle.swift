// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/src)
// RUN: %empty-directory(%t/sdk)
// RUN: split-file %s %t/src

// REQUIRES: asserts

// RUN: %target-swift-frontend -emit-module %t/src/PublicModule.swift \
// RUN:   -module-name PublicModule -swift-version 5 \
// RUN:   -emit-module-path %t/sdk/PublicModule.swiftmodule \
// RUN:   -enable-experimental-feature TypeWrappers

// RUN: %target-swift-frontend -typecheck %t/src/Client.swift \
// RUN:   -enable-experimental-feature TypeWrappers \
// RUN:   -module-name Client -I %t/sdk

//--- PublicModule.swift
@typeWrapper
public struct Wrapper<W: Wrapped, S> {
  public init(for: W.Type, storage: S) {}

  public subscript<V>(propertyKeyPath _: KeyPath<W, V>, storageKeyPath path: KeyPath<S, V>) -> V {
    get { fatalError() }
  }

  public subscript<V>(propertyKeyPath _: KeyPath<W, V>, storageKeyPath path: WritableKeyPath<S, V>) -> V {
    get { fatalError() }
    set { }
  }
}

@Wrapper
public protocol Wrapped {
  init(storageWrapper: Wrapper<Self, $Storage>)
}

//--- Client.swift
import PublicModule

struct Test : Wrapped {
  var x: Int = 42
}

let test = Test()
_ = test.$storage
