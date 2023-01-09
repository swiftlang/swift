// RUN: %empty-directory(%t/src)
// RUN: %empty-directory(%t/sdk)
// RUN: split-file %s %t/src

/// Build the library.
// RUN: %target-swift-frontend -emit-module %t/src/PublicModule.swift \
// RUN:   -module-name PublicModule -swift-version 5 -enable-library-evolution \
// RUN:   -emit-module-path %t/PublicModule.swiftmodule \
// RUN:   -emit-module-interface-path %t/PublicModule.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/PublicModule.private.swiftinterface \
// RUN:   -enable-experimental-feature TypeWrappers

/// Verify swiftinterfaces.
// RUN: %target-swift-typecheck-module-from-interface(%t/PublicModule.swiftinterface) -module-name PublicModule
// RUN: %target-swift-typecheck-module-from-interface(%t/PublicModule.private.swiftinterface) -module-name PublicModule

/// Test the client against the binary swiftmodule.
// RUN: %target-swift-frontend -typecheck %t/src/Client.swift \
// RUN:   -enable-experimental-feature TypeWrappers \
// RUN:   -module-name Client -I %t

/// Test the client against the private swiftinterface.
// RUN: rm %t/PublicModule.swiftmodule
// RUN: %target-swift-frontend -typecheck %t/src/Client.swift \
// RUN:   -module-name Client -I %t \
// RUN:   -enable-experimental-feature TypeWrappers

/// Test the client against the public swiftinterface.
// RUN: rm %t/PublicModule.private.swiftinterface
// RUN: %target-swift-frontend -typecheck %t/src/Client.swift \
// RUN:   -module-name Client -I %t \
// RUN:   -enable-experimental-feature TypeWrappers

//--- PublicModule.swift
@typeWrapper
public struct Wrapper<W : Wrapped, S> {
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
public protocol Wrapped {}

public struct MyWrappedType : Wrapped {
  var x: Int = 0

  public init() {}
}

@propertyWrapper
public struct PropWrapper<T> {
  typealias Wrapped = T

  public var wrappedValue: T {
    get { fatalError() }
    set { value = newValue }
  }

  var value: T?

  public init() {}

  public init(wrappedValue: T) {
    self.value = wrappedValue
  }
}

//--- Client.swift

import PublicModule

final class S : Wrapped {
  @PropWrapper var x: Int

  required init() {}

  required init(x: Int) {
    self.x = x
  }
}

let s = S()
_ = s.$storage

_ = MyWrappedType()
