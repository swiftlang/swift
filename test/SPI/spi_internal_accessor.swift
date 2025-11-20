// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %s -module-name main \
// RUN:   -swift-version 6 -enable-library-evolution \
// RUN:   -emit-module-interface-path %t/main.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/main.private.swiftinterface

// RUN: %FileCheck %s -check-prefix CHECK-PUBLIC --input-file %t/main.swiftinterface
// CHECK-PUBLIC-NOT: @_spi

// RUN: %FileCheck %s -check-prefix CHECK-PRIVATE --input-file %t/main.private.swiftinterface

// RUN: %target-swift-typecheck-module-from-interface(%t/main.swiftinterface) -module-name main
// RUN: %target-swift-typecheck-module-from-interface(%t/main.private.swiftinterface) -module-name main

public struct S {
    @usableFromInline @_spi(X)
    internal var v: Int {
        get { return 42 }
    }
}

extension UnsafeMutablePointer {
  @_spi(X) @available(swift, obsoleted: 1)
// CHECK-PRIVATE: @_spi(X) @available(swift, obsoleted: 1)
  @usableFromInline
// CHECK-PRIVATE: @usableFromInline
  internal var pointee: Pointee {
// CHECK-PRIVATE: internal var pointee: Pointee {
    @_transparent unsafeAddress {
// CHECK-PRIVATE:   @_spi(X) @_transparent unsafeAddress {
      return UnsafePointer(self)
// CHECK-PRIVATE:     return UnsafePointer(self)
    }
// CHECK-PRIVATE:   }
    @_transparent nonmutating unsafeMutableAddress {
// CHECK-PRIVATE:   @_transparent nonmutating unsafeMutableAddress {
      return self
// CHECK-PRIVATE:     return self
    }
// CHECK-PRIVATE:   }
  }
// CHECK-PRIVATE: }
}

extension UnsafeMutablePointer {
  @_spi(X)
// CHECK-PRIVATE: @_spi(X)
  public var pointee2: Pointee {
// CHECK-PRIVATE: public var pointee2: Pointee {
    unsafeAddress {
// CHECK-PRIVATE:   unsafeAddress
      return UnsafePointer(self)
    }
    @_transparent nonmutating unsafeMutableAddress {
// CHECK-PRIVATE:   @_transparent nonmutating unsafeMutableAddress {
      return self
// CHECK-PRIVATE:     return self
    }
// CHECK-PRIVATE:   }
  }
// CHECK-PRIVATE: }
}


@_spi(Foo) @propertyWrapper public struct SPIWrapper<T> {
// CHECK-PRIVATE: @_spi(Foo) @propertyWrapper public struct SPIWrapper<T> {
  public init(wrappedValue: T) {}
// CHECK-PRIVATE:   @_spi(Foo) public init(wrappedValue: T)
  public var wrappedValue: T { fatalError() }
// CHECK-PRIVATE:   @_spi(Foo) public var wrappedValue: T {
// CHECK-PRIVATE:     @_spi(Foo) get
// CHECK-PRIVATE:   }
// CHECK-PRIVATE: }
}

public struct InternalSet {
// CHECK-PRIVATE: public struct InternalSet {
  @_spi(X) public internal(set) var long: Int {
    get { 0 }
    set { }
  }
// CHECK-PRIVATE:   @_spi(X) public var long: Swift.Int {
// CHECK-PRIVATE:     @_spi(X) get
// CHECK-PRIVATE:   }

  @_spi(X) public internal(set) var short: Int
// CHECK-PRIVATE:   @_spi(X) public var short: Swift.Int {
// CHECK-PRIVATE:     get
// CHECK-PRIVATE:   }
}
// CHECK-PRIVATE: }
