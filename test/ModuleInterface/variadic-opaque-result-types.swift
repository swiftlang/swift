// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/VariadicOpaqueResultTypes.swiftinterface) %s -module-name VariadicOpaqueResultTypes -target %target-swift-5.9-abi-triple
// RUN: %target-swift-typecheck-module-from-interface(%t/VariadicOpaqueResultTypes.swiftinterface) -module-name VariadicOpaqueResultTypes
// RUN: %FileCheck %s < %t/VariadicOpaqueResultTypes.swiftinterface


///
/// First, make sure pack expansions can appear in the generic argument list of an opaque return type.
///

public struct I1<each T>: IteratorProtocol {
  public mutating func next() -> (some Any)? { return 3 }
}

// CHECK: public struct I1<each T> : Swift.IteratorProtocol {
// CHECK:  public mutating func next() -> (some Any)?
// CHECK:  public typealias Element = @_opaqueReturnTypeOf("$s25VariadicOpaqueResultTypes2I1V4nextQrSgyF", 0) __<repeat each T>
// CHECK: }


public struct S1<each T>: Sequence {
  public func makeIterator() -> some IteratorProtocol {
    return I1<repeat each T>()
  }
}

// CHECK: public struct S1<each T> : Swift.Sequence {
// CHECK:   public func makeIterator() -> some Swift.IteratorProtocol
// CHECK:   public typealias Element = (@_opaqueReturnTypeOf("$s25VariadicOpaqueResultTypes2S1V12makeIteratorQryF", 0) __<repeat each T>).Element
// CHECK:   public typealias Iterator = @_opaqueReturnTypeOf("$s25VariadicOpaqueResultTypes2S1V12makeIteratorQryF", 0) __<repeat each T>
// CHECK: }


public struct Scalar<T> {
  public struct I2<U>: IteratorProtocol {
    public mutating func next() -> (some Any)? { return 3 }
  }
}


///
/// Now, test nested types. The next example uses the old "flat" syntax, because parameter packs are not involved.
///


// CHECK: public struct Scalar<T> {
// CHECK:   public struct I2<U> : Swift.IteratorProtocol {
// CHECK:     public mutating func next() -> (some Any)?
// CHECK:     public typealias Element = @_opaqueReturnTypeOf("$s25VariadicOpaqueResultTypes6ScalarV2I2V4nextQrSgyF", 0) __<T, U>
// CHECK:   }
// CHECK: }


public struct S2: Sequence {
  public func makeIterator() -> Scalar<Int>.I2<Bool> {
    return .init()
  }
}

// CHECK: public struct S2 : Swift.Sequence {
// CHECK:   public func makeIterator() -> VariadicOpaqueResultTypes.Scalar<Swift.Int>.I2<Swift.Bool>
// CHECK:   public typealias Element = @_opaqueReturnTypeOf("$s25VariadicOpaqueResultTypes6ScalarV2I2V4nextQrSgyF", 0) __<Swift.Int, Swift.Bool>
// CHECK:   public typealias Iterator = VariadicOpaqueResultTypes.Scalar<Swift.Int>.I2<Swift.Bool>
// CHECK: }


///
/// The remaining examples use the new nested syntax.
///

// CHECK: public struct Variadic<each T> {
public struct Variadic<each T> {
  public struct I3<each U>: IteratorProtocol {
    public mutating func next() -> (some Any)? { return 3 }
  }

  // CHECK: public struct I3<each U> : Swift.IteratorProtocol {
  // CHECK:   public mutating func next() -> (some Any)?
  // CHECK:   public typealias Element = @_opaqueReturnTypeOf("$s25VariadicOpaqueResultTypes0A0V2I3V4nextQrSgyF", 0) __<repeat each T>.__<repeat each U>
  // CHECK: }

  public struct Middle {
    public struct Inner<each U> {
      public struct I4: IteratorProtocol {
        public mutating func next() -> (some Any)? { return 3 }
      }
    }
  }

  // CHECK: public struct Middle {
  // CHECK:   public struct Inner<each U> {
  // CHECK:     public struct I4 : Swift.IteratorProtocol {
  // CHECK:       public mutating func next() -> (some Any)?       
  // CHECK:       public typealias Element = @_opaqueReturnTypeOf("$s25VariadicOpaqueResultTypes0A0V6MiddleV5InnerV2I4V4nextQrSgyF", 0) __<repeat each T>.__<repeat each U>
  // CHECK:     }
  // CHECK:   }
  // CHECK: }
}
// CHECK: }

// CHECK: public struct S3 : Swift.Sequence {
// CHECK:   public func makeIterator() -> VariadicOpaqueResultTypes.Variadic<Swift.Int, Swift.Bool>.I3<Swift.Float, Swift.Double>
// CHECK:   public typealias Element = @_opaqueReturnTypeOf("$s25VariadicOpaqueResultTypes0A0V2I3V4nextQrSgyF", 0) __<Swift.Int, Swift.Bool>.__<Swift.Float, Swift.Double>
// CHECK:   public typealias Iterator = VariadicOpaqueResultTypes.Variadic<Swift.Int, Swift.Bool>.I3<Swift.Float, Swift.Double>
// CHECK: }
public struct S3: Sequence {
  public func makeIterator() -> Variadic<Int, Bool>.I3<Float, Double> {
    return .init()
  }
}

// CHECK: public struct S4 : Swift.Sequence {
// CHECK:   public func makeIterator() -> VariadicOpaqueResultTypes.Variadic<Swift.Int, Swift.Bool>.Middle.Inner<Swift.Float, Swift.Double>.I4
// CHECK:   public typealias Element = @_opaqueReturnTypeOf("$s25VariadicOpaqueResultTypes0A0V6MiddleV5InnerV2I4V4nextQrSgyF", 0) __<Swift.Int, Swift.Bool>.__<Swift.Float, Swift.Double>
// CHECK:   public typealias Iterator = VariadicOpaqueResultTypes.Variadic<Swift.Int, Swift.Bool>.Middle.Inner<Swift.Float, Swift.Double>.I4
// CHECK: }
public struct S4: Sequence {
  public func makeIterator() -> Variadic<Int, Bool>.Middle.Inner<Float, Double>.I4 {
    return .init()
  }
}

