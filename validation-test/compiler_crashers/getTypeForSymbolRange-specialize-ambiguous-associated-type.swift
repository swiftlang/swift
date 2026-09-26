// {"kind":"typecheck","signature":"getTypeForSymbolRange(swift::rewriting::Symbol const*, swift::rewriting::Symbol const*, llvm::ArrayRef<swift::GenericTypeParamType*>, swift::rewriting::PropertyMap const&)","signatureAssert":"Assertion failed: (std::find(conformsTo.begin(), conformsTo.end(), symbol.getProtocol()) != conformsTo.end()), function getTypeForSymbolRange"}
// RUN: not --crash %target-swift-frontend -typecheck %s

public protocol P1 {
  associatedtype DP1
  associatedtype DP11
}

public protocol P2 {
  associatedtype DP2: P1
}

public struct H<T> {}

public struct MyStruct3: P1 {
  public typealias DP1 = Int
  public typealias DP11 = H<Int>
}

public struct MyStruct4: P2 {
  public typealias DP2 = MyStruct3
}

@_specialize(where T == MyStruct4)
public func foo<T: P2>(_ t: T) where T.DP2.DP11 == H<T.DP2.DP1> {}

struct MyStruct3 {}
