// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -o %t %t/lib.swift
// RUN: %target-swift-frontend -typecheck -primary-file %t/main.swift %t/other.swift -I %t -verify -enable-upcoming-feature MemberImportVisibility

// REQUIRES: swift_feature_MemberImportVisibility

//--- main.swift

import Swift
// expected-note 15 {{add import of module 'lib'}}

for _ in makeSequence() { }
// expected-error@-1 {{instance method 'makeIterator()' is not available due to missing import of defining module 'lib'}}
// expected-error@-2 {{instance method 'next()' is not available due to missing import of defining module 'lib'}}

takesNilExpressible(nil)
// expected-error@-1 {{initializer 'init(nilLiteral:)' is not available due to missing import of defining module 'lib'}}

takesIntExpressible(1)
// expected-error@-1 {{initializer 'init(integerLiteral:)' is not available due to missing import of defining module 'lib'}}

takesFloatExpressible(1.0)
// expected-error@-1 {{initializer 'init(floatLiteral:)' is not available due to missing import of defining module 'lib'}}

takesBoolExpressible(true)
// expected-error@-1 {{initializer 'init(booleanLiteral:)' is not available due to missing import of defining module 'lib'}}

takesUnicodeScalarExpressible("🐦")
// expected-error@-1 {{initializer 'init(unicodeScalarLiteral:)' is not available due to missing import of defining module 'lib'}}

takesExtendedGraphemeClusterExpressible("🦸🏾‍♀️")
// expected-error@-1 {{initializer 'init(extendedGraphemeClusterLiteral:)' is not available due to missing import of defining module 'lib'}}

takesStringLiteralExpressible("Hello world")
// expected-error@-1 {{initializer 'init(stringLiteral:)' is not available due to missing import of defining module 'lib'}}

takesArrayExpressible([1])
// expected-error@-1 {{initializer 'init(arrayLiteral:)' is not available due to missing import of defining module 'lib'}}

takesDictionaryExpressible(["one": 1])
// expected-error@-1 {{initializer 'init(dictionaryLiteral:)' is not available due to missing import of defining module 'lib'}}

takesMessage("\(1)")
// expected-error@-1 {{initializer 'init(stringInterpolation:)' is not available due to missing import of defining module 'lib'}}
// expected-error@-2 {{instance method 'appendInterpolation' is not available due to missing import of defining module 'lib'}}
// expected-error@-3 2 {{instance method 'appendLiteral' is not available due to missing import of defining module 'lib'}}

takesColorExpressible(#colorLiteral(red: 0.0, green: 0.0, blue: 0.0, alpha: 1))
// FIXME: Missing diangostic

takesImageExpressible(#imageLiteral(resourceName: "image.png"))
// FIXME: Missing diangostic

takesFileReferenceExpressible(#fileLiteral(resourceName: "file.txt"))
// FIXME: Missing diangostic

//--- other.swift

import lib

func makeSequence() -> EmptySequence {
  return MySequence()
}

func takesNilExpressible(_ x: NilExpressible) { }
func takesIntExpressible(_ x: IntExpressible) { }
func takesFloatExpressible(_ x: FloatExpressible) { }
func takesBoolExpressible(_ x: BoolExpressible) { }
func takesUnicodeScalarExpressible(_ x: UnicodeScalarExpressible) { }
func takesExtendedGraphemeClusterExpressible(_ x: ExtendedGraphemeClusterExpressible) { }
func takesStringLiteralExpressible(_ x: StringExpressible) { }
func takesArrayExpressible<E>(_ x: ArrayExpressible<E>) { }
func takesDictionaryExpressible<K, V>(_ x: DictionaryExpressible<K, V>) { }
func takesMessage(_ x: Message) { }
func takesColorExpressible(_ x: ColorExpressible) { }
func takesImageExpressible(_ x: ImageExpressible) { }
func takesFileReferenceExpressible(_ x: FileReferenceExpressible) { }

//--- lib.swift

public struct EmptySequence: Sequence {
  public struct Iterator: IteratorProtocol {
    public mutating func next() -> Int? { nil }
  }

  public func makeIterator() -> Iterator { Iterator() }
  public init() { }
}

public struct NilExpressible: ExpressibleByNilLiteral {
  public init(nilLiteral: ()) { }
}

public struct IntExpressible: ExpressibleByIntegerLiteral {
  public init(integerLiteral value: Int) { }
}

public struct FloatExpressible: ExpressibleByFloatLiteral {
  public init(floatLiteral value: Float) { }
}

public struct BoolExpressible: ExpressibleByBooleanLiteral {
  public init(booleanLiteral value: Bool) { }
}

public struct UnicodeScalarExpressible: ExpressibleByUnicodeScalarLiteral {
  public init(unicodeScalarLiteral value: Unicode.Scalar) { }
}

public struct ExtendedGraphemeClusterExpressible: ExpressibleByExtendedGraphemeClusterLiteral {
  public init(extendedGraphemeClusterLiteral value: Character) { }
}

public struct StringExpressible: ExpressibleByStringLiteral {
  public init(stringLiteral value: String) { }
}

public struct ArrayExpressible<Element>: ExpressibleByArrayLiteral {
  public init(arrayLiteral elements: Element...) { }
}

public struct DictionaryExpressible<Key, Value>: ExpressibleByDictionaryLiteral {
  public init(dictionaryLiteral elements: (Key, Value)...) { }
}

public struct MessageInterpolation: StringInterpolationProtocol {
  public init(literalCapacity: Int, interpolationCount: Int) { }
  public mutating func appendInterpolation(_ value: @autoclosure () -> Int) { }
  public mutating func appendLiteral(_ literal: String) { }
}

public struct Message: ExpressibleByStringInterpolation {
  public init(stringInterpolation: MessageInterpolation) { }
  public init(stringLiteral: String) { }
}

public struct ColorExpressible: _ExpressibleByColorLiteral {
  public init(_colorLiteralRed red: Float, green: Float, blue: Float, alpha: Float) { }
}

public struct ImageExpressible: _ExpressibleByImageLiteral {
  public init(imageLiteralResourceName path: String) { }
}

public struct FileReferenceExpressible: _ExpressibleByFileReferenceLiteral {
  public init(fileReferenceLiteralResourceName path: String) { }
}
