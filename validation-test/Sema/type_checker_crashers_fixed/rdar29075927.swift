// RUN: not %target-swift-frontend %s

typealias Element<S: Sequence> = S.Iterator.Element

protocol StringProtocol {
  associatedtype Content
  associatedtype UnicodeScalars : BidirectionalCollection
  var unicodeScalars : UnicodeScalars { get }
  
  mutating func append<T: StringProtocol>(other: T)
  where Content == T.Content,
  Element<UnicodeScalars> == Element<T.UnicodeScalars>,
    Element<T.UnicodeScalars> == Unicode.Scalar
}

struct X : StringProtocol {
  typealias Content = Int
  var unicodeScalars: [Unicode.Scalar]

  mutating func append<T: StringProtocol>(other: T)
  where Content == T.Content,
    T.UnicodeScalars.Iterator.Element == Unicode.Scalar
  {
    print(other.unicodeScalars.first!)
  }
}
