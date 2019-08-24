// RUN: not %target-swift-frontend %s

typealias Element<S: Sequence> = S.Iterator.Element

protocol StringProtocol {
  associatedtype Content
  associatedtype UnicodeScalars : BidirectionalCollection
  var unicodeScalars : UnicodeScalars { get }
  
  mutating func append<T: StringProtocol>(other: T)
  where Content == T.Content,
  Element<UnicodeScalars> == Element<T.UnicodeScalars>,
    Element<T.UnicodeScalars> == UnicodeScalar
}

struct X : StringProtocol {
  typealias Content = Int
  var unicodeScalars: [UnicodeScalar]

  mutating func append<T: StringProtocol>(other: T)
  where Content == T.Content,
    T.UnicodeScalars.Iterator.Element == UnicodeScalar
  {
    print(other.unicodeScalars.first!)
  }
}
