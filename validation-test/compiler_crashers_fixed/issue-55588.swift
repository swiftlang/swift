// RUN: %target-swift-frontend -emit-ir %s

// https://github.com/apple/swift/issues/55588

public protocol Book {
  associatedtype Name
}
public protocol BookDecorator: Book where Name == DecoratedBook.Name {
  associatedtype DecoratedBook: Book
  associatedtype Name = DecoratedBook.Name
}
public class ConcreteBookDecorator<T: Book>: BookDecorator {
  public typealias DecoratedBook = T
}
