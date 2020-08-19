// RUN: %target-swift-frontend -emit-ir %s

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
