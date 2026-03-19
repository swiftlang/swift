//===--- BorrowingIteratorAlgorithms.swift --------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// map(_:)
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.4, *)
public struct BorrowingMap<Iterator: BorrowingIteratorProtocol, ResultElement>: ~Copyable, ~Escapable
  where Iterator: ~Copyable & ~Escapable, Iterator.Element: ~Copyable, ResultElement: ~Copyable
{
  var iterator: Iterator
  var transform: (borrowing Iterator.Element) -> ResultElement
  
  @_lifetime(copy iterator)
  init(
    iterator: consuming Iterator,
    transform: @escaping (borrowing Iterator.Element) -> ResultElement
  ) {
    self.iterator = iterator
    self.transform = transform
  }
}

//extension BorrowingMap: BorrowingSequence where Self: ~Copyable & ~Escapable, ResultElement: ~Copyable {
//  typealias Element = ResultElement
//  
//  
//  struct BorrowingIterator: BorrowingIteratorProtocol, ~Escapable {
//    var iterator: Iterator
//    var transform: (borrowing Iterator.Element) -> ResultElement
//    
//    mutating func nextSpan() -> Span<ResultElement> {
//      
//    }
//  }
//}

@available(SwiftStdlib 6.4, *)
extension BorrowingIteratorProtocol where Self: ~Copyable & ~Escapable, Element: ~Copyable {
  @_lifetime(copy self)
  public consuming func map<ResultElement>(
    _ transform: @escaping (borrowing Element) -> ResultElement
  ) -> BorrowingMap<Self, ResultElement>
    where ResultElement: ~Copyable
  {
    .init(iterator: self, transform: transform)
  }
}
