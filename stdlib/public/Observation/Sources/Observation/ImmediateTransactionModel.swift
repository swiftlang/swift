//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

import Swift

@available(SwiftStdlib 5.9, *)
struct ImmediateObservationTransactionModel: ObservationTransactionModel {
  func register<O: Observer, Member>(
    transaction: inout ObservationTransaction<O.Subject>,
    observer: O,
    observable: O.Subject,
    willSet keyPath: KeyPath<O.Subject, Member>,
    to newValue: Member
  ) -> Bool {
    return true
  }
  
  func register<O: Observer, Member>(
    transaction: inout ObservationTransaction<O.Subject>,
    observer: O,
    observable: O.Subject,
    didSet keyPath: KeyPath<O.Subject, Member>
  ) -> Bool {
    transaction.addObserver(observer)
    return true
  }
  
  func commit<Subject>(
    transaction: inout ObservationTransaction<Subject>, 
    observable: Subject
  ) -> Bool where Subject : Observable {
    return true
  }
  
  func invalidate() {
    
  }
}
