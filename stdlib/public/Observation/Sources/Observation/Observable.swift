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
public protocol Observable: Identifiable {
  associatedtype Token
  
  func addObserver(
    _ observer: some Observer<Self>, 
    for members: MemberKeyPaths<Self>, 
    using transactionModel: some ObservationTransactionModel
  ) -> Token
  func removeObserver(_ observation: Token)
  func memberKeyPaths(for keyPath: PartialKeyPath<Self>) -> MemberKeyPaths<Self>
}

@available(SwiftStdlib 5.9, *)
extension Observable {
  public func addObserver<O: Observer>(
    _ observer: O, 
    for fields: MemberKeyPaths<Self>
  ) -> Token where O.Subject == Self {
    addObserver(observer, 
      for: fields, 
      using: ImmediateObservationTransactionModel())
  }
  
  public func addObserver<Member>(
    _ observer: some Observer<Self>, 
    for keyPath: KeyPath<Self, Member>, 
    using transactionModel: some ObservationTransactionModel
  ) -> Token {
    addObserver(observer, for: [keyPath], using: transactionModel)
  }
  
  public func addObserver<O: Observer, Member>(
    _ observer: O, 
    for keyPath: KeyPath<Self, Member>
  ) -> Token where O.Subject == Self {
    addObserver(observer, 
      for: keyPath, 
      using: ImmediateObservationTransactionModel())
  }
  
  public func memberKeyPaths(
    for keyPath: PartialKeyPath<Self>
  ) -> MemberKeyPaths<Self> {
    return [keyPath]
  }
}





