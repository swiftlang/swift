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
public struct ObservationTransaction<Subject: Observable>: Hashable, Identifiable {
  let token: ObservationToken
  var members: MemberKeyPaths<Subject>
  let allocated: Bool
  var notifiers: [(Subject, MemberKeyPaths<Subject>) -> Void]
  
  internal init(allocated: Bool = false) {
    self.token = ObservationToken()
    self.members = MemberKeyPaths()
    self.allocated = allocated
    self.notifiers = [(Subject, MemberKeyPaths<Subject>) -> Void]()
  }
  
  public mutating func addObserver(_ observer: some Observer<Subject>) {
    notifiers.append(observer.changes(_:to:))
  }
  
  public func notifyObservers(_ subject: Subject) {
    for notifier in notifiers {
      notifier(subject, members)
    }
  }
  
  public var id: ObservationToken {
    return token
  }
  
  public func hash(into hasher: inout Hasher) {
    hasher.combine(token)
  }
  
  public static func == (_ lhs: ObservationTransaction<Subject>, _ rhs: ObservationTransaction<Subject>) -> Bool {
    return lhs.token == rhs.token
  }
}
