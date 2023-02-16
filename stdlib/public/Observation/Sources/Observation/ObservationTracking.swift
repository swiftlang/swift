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
protocol ObservationTrackingEntry {
  mutating func addAccess(_ keyPath: AnyKeyPath)
  mutating func addObserver<TransactionModel: ObservationTransactionModel>(using transactionModel: TransactionModel, _ changed: @Sendable @escaping () -> Void)
  mutating func removeObserver()
}

@available(SwiftStdlib 5.9, *)
public struct ObservationTracking {
  struct EntryObserver<Subject: Observable>: Observer {
    let changed: @Sendable () -> Void
    
    init(_ changed: @Sendable @escaping () -> Void) {
      self.changed = changed
    }
    
    func changes(_ subject: Subject, to members: MemberKeyPaths<Subject>) {
      changed()
    }
  }
  
  struct Entry<Subject: Observable>: ObservationTrackingEntry {
    let subject: Subject
    var members: MemberKeyPaths<Subject>
    var token: Subject.Token?
    
    init(_ subject: Subject) {
      self.subject = subject
      self.members = MemberKeyPaths()
    }
    
    mutating func addAccess(_ keyPath: AnyKeyPath) {
      if let keyPath = keyPath as? PartialKeyPath<Subject> {
        members.insert(keyPath)
      }
    }
    
    mutating func addObserver<TransactionModel: ObservationTransactionModel>(using transactionModel: TransactionModel, _ changed: @Sendable @escaping () -> Void) {
      if let token {
        subject.removeObserver(token)
      }
      token = subject.addObserver(EntryObserver(changed), for: members, using: transactionModel)
    }
    
    mutating func removeObserver() {
      if let token {
        subject.removeObserver(token)
      }
      token = nil
    }
  }
  
  var entries: [any ObservationTrackingEntry]
  
  init(accessList: AccessList) {
    self.entries = Array(accessList.entries.values)
  }
  
  public mutating func addObserver<TransactionModel: ObservationTransactionModel>(using transactionModel: TransactionModel, _ changed: @Sendable @escaping () -> Void) {
    for idx in 0..<entries.count {
      entries[idx].addObserver(using: transactionModel, changed)
    }
  }
  
  public mutating func removeObserver() {
    for idx in 0..<entries.count {
      entries[idx].removeObserver()
    }
  }
  
  struct AccessList {
    var entries = [AnyHashable: any ObservationTrackingEntry]()
    
    mutating func addAccess<Subject: Observable>(propertyKeyPath: PartialKeyPath<Subject>, subject: Subject) {
      entries[subject.id, default: Entry(subject)].addAccess(propertyKeyPath)
    }
  }
  
  public static func registerAccess<Subject: Observable>(propertyKeyPath: PartialKeyPath<Subject>, subject: Subject) {
    if let trackingPtr = ThreadLocal[.trackingKey]?.assumingMemoryBound(to: AccessList?.self) {
      if trackingPtr.pointee == nil {
        trackingPtr.pointee = AccessList()
      }
      trackingPtr.pointee?.addAccess(propertyKeyPath: propertyKeyPath, subject: subject)
    }
  }

  public static func withTracking(_ apply: () -> Void) -> ObservationTracking? {
    var accessList: AccessList?
    withUnsafeMutablePointer(to: &accessList) { ptr in
      ThreadLocal[.trackingKey] = UnsafeMutableRawPointer(ptr)
      apply()
      ThreadLocal[.trackingKey] = nil
    }
    return accessList.map { ObservationTracking(accessList: $0) }
  }
}
