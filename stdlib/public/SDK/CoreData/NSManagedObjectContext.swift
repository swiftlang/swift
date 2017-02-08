//===----------------------------------------------------------------------===//
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

@_exported import CoreData
import Foundation

extension NSManagedObjectContext {
  public func fetch<T: NSFetchRequestResult>(_ request: NSFetchRequest<T>) throws -> [T] {
    return try fetch(unsafeDowncast(request, to: NSFetchRequest<NSFetchRequestResult>.self)) as! [T]
  }

  public func count<T: NSFetchRequestResult>(for request: NSFetchRequest<T>) throws -> Int {
    return try count(for: unsafeDowncast(request, to: NSFetchRequest<NSFetchRequestResult>.self))
  }
}
