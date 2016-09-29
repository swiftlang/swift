@_exported import CoreData
import Foundation

extension NSManagedObjectContext {
  public func fetch<T: NSFetchRequestResult>(_ request: NSFetchRequest<T>) throws -> [T] {
    return try fetch(unsafeBitCast(request, to: NSFetchRequest<NSFetchRequestResult>.self)) as! [T]
  }

  public func count<T: NSFetchRequestResult>(for request: NSFetchRequest<T>) throws -> Int {
    return try count(for: unsafeBitCast(request, to: NSFetchRequest<NSFetchRequestResult>.self))
  }
}
