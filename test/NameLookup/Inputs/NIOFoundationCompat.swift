import Foundation

extension Data {
  @inlinable
  public func withUnsafeBytes<R>(_ body: (UnsafeRawBufferPointer) throws -> R) rethrows -> R {
    let r: R? = nil
    return r!
  }
}
