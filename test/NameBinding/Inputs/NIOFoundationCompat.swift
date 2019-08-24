import Foundation

extension Data {
  @_inlineable
  public func withUnsafeBytes<R>(_ body: (UnsafeRawBufferPointer) throws -> R) rethrows -> R {
    let r: R? = nil
    return r!
  }
}
