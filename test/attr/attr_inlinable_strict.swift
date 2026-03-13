// RUN: %target-typecheck-verify-swift -enable-library-evolution -enable-upcoming-feature StrictAccessControl

// REQUIRES: swift_feature_StrictAccessControl

public struct HasUsableFromInlinePrivateSetProperty {
  @usableFromInline private(set) var bytes: UnsafeMutableRawPointer // expected-note 2 {{setter for property 'bytes' is not '@usableFromInline' or public}}
  public init() {
      self.bytes = UnsafeMutableRawPointer.allocate(byteCount: 1024, alignment: 8)
  }
  @usableFromInline
  func modifyPointer(_ ptr: inout UnsafeMutableRawPointer) {
    ptr = UnsafeMutableRawPointer.allocate(byteCount: 1, alignment: 1)
  }
  @usableFromInline
  func readPointer(_ ptr: UnsafeMutableRawPointer) {
    _ = ptr
  }
  // writes should trigger diagnostic
  @inlinable
  public mutating func writeDirect() {
      self.bytes = UnsafeMutableRawPointer.allocate(byteCount: 2048, alignment: 8) // expected-error {{setter for property 'bytes' is private and cannot be referenced from an '@inlinable' function}}
  }
  @inlinable
  public mutating func writeFunc() {
    modifyPointer(&self.bytes) // expected-error {{setter for property 'bytes' is private and cannot be referenced from an '@inlinable' function}}
  }
  // reads should be ok
  @inlinable
  public func usesBytes() -> UnsafeMutableRawPointer {
    _ = self.bytes
  }
  @inlinable
  public func readsViaLoad() -> Int {
    return self.bytes.load(as: Int.self)
  }
  @inlinable
  public func readsViaFunc() {
    readPointer(self.bytes)  // OK
  }
}
