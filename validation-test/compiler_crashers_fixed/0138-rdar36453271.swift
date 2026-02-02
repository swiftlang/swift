// RUN: %target-swift-frontend %s -emit-ir -o 0

extension Slice where Base == UnsafeBufferPointer<UInt16> {
  var rebased: UnsafeBufferPointer<UInt16> {
    return UnsafeBufferPointer(rebasing: self)
  }
}

extension Slice where Base == UnsafeMutableBufferPointer<UInt16> {
  var rebased: UnsafeMutableBufferPointer<UInt16> {
    return UnsafeMutableBufferPointer(rebasing: self)
  }
}
