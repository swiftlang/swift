// RUN: %target-swift-frontend %s -emit-silgen \
// RUN:   -enable-experimental-feature Lifetimes

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Lifetimes


public struct Span<Element> : ~Escapable {
  private var baseAddress: UnsafeRawPointer
  public let count: Int
  @_lifetime(copy owner)
  public init<Owner: ~Copyable & ~Escapable>(
      baseAddress: UnsafeRawPointer,
      count: Int,
      dependsOn owner: borrowing Owner
    ) {
    self.baseAddress = baseAddress
    self.count = count
  }
}

extension ContiguousArray {
  public var span: Span<Element> {
    borrowing _read {
      yield Span(
        baseAddress: _baseAddressIfContiguous!, count: count, dependsOn: self
      )
    }
  }
}

// CHECK-LABEL: sil hidden @$s39lifetime_dependence_param_position_test11mayReassign4span2toyAA4SpanVySiGz_s15ContiguousArrayVySiGtF : $@convention(thin) (_lifetime(_borrow 1) @inout Span<Int>, @guaranteed ContiguousArray<Int>) -> () {
@_lifetime(span: borrow to)
func mayReassign(span: inout Span<Int>, to: ContiguousArray<Int>) {
  span = to.span
}

