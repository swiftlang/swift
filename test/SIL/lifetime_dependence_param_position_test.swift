// RUN: %target-swift-frontend %s -emit-silgen \
// RUN:   -enable-experimental-feature NonescapableTypes

// REQUIRES: asserts
// REQUIRES: swift_in_compiler


public struct Span<Element> : ~Escapable {
  private var baseAddress: UnsafeRawPointer
  public let count: Int
  public init<Owner: ~Copyable & ~Escapable>(
      baseAddress: UnsafeRawPointer,
      count: Int,
      dependsOn owner: borrowing Owner
    ) {
      self.init(
        baseAddress: baseAddress, count: count, dependsOn: owner
      )
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

// CHECK-LABEL: sil hidden [ossa] @$s39lifetime_dependence_param_position_test11mayReassign4span2toyAA4SpanVySiGzYlsUS__s15ContiguousArrayVySiGtF : $@convention(thin) (@inout Span<Int>, @guaranteed ContiguousArray<Int>) -> _scope(1)  () {
func mayReassign(span: dependsOn(to) inout Span<Int>, to: ContiguousArray<Int>) {
  span = to.span
}

