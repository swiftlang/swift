// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature LifetimeDependence

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_LifetimeDependence

struct Span<T>: ~Escapable {
  private var base: UnsafePointer<T>
  private var count: Int

  @_unsafeNonescapableResult
  init(base: UnsafePointer<T>, count: Int) {
    self.base = base
    self.count = count
  }

  @lifetime(borrow generic)
  init<S>(base: UnsafePointer<T>, count: Int, generic: borrowing S) {
    self.base = base
    self.count = count
  }
}

struct Wrapper<T: BitwiseCopyable>: ~Escapable {
  private let span: Span<T>

  @lifetime(copy span)
  init(span: borrowing Span<T>) {
    self.span = copy span
  }
}

struct SuperWrapper<T: BitwiseCopyable>: ~Escapable {
  private let wrapper: Wrapper<T>

  // An extra field forces a projection on 'self' within the initializer without any access scope.
  var depth: Int = 0

  // Make sure that LocalVariableUtils can successfully analyze 'self'. That's required to determine that the assignment
  // of `wrapper` is returned without escaping
  @lifetime(copy span)
  init(span: borrowing Span<T>) {
    self.wrapper = Wrapper(span: span)
  }
}
