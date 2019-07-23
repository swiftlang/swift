// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1 -solver-disable-shrink -disable-constraint-solver-performance-hacks -solver-enable-operator-designated-types
// REQUIRES: tools-release,no_asserts

func test(header_field_mark: Bool?, header_value_mark: Bool?,
  url_mark: Bool?, body_mark: Bool?, status_mark: Bool?) {
  assert(((header_field_mark != nil ? 1 : 0) +
      (header_value_mark != nil ? 1 : 0) +
      (url_mark != nil ? 1 : 0)  +
      (body_mark != nil ? 1 : 0) +
      (status_mark != nil ? 1 : 0)) <= 1)
}
