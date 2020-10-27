// RUN: %target-typecheck-verify-swift -swift-version 5

private struct PrivateType {}
// expected-note@-1 {{struct 'PrivateType' is not '@usableFromInline' or public}}
// expected-note@-2 {{initializer 'init()' is not '@usableFromInline' or public}}
// expected-note@-3 {{type declared here}}

@_fixed_layout public class UsesPrivateType {
  private var y1: PrivateType
  // expected-error@-1 {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}

  private var y2 = PrivateType()
  // expected-error@-1 {{struct 'PrivateType' is private and cannot be referenced from a property initializer in a '@frozen' type}}
  // expected-error@-2 {{initializer 'init()' is private and cannot be referenced from a property initializer in a '@frozen' type}}
  // expected-error@-3 {{type referenced from a stored property with inferred type 'PrivateType' in a '@frozen' struct must be '@usableFromInline' or public}}

  init() {}
}
