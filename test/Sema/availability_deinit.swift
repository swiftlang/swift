// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.50

// REQUIRES: OS=macosx

@available(*, unavailable)
class Unavailable {
  deinit {}
}

@available(*, unavailable)
class UnavailableWithUnavailableDeinit {
  @available(*, unavailable)
  deinit {}
}

@available(*, unavailable)
enum UnavailableEnum {
  class NestedWithUnavailableDeinit {
    @available(*, unavailable)
    deinit {}
  }
}

class DeinitUnavailable {
  @available(*, unavailable) // expected-error {{deinitializer cannot be marked unavailable with '@available'}}
  deinit {}
}

class DeinitUnavailableMacOS {
  @available(macOS, unavailable) // expected-error {{deinitializer cannot be marked unavailable with '@available'}}
  deinit {}
}

class AvailableAtDeploymentTargetDeinit {
  @available(macOS 10.50, *)
  deinit {}
}

class PotentiallyUnavailableDeinit {
  @available(macOS 10.51, *) // expected-error {{deinitializer cannot be marked potentially unavailable with '@available'}}
  deinit {}
}

@available(macOS 10.51, *)
func funcAvailable10_51() {}

class AlwaysAvailable { // expected-note {{add @available attribute to enclosing class}}
  deinit {
    funcAvailable10_51() // expected-error {{'funcAvailable10_51()' is only available in macOS 10.51 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}
  }
}
