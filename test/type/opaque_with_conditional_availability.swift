// RUN: %target-swift-frontend -target %target-cpu-apple-macosx10.15 -typecheck -verify %s
// REQUIRES: OS=macosx

protocol P {}

@available(macOS 10.16, *)
struct X : P {
}

struct Y : P {
}

struct Z : P {
}

// Ok: X is only available on 10.16, Y is universal
func test1() -> some P {
  if #available(macOS 10.16, *) {
    return X()
  }

  return Y()
}

func test2() -> some P {
  // expected-error@-1 {{function declares an opaque return type 'some P', but the return statements in its body do not have matching underlying types}}
  if #available(macOS 10.16, *) {
    return X()
  } else {
    return Y() // expected-note {{return statement has underlying type 'Y'}}
  }

  return Z() // expected-note {{return statement has underlying type 'Z'}}
}

func test_no_else_if() -> some P {
  // expected-error@-1 {{function declares an opaque return type 'some P', but the return statements in its body do not have matching underlying types}}
  if #available(macOS 10.16, *) {
    return X()
  } else if #available(macOS 10.17, *) {
    return Y() // expected-note {{return statement has underlying type 'Y'}}
  }

  return Z() // expected-note {{return statement has underlying type 'Z'}}
}

func test4(cond: Bool) -> some P {
  // expected-error@-1 {{function declares an opaque return type 'some P', but the return statements in its body do not have matching underlying types}}
  if cond {
    if #available(macOS 10.16, *) {
      return X() // expected-note {{return statement has underlying type 'X'}}
    }
  }

  return Y() // expected-note {{return statement has underlying type 'Y'}}
}

func test5(cond: Bool) -> some P {
  // expected-error@-1 {{function declares an opaque return type 'some P', but the return statements in its body do not have matching underlying types}}
  if #available(macOS 10.16, *) {
    if cond {
      return X() // expected-note {{return statement has underlying type 'X'}}
    }
  }

  return Y() // expected-note {{return statement has underlying type 'Y'}}
}

func test5(v: Int) -> some P {
  // expected-error@-1 {{function declares an opaque return type 'some P', but the return statements in its body do not have matching underlying types}}
  if #available(macOS 10.16, *), v > 42 {
    return X() // expected-note {{return statement has underlying type 'X'}}
  }

  return Y() // expected-note {{return statement has underlying type 'Y'}}
}

// Ok because availability conditions are conjoined.
func test_multiple_conditions() -> some P {
  if #available(macOS 10.16, *), #available(iOS 1000, *) {
    return X()
  }

  return Y()
}

func test_no_nested_availability_conditions() -> some P {
  // expected-error@-1 {{function declares an opaque return type 'some P', but the return statements in its body do not have matching underlying types}}
  if #available(macOS 10.16, *) {
    if #available(iOS 10000, *) {
      return X() // expected-note {{return statement has underlying type 'X'}}
    }
  }

  return Y() // expected-note {{return statement has underlying type 'Y'}}
}

// TODO(diagnostics): Need a tailored diagnostic in this case to point out that there is no unconditional `return`.
func test_fail_without_universally_available_type() -> some P {
  // expected-error@-1 {{function declares an opaque return type, but has no return statements in its body from which to infer an underlying type}}
  if #available(macOS 11, *) {
    return X()
  }

  if #available(macOS 12, *) {
    return Y()
  }
}

func test_unavailability_condition() -> some P {
  if #unavailable(macOS 12) {
    return Y()
  }

  if #available(macOS 16, *) {
    return X()
  }

  return Z()
}
