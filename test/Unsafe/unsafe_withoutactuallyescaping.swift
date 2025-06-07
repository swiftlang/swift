// RUN: %target-typecheck-verify-swift -strict-memory-safety

// REQUIRES: objc_interop

func callMe(body: @escaping @convention(block) () -> Void) { }

func callMeSwiftly(body: @escaping () -> Void) { }

func testCall(body: @convention(block) () -> Void, swiftBody: () -> Void) {
  // expected-warning@+2{{unsafe}}{{3-3=unsafe }}
  // expected-note@+1{{'withoutActuallyEscaping' function of type '@convention(block) () -> Void' is unsafe}}
  withoutActuallyEscaping(body) { nonescapingBody in
    callMe(body: nonescapingBody)
  }

  withoutActuallyEscaping(swiftBody) { nonescapingBody in
    callMeSwiftly(body: nonescapingBody)
  }
}
