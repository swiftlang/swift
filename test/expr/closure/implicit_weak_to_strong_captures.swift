// RUN: %target-typecheck-verify-swift -disable-availability-checking

func escape(_ closure: @escaping () -> Void) {}
func noescape(_ closure: () -> Void) {}
func autoescape<T>(_ e: @autoclosure @escaping () -> T) {}

class SomeClass {}

class TestImplicitWeakToStrongCaptures {
  func test_self() {
    escape { // expected-note {{'self' implicitly captured here}}
      escape { [weak self] in  // expected-warning {{'weak' capture implicitly captures 'self' as strong reference in outer scope}}
        _ = self
      }
    }
  }

  func test_locals() {
    let a = SomeClass()
    escape { // expected-note {{'a' implicitly captured here}}
      let b = SomeClass()
      escape { [
        weak a, // expected-warning {{'weak' capture implicitly captures 'a' as strong reference in outer scope}}
        weak b
      ] in
        _ = a
        _ = b
      }
    }
  }

  func test_flavors_of_weak() {
    escape {  // expected-note 3 {{'self' implicitly captured here}}
      escape { [weak self] in _ = self }    // expected-warning {{'weak' capture implicitly captures 'self' as strong reference in outer scope}}
      escape { [unowned self] in _ = self } // expected-warning {{'unowned' capture implicitly captures 'self' as strong reference in outer scope}}
      escape { [unowned(unsafe) self] in _ = self } // expected-warning {{'unowned(unsafe)' capture implicitly captures 'self' as strong reference in outer scope}}
    }
  }

  func test_binding_new_name() {
    escape {  // expected-note {{'self' implicitly captured here}}
      escape { [weak welf = self] in _ = welf }    // expected-warning {{'weak' capture 'welf' implicitly captures 'self' as strong reference in outer scope}}
    }
  }

  func test_nonescaping() {
    escape {  // expected-note {{'self' implicitly captured here}}
      noescape {
        escape { [weak self] in _ = self }  // expected-warning {{'weak' capture implicitly captures 'self' as strong reference in outer scope}}
      }
    }
  }

  func test_struct() {
    struct S {
      func f() {
        let obj = SomeClass()

        escape { // expected-note {{'obj' implicitly captured here}}
          escape { [weak obj] in _ = obj } // expected-warning {{'weak' capture implicitly captures 'obj' as strong reference in outer scope}}
        }
      }
    }
  }

  static func test_static_method() {
    let obj = SomeClass()

    escape { // expected-note {{'obj' implicitly captured here}}
      escape { [weak obj] in _ = obj } // expected-warning {{'weak' capture implicitly captures 'obj' as strong reference in outer scope}}
    }
  }

  func test_multiple_nesting_levels() {
    escape { // expected-note {{'self' implicitly captured here}}
      escape {
        escape {
          escape { [weak self] in _ = self } // expected-warning {{'weak' capture implicitly captures 'self' as strong reference in outer scope}}
        }
      }
    }
  }

  func test_mixed_capture_scenarios() {
    let obj1 = SomeClass()
    let obj2 = SomeClass()

    // expected-note @+2 {{'obj1' implicitly captured here}}
    // expected-note @+1 {{'obj2' implicitly captured here}}
    escape {
      _ = obj2
      // expected-warning @+2 {{'weak' capture implicitly captures 'obj1' as strong reference in outer scope}}
      // expected-warning @+1 {{'weak' capture implicitly captures 'obj2' as strong reference in outer scope}}
      escape { [weak obj1, weak obj2] in
        _ = obj1
        _ = obj2
      }
    }
  }

  func test_capture_with_parameters() {
    func innerFunc(param: SomeClass) {
      escape { // expected-note {{'param' implicitly captured here}}
        escape { [weak param] in _ = param } // expected-warning {{'weak' capture implicitly captures 'param' as strong reference in outer scope}}
      }
    }
  }

  func test_computed_property_with_capture() {
    let obj = SomeClass()

    var computed: (() -> Void)? {
      get {
        escape { // expected-note {{'obj' implicitly captured here}}
          escape { [weak obj] in _ = obj } // expected-warning {{'weak' capture implicitly captures 'obj' as strong reference in outer scope}}
        }
        return nil
      }
    }
  }

  func test_cases_that_should_not_be_diagnosed() {
    // Explicit capture item in outer escaping closure
    escape { [self] in
      escape { [weak self] in _ = self }
    }

    // Explicit capture item in non-escaping intermediate closure
    // TODO: this case seems odd... should it be treated differently?
    escape {
      noescape { [self] in // no warning since the capture's Decl is local to the parent closure
        escape { [weak self] in _ = self }
      }
    }

    // Top-level non-escaping closure
    noescape { // no warning expected since implicit capture is in a non-escaping closure
      escape { [weak self] in _ = self }
    }

    // Already weak/unowned references
    noescape {
      let obj = SomeClass()

      weak var thing1 = obj
      unowned let thing2 = obj
      unowned(unsafe) let thing3 = obj

      escape {  // no warning since the captures are already non-strong
        escape { [weak thing1] in _ = thing1 }
        escape { [unowned thing2] in _ = thing2 }
        escape { [unowned(unsafe) thing3] in _ = thing3 }

        // and changing the reference ownership of less-than-strong capture shouldn't warn
        escape { [unowned thing1] in _ = thing1 }
        escape { [unowned(unsafe) thing2] in _ = thing2 }
        escape { [weak thing3] in _ = thing3 }
      }
    }

    // Multiple explicit captures of same object
    escape { [self] in
      escape { [self] in
        escape { [weak self] in _ = self } // No warning - explicit capture chain
      }
    }
  }

  func test_edge_cases() {
    // Shadowing
    do {
      let obj = SomeClass()
      escape {
        _ = obj
        do {
          let obj = SomeClass()
          escape { [weak obj] in _ = obj }
        }
      }
    }

    // Capture in lazy property
    lazy var lazyProp: Int? = {
      escape { // expected-note {{'self' implicitly captured here}}
        escape { [weak self] in _ = self } // expected-warning {{'weak' capture implicitly captures 'self' as strong reference in outer scope}}
      }
      return nil
    }()

    // Capture in closure assigned to property
    var closureProp: (() -> Void)?

    func setupClosure() {
      closureProp = { // expected-note {{'self' implicitly captured here}}
        escape {
          escape { [weak self] in _ = self } // expected-warning {{'weak' capture implicitly captures 'self' as strong reference in outer scope}}
        }
      }
      _ = closureProp
    }
  }

  // TODO: how to deal with @_implicitSelfCapture and isolated captures?
  func test_async_context() async {
    Task { // expected-note {{'self' implicitly captured here}}
      Task { [weak self] in _ = self } // expected-warning {{'weak' capture implicitly captures 'self' as strong reference in outer scope}}
    }
  }

  func test_async_let() async {
    final class S: Sendable {}
    let s = S()
    // TODO: why doesn't this trigger the diagnostic?
    async let b: Bool = { [weak s] in
      s != nil
    }()
    _ = await b
  }

  func test_autoclosure() {
    autoescape(
      escape { [weak self] in
        _ = self
      }
    )
  }
}

func testGlobalFunction() {
  let obj = SomeClass()

  escape { // expected-note {{'obj' implicitly captured here}}
    escape { [weak obj] in _ = obj } // expected-warning {{'weak' capture implicitly captures 'obj' as strong reference in outer scope}}
  }
}
