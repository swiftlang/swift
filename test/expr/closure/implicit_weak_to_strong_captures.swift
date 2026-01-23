// RUN: %target-typecheck-verify-swift

func escape(_ closure: @escaping () -> Void) {}
func noescape(_ closure: () -> Void) {}
func autoescape<T>(_ e: @autoclosure @escaping () -> T) {}

class SomeClass {}
let globalObject: AnyObject = 0 as AnyObject

class TestImplicitWeakToStrongCaptures {
  let field: AnyObject = 0 as AnyObject

  func test_self() {
    escape {  // expected-note {{'self' implicitly strongly captured here}}
              // expected-note @-1 {{add 'self' as a capture list item to silence}}
      escape { [weak self] in  // expected-warning {{'weak' ownership of capture 'self' differs from implicitly-captured strong reference in outer scope}}
      // expected-note @-1 {{explicitly assign the capture list item to silence}}
        _ = self
      }
    }
  }

  func test_locals() {
    let a = SomeClass()
    escape {  // expected-note {{'a' implicitly strongly captured here}}
              // expected-note @-1 {{add 'a' as a capture list item to silence}}
      let b = SomeClass()
      escape { [
        weak a, // expected-warning {{'weak' ownership of capture 'a' differs from implicitly-captured strong reference in outer scope}}
        // expected-note @-1 {{explicitly assign the capture list item to silence}}
        weak b
      ] in
        _ = a
        _ = b
      }
    }
  }

  func test_flavors_of_weak() {
    escape {  // expected-note 3 {{'self' implicitly strongly captured here}}
              // expected-note @-1 3 {{add 'self' as a capture list item to silence}}
      escape { [weak self] in _ = self }    // expected-warning {{'weak' ownership of capture 'self' differs from implicitly-captured strong reference in outer scope}}
      // expected-note @-1 {{explicitly assign the capture list item to silence}}
      escape { [unowned self] in _ = self } // expected-warning {{'unowned' ownership of capture 'self' differs from implicitly-captured strong reference in outer scope}}
      // expected-note @-1 {{explicitly assign the capture list item to silence}}
      escape { [unowned(unsafe) self] in _ = self } // expected-warning {{'unowned(unsafe)' ownership of capture 'self' differs from implicitly-captured strong reference in outer scope}}
      // expected-note @-1 {{explicitly assign the capture list item to silence}}
    }
  }

  func test_binding_new_name() {
    escape {
      escape { [weak welf = self] in _ = welf }
    }
  }

  func test_nonescaping() {
    escape {  // expected-note {{'self' implicitly strongly captured here}}
              // expected-note @-1 {{add 'self' as a capture list item to silence}}
      noescape {
        escape { [weak self] in _ = self }  // expected-warning {{'weak' ownership of capture 'self' differs from implicitly-captured strong reference in outer scope}}
      // expected-note @-1 {{explicitly assign the capture list item to silence}}
      }
    }
  }

  func test_struct() {
    struct S {
      func f() {
        let obj = SomeClass()

        escape {  // expected-note {{'obj' implicitly strongly captured here}}
                  // expected-note @-1 {{add 'obj' as a capture list item to silence}}
          escape { [weak obj] in _ = obj } // expected-warning {{'weak' ownership of capture 'obj' differs from implicitly-captured strong reference in outer scope}}
          // expected-note @-1 {{explicitly assign the capture list item to silence}}
        }
      }
    }
  }

  static func test_static_method() {
    let obj = SomeClass()

    escape {  // expected-note {{'obj' implicitly strongly captured here}}
              // expected-note @-1 {{add 'obj' as a capture list item to silence}}
      escape { [weak obj] in _ = obj } // expected-warning {{'weak' ownership of capture 'obj' differs from implicitly-captured strong reference in outer scope}}
      // expected-note @-1 {{explicitly assign the capture list item to silence}}
    }
  }

  func test_multiple_nesting_levels() {
    escape {  // expected-note {{'self' implicitly strongly captured here}}
              // expected-note @-1 {{add 'self' as a capture list item to silence}}
      escape {
        escape {
          escape { [weak self] in _ = self } // expected-warning {{'weak' ownership of capture 'self' differs from implicitly-captured strong reference in outer scope}}
          // expected-note @-1 {{explicitly assign the capture list item to silence}}
        }
      }
    }
  }

  func test_mixed_capture_scenarios() {
    let obj1 = SomeClass()
    let obj2 = SomeClass()

    // expected-note @+2 {{'obj1' implicitly strongly captured here}}
    // expected-note @+1 {{'obj2' implicitly strongly captured here}}
    escape {
      // expected-note @-1 {{add 'obj1' as a capture list item to silence}}
      // expected-note @-2 {{add 'obj2' as a capture list item to silence}}
      _ = obj2
      // expected-warning @+2 {{'weak' ownership of capture 'obj1' differs from implicitly-captured strong reference in outer scope}}
      // expected-warning @+1 {{'weak' ownership of capture 'obj2' differs from implicitly-captured strong reference in outer scope}}
      escape { [weak obj1, weak obj2] in
      // expected-note @-1 {{explicitly assign the capture list item to silence}}
      // expected-note @-2 {{explicitly assign the capture list item to silence}}
        _ = obj1
        _ = obj2
      }
    }
  }

  func test_capture_with_parameters() {
    func innerFunc(param: SomeClass) {
      escape {  // expected-note {{'param' implicitly strongly captured here}}
                // expected-note @-1 {{add 'param' as a capture list item to silence}}
        escape { [weak param] in _ = param } // expected-warning {{'weak' ownership of capture 'param' differs from implicitly-captured strong reference in outer scope}}
        // expected-note @-1 {{explicitly assign the capture list item to silence}}
      }
    }
  }

  func test_computed_property_with_capture() {
    let obj = SomeClass()

    var computed: (() -> Void)? {
      get {
        escape {  // expected-note {{'obj' implicitly strongly captured here}}
                  // expected-note @-1 {{add 'obj' as a capture list item to silence}}
          escape { [weak obj] in _ = obj } // expected-warning {{'weak' ownership of capture 'obj' differs from implicitly-captured strong reference in outer scope}}
          // expected-note @-1 {{explicitly assign the capture list item to silence}}
        }
        return nil
      }
    }
  }

  func test_cases_that_should_not_be_diagnosed() {
    func test_explicit_binding_new_name() {
      escape {
        escape { [weak welf = self] in _ = welf }
      }
    }

    func test_explicit_binding_same_name() {
      escape {
        escape { [unowned self = self] in _ = self }
      }
    }

    // Explicit capture item in outer escaping closure
    escape { [self] in
      escape { [weak self] in _ = self }
    }

    // Explicit capture item in non-escaping intermediate closure
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

      weak var thing1 = obj; thing1 = obj
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

    escape {
      escape { [self] in
        escape { [weak self] in _ = self }
      }
    }

    escape { [self] in
      escape {
        escape { [weak self] in _ = self }
      }
    }

    // Captures in nested types should not trigger the warning
    escape {
      struct LocalType {
        func foo() {
          _ = { [weak globalObject] in
            _ = globalObject
          }
        }
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
      escape {  // expected-note {{'self' implicitly strongly captured here}}
                // expected-note @-1 {{add 'self' as a capture list item to silence}}
        escape { [weak self] in _ = self } // expected-warning {{'weak' ownership of capture 'self' differs from implicitly-captured strong reference in outer scope}}
        // expected-note @-1 {{explicitly assign the capture list item to silence}}
      }
      return nil
    }()

    // Capture in closure assigned to property
    var closureProp: (() -> Void)?

    func setupClosure() {
      closureProp = { // expected-note {{'self' implicitly strongly captured here}}
                      // expected-note @-1 {{add 'self' as a capture list item to silence}}
        escape {
          escape { [weak self] in _ = self } // expected-warning {{'weak' ownership of capture 'self' differs from implicitly-captured strong reference in outer scope}}
          // expected-note @-1 {{explicitly assign the capture list item to silence}}
        }
      }
      _ = closureProp
    }
  }

  func test_async_context() async {
    Task {  // expected-note {{'self' implicitly strongly captured here}}
            // expected-note @-1 {{add 'self' as a capture list item to silence}}
      Task { [weak self] in _ = self } // expected-warning {{'weak' ownership of capture 'self' differs from implicitly-captured strong reference in outer scope}}
      // expected-note @-1 {{explicitly assign the capture list item to silence}}{{24-24= = self}}
    }
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

  escape { // expected-note {{'obj' implicitly strongly captured here}}
    // expected-note @-1 {{add 'obj' as a capture list item to silence}}
    escape { [weak obj] in _ = obj } // expected-warning {{'weak' ownership of capture 'obj' differs from implicitly-captured strong reference in outer scope}}
    // expected-note @-1 {{explicitly assign the capture list item to silence}}{{23-23= = obj}}
  }
}
