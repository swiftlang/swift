// RUN: %target-typecheck-verify-swift -disable-availability-checking

func escape(_ closure: @escaping () -> Void) {}
func noescape(_ closure: () -> Void) {}

class SomeClass {}

class TestImplicitWeakToStrongCaptures {
  func test_self() {
    escape { // expected-note {{'self' implicitly captured here}}
      escape { [weak self] in  // expected-warning {{'weak' capture list item 'self' implicitly captured as strong reference in outer scope}}
        _ = self
      }
    }
  }

  func test_locals() {
    let a = SomeClass()
    escape { // expected-note {{'a' implicitly captured here}}
      let b = SomeClass()
      escape { [
        weak a, // expected-warning {{'weak' capture list item 'a' implicitly captured as strong reference in outer scope}}
        weak b
      ] in
        _ = a
        _ = b
      }
    }
  }

  func test_flavors_of_weak() {
    escape {  // expected-note 3 {{'self' implicitly captured here}}
      escape { [weak self] in _ = self }    // expected-warning {{'weak' capture list item 'self' implicitly captured as strong reference in outer scope}}
      escape { [unowned self] in _ = self } // expected-warning {{'unowned' capture list item 'self' implicitly captured as strong reference in outer scope}}
      escape { [unowned(unsafe) self] in _ = self } // expected-warning {{'unowned(unsafe)' capture list item 'self' implicitly captured as strong reference in outer scope}}
    }
  }

  func test_binding_new_name() {
    escape {  // expected-note {{'self' implicitly captured here}}
      escape { [weak welf = self] in _ = welf }    // expected-warning {{'weak' capture list item 'self' implicitly captured as strong reference in outer scope}}
    }
  }

  func test_nonescaping() {
    escape {  // expected-note {{'self' implicitly captured here}}
      noescape {
        escape { [weak self] in _ = self }  // expected-warning {{'weak' capture list item 'self' implicitly captured as strong reference in outer scope}}
      }
    }
  }

  func test_cases_that_should_not_be_diagnosed() {
    escape {
      _ = self  // self is referenced in the outer closure
      escape { [weak self] in
        _ = self
      }
    }

    escape {
      escape { [weak self] in _ = self }
      _ = self  // self is referenced in the outer closure
    }

    escape { [self] in
      escape { [weak self] in _ = self }
    }

    escape {
      noescape { [self] in // no warning since there is an explicit capture
        escape { [weak self] in _ = self }
      }
    }

    noescape { // no warning expected since implicit capture is in a non-escaping closure
      escape { [weak self] in _ = self }
    }

    noescape {
      let obj = SomeClass()

      weak var thing1 = obj
      unowned let thing2 = obj
      unowned(unsafe) let thing3 = obj

      escape {  // no warning since the captures are already non-strong
        escape { [weak thing1] in _ = thing1 }
        escape { [unowned thing2] in _ = thing2 }
        escape { [unowned(unsafe) thing3] in _ = thing3 }

        // and changing the reference ownership of less-than-strong capture shouldn't matter
        escape { [unowned thing1] in _ = thing1 }
        escape { [unowned(unsafe) thing2] in _ = thing2 }
        escape { [weak thing3] in _ = thing3 }
      }
    }
  }
}
