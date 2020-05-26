// RUN: %target-run-simple-swift(-O)
// REQUIRES: executable_test

// We were crashing here due to not preserving rc identity. 
// rdar://41328987

func takeEscaping(closure: @escaping (String) -> Void) {}

public class Helper {
  weak var o: P?

  @_optimize(none)
  init(o: P) {
    self.o = o
  }
}

protocol P: class {}

public class Binding: P {
  private var helper: Helper?

  public init() {
    helper = Helper(o: self)
    
    // Listen to model changes
    takeEscaping { [unowned self] (value: String) in
      self.update()
    }

    takeEscaping { [unowned self] (value: String) in
      self.update()
    }
  }

  func update() {}
}

@_optimize(none)
func testCrash() {
  _ = Binding()
}

testCrash()
