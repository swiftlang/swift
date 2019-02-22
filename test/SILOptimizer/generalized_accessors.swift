// RUN: %target-swift-frontend -emit-sil -primary-file %s -o /dev/null -verify
//
// Tests for yield-once diagnostics emitted for generalized accessors.

struct TestNoYield {
  var computed: Int {
    _read {
    } // expected-error {{accessor must yield before returning}}

    _modify {
    } // expected-error {{accessor must yield before returning}}
  }
}

struct TestReturnPathWithoutYield {
  var stored: Int
  var flag: Bool

  var computed: Int {
    mutating _read {
      if flag {
        yield stored // expected-note {{yield along one path is here}}
      }
      flag = true
    } // expected-error {{accessor must yield on all paths before returning}}

    _modify {
      // Diagnostics should attach a note to the earliest conflicting branch.
      if flag {
        yield &stored // expected-note {{yield along one path is here}}
      }

      if !flag {
        flag = true
      }
    } // expected-error {{accessor must yield on all paths before returning}}
  }
}

struct TestIfElseWithoutYield {
  var stored: Int
  var flag: Bool

  var computed: Int {
    mutating _read {
      if flag {
        yield stored // expected-note {{yield along one path is here}}
      } else {
        flag = true
      }
    } // expected-error {{accessor must yield on all paths before returning}}

    _modify {
      if flag {
        flag = true
      } else {
        yield &stored // expected-note {{yield along one path is here}}
      }
    } // expected-error {{accessor must yield on all paths before returning}}
  }
}


struct TestMultipleYields {
  var stored: Int
  var flag: Bool
  var computed: Int {
    _read {
      if flag {
        yield stored // expected-note {{previous yield was here}}
      }
      yield stored // expected-error {{accessor must not yield more than once}}
    }

    _modify {
      yield &stored // expected-note {{previous yield was here}}
      if flag {
        yield &stored // expected-error {{accessor must not yield more than once}}
      }
    }
  }
}

struct TestMultipleYields2 {
  var stored: Int
  var flag: Bool
  var computed: Int {
    _read {
      if flag {
        yield stored
        return
      }
      yield stored
    }

    _modify {
      if flag {
        yield &stored
      } else {
        yield &stored
      }
    }
  }
}

 struct TestConvolutedPaths {
  var flag: Bool
  var stored : Int

  var computed: Int {
    _read {
      if flag {
        yield stored // expected-note {{previous yield was here}}
      }
      if !flag {
        yield stored // expected-error {{accessor must not yield more than once}}
      }
    }

    _modify {
      if flag {
        yield &stored // expected-note {{previous yield was here}}
      }
      if !flag {
        yield &stored // expected-error {{accessor must not yield more than once}}
      }
    }
  }
}

struct TestYieldInLoops {
  var stored: Int
  var count: Int
  var computed: Int {
    _read {
      for _ in 0..<count {
        yield stored // expected-error {{accessor must not yield more than once}}
                     // expected-note@-1 {{previous yield was here}}
      }
    }

    _modify {
      yield &stored // expected-note {{previous yield was here}}
      for _ in 0..<count {
        yield &stored // expected-error {{accessor must not yield more than once}}
      }
    }
  }
}

enum CompassPoint {
  case north
  case south
  case east
  case west
}

struct TestYieldInSwitch {
  var stored: Int
  var cp: CompassPoint

  var computed: Int {
    get { return stored }
    _modify {
      switch cp {
      case .north:
        yield &stored // expected-note {{yield along one path is here}}
      case .south:
        stored = 10
      case .east:
        yield &stored
      case .west:
        stored = 12
      }
      cp = .north
    } // expected-error {{accessor must yield on all paths before returning}}
  }
}

struct TestYieldInSwitchWithFallThrough {
  var stored: Int
  var cp: CompassPoint

  var computed: Int {
    _read {
      switch cp {
      case .north:
        fallthrough
      case .south:
        fallthrough
      case .east:
        fallthrough
      case .west:
        yield stored
      }
    }

    _modify {
      switch cp {
      case .north:
        fallthrough
      case .south:
        yield &stored // expected-note {{yield along one path is here}}
      case .east:
        fallthrough
      case .west:
        stored = 12
      }
    } // expected-error {{accessor must yield on all paths before returning}}
  }
}

struct TestExplicitReturn {
  var stored: Int
  var flag: Bool

  var computed: Int {
    mutating _read {
      if stored > 0 {
        return
      }
      if flag {
        yield stored // expected-note {{yield along one path is here}}
      } else {
        yield stored
      }
      flag = true
    } // expected-error {{accessor must yield on all paths before returning}}

    _modify {
      if stored > 0 {
        return
      }
      if stored == 0 {
        if flag {
          yield &stored // expected-note {{yield along one path is here}}
        } else {
          yield &stored
        }
        return
      }
      flag = true
    } // expected-error {{accessor must yield on all paths before returning}}
  }

  var anotherProp: Int {
    mutating _read {
      if flag {
        stored = 2
      }
      return // expected-error {{accessor must yield before returning}}

      if !flag { // expected-warning {{code after 'return' will never be executed}}
        stored = 3
      }
    }
  }
}

struct TestYieldsInGuards {
  var storedOpt: Int?
  var defaultStorage: Int

  var computed: Int {
    _read {
      guard let stored = storedOpt else {
        yield defaultStorage
        return
      }
      yield stored
    }
  }
}

struct TestYieldsInGuards2 {
  var storedOpt: Int?
  var defaultStorage: Int

  var computed: Int {
    _read {
      guard let stored = storedOpt else {
        return
      }
      yield stored // expected-note {{yield along one path is here}}
    } // expected-error {{accessor must yield on all paths before returning}}

    _modify {
      guard let stored = storedOpt else {
        yield &defaultStorage // expected-note {{yield along one path is here}}
        return
      }
      storedOpt = stored + 1
    } // expected-error {{accessor must yield on all paths before returning}}
  }
}

enum SoftError : Error {
  case Ignorable
}
func aThrowingFunction() throws {
  throw SoftError.Ignorable
}

struct TestYieldInDoCatch {
  var stored: Int
  var computed: Int {
    _read {
      do {
        try aThrowingFunction()
        yield stored
      } catch {
        yield stored
      }
    }
  }
}

struct TestYieldInDoCatch2 {
  var stored: Int
  var computed: Int {
    _read {
      do {
        try aThrowingFunction()
        yield stored  // expected-note {{yield along one path is here}}
      } catch {
      }
    } // expected-error {{accessor must yield on all paths before returning}}

    _modify {
      do {
        try aThrowingFunction()
      }
      catch {
        yield &stored  // expected-note {{yield along one path is here}}
      }
    } // expected-error {{accessor must yield on all paths before returning}}
  }
}

enum Binary {
  case one
  case two
}

struct TestMutipleTries {
  var stored: Int
  var computed: Int {
    _read {
      do {
        try aThrowingFunction()
        yield stored // expected-note {{previous yield was here}}

        try aThrowingFunction()
        yield stored // expected-error {{accessor must not yield more than once}}
      } catch {
      }
    }

    _modify {
      do {
        try aThrowingFunction()
        yield &stored           // expected-note {{yield along one path is here}}

        try aThrowingFunction()
      }
      catch {
      }
    } // expected-error {{accessor must yield on all paths before returning}}
  }

  var flag: Bool
  var bin: Binary

  var computed2: Int {
    _read {
      do {
        if flag {
          try aThrowingFunction()
        } else {
          try aThrowingFunction()
        }

        yield stored // expected-note {{yield along one path is here}}
      } catch {
      }
    } // expected-error {{accessor must yield on all paths before returning}}

    _modify {
      do {
        switch bin {
        case .one:
          try aThrowingFunction()
          yield &stored // expected-note {{yield along one path is here}}

        case .two:
          try aThrowingFunction()
          yield &stored
        }
      } catch {
      }
    } // expected-error {{accessor must yield on all paths before returning}}
  }

  var computed3: Int {
    _read {
      do {
        if flag {
          try aThrowingFunction()
        } else {
          try aThrowingFunction()
        }
      } catch {
        yield stored // expected-note {{yield along one path is here}}
      }
    } // expected-error {{accessor must yield on all paths before returning}}

    _modify {
      do {
        switch bin {
        case .one:
          try aThrowingFunction()
        case .two:
          try aThrowingFunction()
        }
      } catch {
        yield &stored // expected-note {{yield along one path is here}}
      }
    } // expected-error {{accessor must yield on all paths before returning}}
  }
}

struct TestMutipleCatches {
  enum NumError: Error {
    case One
    case Two
    case Three
  }

  var stored: Int
  var computed: Int {
    _read {
      do {
        try aThrowingFunction()
        yield stored            // expected-note {{yield along one path is here}}
      } catch NumError.One {
        yield stored
      } catch NumError.Two {
        yield stored
      } catch NumError.Three {
      } catch {
        yield stored
      }
    }  // expected-error {{accessor must yield on all paths before returning}}
  }
}

// Test for labeled breaks.

struct TestYieldWithLabeledBreaks {
  var stored: Int
  var flag: Bool
  var bin: Binary

  var computed: Int {
    _read {
      ifstmt: if flag {
        switch bin {
        case .one:
          yield stored // expected-note {{yield along one path is here}}
        case .two:
          break ifstmt
        }
      }
    } // expected-error {{accessor must yield on all paths before returning}}

    _modify {
      loop: while flag {
        switch bin {
        case .one:
          yield &stored // expected-error {{accessor must not yield more than once}}
                        // expected-note@-1 {{previous yield was here}}
        case .two:
          break loop
        }
      }
    }
  }
}
