// RUN: %target-typecheck-verify-swift  -target %target-swift-5.1-abi-triple

enum E : Error {
  case NotAvailable
}

class GolfCourse {
  var yards : Int {
    get throws { throw E.NotAvailable }
  }
  var holes : Int {
    get { 18 }
  }

  var par : Int {
    get async throws { 71 }
  }

  subscript(_ i : Int) -> Int {
    get throws { throw E.NotAvailable }
  }
}

class Presidio : GolfCourse {
  private var yardsFromBackTees = 6481
  override var yards : Int { // removes effect & makes it mutable
    get {
      do {
        return try super.yards
      } catch {
        return yardsFromBackTees
      }
    }
    set { yardsFromBackTees = newValue }
  }

  override var holes : Int {  // expected-error {{cannot override non-async property with async property}}
    get async { 18 }
  }

  override var par : Int {
    get async { 72 } // removes the 'throws' effect
  }

  override subscript(_ i : Int) -> Int { // removes effects
    get { (try? super[i]) ?? 3 }
  }
}

class PresidioBackNine : Presidio {
  override var par : Int { // expected-error{{cannot override non-throwing property with throwing property}}
    get throws { 36 } // attempts to put the 'throws' effect back
  }

  override subscript(_ i : Int) -> Int { // expected-error{{cannot override non-async subscript with async subscript}}
    get async throws { 0 }
  }
}

func timeToPlay(gc : Presidio) async {
  _ = gc.yards
  _ = (gc as GolfCourse).yards // expected-error{{property access can throw, but it is not marked with 'try' and the error is not handled}}
  _ = try? (gc as GolfCourse).yards

  // expected-error@+3 {{property access can throw, but it is not marked with 'try' and the error is not handled}}
  // expected-error@+2 {{expression is 'async' but is not marked with 'await'}}{{7-7=await }}
  // expected-note@+1:7{{property access is 'async'}}
  _ = (gc as GolfCourse).par
  _ = try? await (gc as GolfCourse).par

  _ = await gc.par

  _ = gc[2]
  _ = (gc as GolfCourse)[2] // expected-error{{subscript access can throw, but it is not marked with 'try' and the error is not handled}}
  _ = try? (gc as GolfCourse)[2]
}

class AcceptableDynamic {
  dynamic var par : Int {
    get async throws { 60 }
  }

  dynamic subscript(_ i : Int) -> Int {
    get throws { throw E.NotAvailable }
  }
}

// mainly just some soundness checks
// expected-error@+1 {{class 'Misc' has no initializers}}
class Misc {
  // expected-error@+2 {{'lazy' cannot be used on a computed property}}
  // expected-error@+1 {{lazy properties must have an initializer}}
  lazy var someProp : Int {
    get throws { 0 }
  }
}
