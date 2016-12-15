// RUN: %target-swift-frontend -emit-ir %s

// rdar://29605388 -- Swift 3 admitted opening an existential type with
// associated types in one case.

protocol HasAssoc {
  associatedtype A
}

enum MyError : Error {
  case bad(Any)
}

func checkIt(_ js: Any) throws {
  switch js {
    case let dbl as HasAssoc:
      throw MyError.bad(dbl)
        
    default:
      fatalError("wrong")
  }
}
