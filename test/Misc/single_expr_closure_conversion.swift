// RUN: %target-parse-verify-swift

struct Blob {}

func withBlob(block: Blob -> ()) {}

protocol Binding {}
extension Int: Binding {}
extension Double: Binding {}
extension String: Binding {}
extension Blob: Binding {}

struct Stmt {
  func bind(values: Binding?...) -> Stmt {
    return self
  }

  func bind(values: [Binding?]) -> Stmt {
    return self
  }

  func bind(values: [String: Binding?]) -> Stmt {
    return self
  }
}

let stmt = Stmt()
withBlob { stmt.bind(1, 2.0, "3", $0) }
withBlob { stmt.bind([1, 2.0, "3", $0]) }
withBlob { stmt.bind(["1": 1, "2": 2.0, "3": "3", "4": $0]) }