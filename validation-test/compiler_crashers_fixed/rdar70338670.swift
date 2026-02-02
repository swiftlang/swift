// RUN: %target-swift-frontend -emit-ir %s

public struct X {
  public subscript(_ key: String, as type: Error.Type = Error.self) -> Error? {
    get {
      return nil
    }
  }
}

let x = X()
_ = x["hi"]
