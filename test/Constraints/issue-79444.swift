// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/79444
class C {
  func foo() {
    _ = { [x = "\(self)"] in } // expected-warning {{capture 'x' was never used}}
    _ = { [x = "\(self)"] in x }
    _ = { [x = "\(self)"] in 
      let y = x
      return y 
    }
    _ = { [x = "\(self)"] in
      let fn = { [y = "\(x)"] in
        let z = y
        return z 
      }
      return fn() 
    }
  }
}
