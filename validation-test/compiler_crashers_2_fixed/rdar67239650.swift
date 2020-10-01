// RUN: %target-swift-frontend -typecheck %s

@_functionBuilder
struct SillyBuilder {
  static func buildBlock() -> () {}
}

struct SillyStruct {
  init(@SillyBuilder _: () -> ()) {}
}

struct UsesSillyStruct {
  var x: Int = 0

  func foo() {
    SillyStruct {
      let fn = {
        if true {
          _ = x
        }
      }
    }
  }
}
