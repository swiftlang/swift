// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -typecheck -verify %s

protocol Taggable {}
extension String: Taggable {}

@resultBuilder
struct TaggableBuilder {
  static func buildBlock(_ params: Taggable...) -> String {
    return "Your tags weren't worth keeping anyway"
  }
}

@TaggableBuilder
func testFuncWithOpaqueResult() -> some Taggable {
  "This is an amazing tag"
}

@TaggableBuilder
var testGetterWithOpaqueResult: some Taggable {
  "This is also an amazing tag"
}
