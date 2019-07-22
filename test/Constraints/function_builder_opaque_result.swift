// RUN: %target-swift-frontend -disable-availability-checking -typecheck -verify %s

protocol Taggable {}
extension String: Taggable {}

@_functionBuilder
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
