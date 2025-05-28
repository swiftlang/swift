// RUN: %target-typecheck-verify-swift -enable-experimental-feature CompileTimeValues
// REQUIRES: swift_feature_CompileTimeValues
@const let x: Int = 42
@const var y: Int = 42 // expected-error{{@const may only be used on 'let' declarations}}

// FIXME: Only allow 'let' for `@const` properties, even in protocol requirements
protocol ConstUserProto {
  @const static var v: String { get }
}

class ConstFanClassWrong: ConstUserProto {
  @const static let v: String = ""
  @const static var B: String = "" // expected-error{{@const may only be used on 'let' declarations}}
  @const static let C: String = ""
}

func takeIntConst(@const _ a: Int) {}

@const func constFunc(_ a: Int) {}

struct Article {
  let id: String
}
@const let keypath = \Article.id // expected-error{{keypaths not supported in a '@const' expression}}

func LocalConstVarUser() -> Int {
  @const let localConst = 3
  return localConst + 1
}

@const let a: Bool = Bool.random() // expected-error{{not supported in a '@const' expression}}
