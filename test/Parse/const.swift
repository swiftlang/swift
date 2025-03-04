// RUN: %target-typecheck-verify-swift -enable-experimental-feature CompileTimeValues
// REQUIRES: swift_feature_CompileTimeValues
@const let x: Int = 42

// FIXME: Only allow 'let' for `@const` properties, even in protocol requirements
protocol ConstUserProto {
	@const static var v: String { get }
}

class ConstFanClassWrong: ConstUserProto {
	  @const static let v: String = ""
    // FIXME: Only allow 'let' for `@const` properties
    @const static var B: String = ""
}

func takeIntConst(@const _ a: Int) {}

@const func constFunc(_ a: Int) {}

struct Article {
  let id: String
}
@const let keypath = \Article.id

func LocalConstVarUser() -> Int {
		@const let localConst = 3
		return localConst + 1
}

// FIXME: This should be diagnosed
@const let a: Bool = Bool.random()
