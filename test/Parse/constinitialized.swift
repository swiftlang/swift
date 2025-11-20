// RUN: %target-typecheck-verify-swift -enable-experimental-feature CompileTimeValues
// REQUIRES: swift_feature_CompileTimeValues

@constInitialized let x: Int = 42
@constInitialized var y: Int = 42

protocol ConstUserProto {
	@constInitialized var v: String { get } // expected-error{{'@constInitialized' cannot be used inside a protocol declaration}}
}

class ConstFanClassWrong: ConstUserProto {
	@constInitialized var v: String = "" // expected-error{{properties with attribute @constInitialized must be static}}
  @constInitialized static var B: String = ""
  @constInitialized static var Computed: String { get { return "" } } // expected-error{{'@constInitialized' must not be used on computed properties}}
}

func takeIntConst(@constInitialized _ a: Int) {} // expected-error{{@constInitialized may only be used on 'var' declarations}}

@constInitialized func constFunc(_ a: Int) {} // expected-error{{@constInitialized may only be used on 'var' declarations}}

func LocalConstVarUser() -> Int {
		@constInitialized let localConst = 3 // expected-error{{attribute @constInitialized can only be used in a non-local scope}}
		return localConst + 1
}
