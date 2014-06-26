// RUN: %swift -O %s
// We used to crash on this when trying to devirtualize a call to:
//
// %168 = witness_method $Array<String>, #Sequence."~>"!1 : $@cc(witness_method) @thin <τ_0_0, τ_1_0 where τ_0_0 : Sequence, τ_0_0.GeneratorType : Generator> (@out Optional<τ_1_0>, @in τ_0_0, _PreprocessingPass, @owned @callee_owned (@out τ_1_0, @in τ_0_0) -> (), @thick τ_0_0.Type) -> ()
// ...
// %181 = apply %168<Array<String>, IndexingGenerator<Array<String>>, String, Int>(%166#1, %169#1, %180, %179, %167) : $@cc(witness_method) @thin <τ_0_0, τ_1_0 where τ_0_0 : Sequence, τ_0_0.GeneratorType : Generator> (@out Optional<τ_1_0>, @in τ_0_0, _PreprocessingPass, @owned @callee_owned (@out τ_1_0, @in τ_0_0) -> (), @thick τ_0_0.Type) -> ()
//
// rdar://17399536

// SILModule::findFuncInWitnessTable returns a substitution array of size one.
// GenericSignature::getSubstitutionMap expects the array to have size two
// (t_0_0 and t_0_1).
// As a workaround the devirtualizer checks that the array size match as
// expected an bail if this is not the case.

func asHex(a: [UInt8]) -> String {
  return "".join(a.map { "0x" + String($0, radix: 16)})
}
