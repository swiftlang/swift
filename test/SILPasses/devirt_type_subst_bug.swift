// RUN: %target-swift-frontend -emit-sil -O -emit-object %s

// We used to crash on this when trying to devirtualize a call to:
//
// %168 = witness_method $Array<String>, #SequenceType."~>"!1 : $@cc(witness_method) @thin <τ_0_0, τ_1_0 where τ_0_0 : SequenceType, τ_0_0.Generator : GeneratorType> (@out Optional<τ_1_0>, @in τ_0_0, _PreprocessingPass, @owned @callee_owned (@out τ_1_0, @in τ_0_0) -> (), @thick τ_0_0.Type) -> ()
// ...
// %181 = apply %168<Array<String>, IndexingGenerator<Array<String>>, String, Int>(%166#1, %169#1, %180, %179, %167) : $@cc(witness_method) @thin <τ_0_0, τ_1_0 where τ_0_0 : SequenceType, τ_0_0.Generator : GeneratorType> (@out Optional<τ_1_0>, @in τ_0_0, _PreprocessingPass, @owned @callee_owned (@out τ_1_0, @in τ_0_0) -> (), @thick τ_0_0.Type) -> ()
//
// rdar://17399536
// rdar://17440222

// SILModule::lookUpFunctionInWitnessTable returns a substitution
// array of size one.  GenericSignature::getSubstitutionMap expects
// the array to have size two (t_0_0 and t_0_1).  We combine the
// substitution list from lookUpFunctionInWitnessTable and the list
// from ApplyInst to match what GenericSignature expects.

func asHex(a: [UInt8]) -> String {
  return "".join(a.map { "0x" + String($0, radix: 16)})
}
