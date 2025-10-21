// {"kind":"typecheck","original":"1806b993","signature":"swift::constraints::ConstraintSystem::diagnoseAmbiguityWithFixes(llvm::SmallVectorImpl<swift::constraints::Solution>&)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<each b >(repeat inout each b) -> (repeat each b
( ;
let c 0 = a( 12.3 + 3
