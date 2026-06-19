// {"kind":"typecheck","original":"74e5f02c","signature":"swift::constraints::MissingArgumentsFailure::diagnoseAsError()","signatureNext":"AddMissingArguments::diagnose"}
// RUN: not %target-swift-frontend -typecheck %s
func a<each c, d, e>(repeat (each c, d) -> e, repeat each c)
a(repeat b
