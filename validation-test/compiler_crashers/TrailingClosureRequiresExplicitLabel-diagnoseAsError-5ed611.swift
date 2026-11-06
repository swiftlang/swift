// {"kind":"typecheck","signature":"swift::constraints::TrailingClosureRequiresExplicitLabel::diagnoseAsError()","signatureNext":"SpecifyLabelToAssociateTrailingClosure::diagnose"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
a {}
func a<each b, c>(repeat each b, c)
