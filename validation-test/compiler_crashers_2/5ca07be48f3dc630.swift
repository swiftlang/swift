// {"signature":"swift::TuplePatternElt& llvm::SmallVectorTemplateBase<swift::TuplePatternElt, true>::growAndEmplaceBack<swift::Identifier, swift::SourceLoc, swift::Pattern*&>(swift::Identifier&&, swift::SourceLoc&&, swift::Pattern*&)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
switch {                           case .a(&b
