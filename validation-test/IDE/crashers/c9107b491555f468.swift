// {"kind":"complete","signature":"parseGuardedPattern(swift::Parser&, (anonymous namespace)::GuardedPattern&, swift::ParserStatus&, llvm::SmallVectorImpl<swift::VarDecl*>&, (anonymous namespace)::GuardedPatternContext, bool)"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
case = switch { case #^COMPLETE^#<#expression#>
