// {"kind":"complete","original":"06e7188f","signature":"(anonymous namespace)::CodeCompletionCallbacksImpl::readyForTypeChecking(swift::SourceFile*)","signatureAssert":"Assertion failed: (!ParsedExpr || Kind == CompletionKind::KeyPathExprObjC && \"Should use solver-based completion for expressions\"), function readyForTypeChecking"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
case (a: b#^^#
<#expression#>
