// {"kind":"complete","signature":"swift::TypeBase::getContextSubstitutions(swift::DeclContext const*, swift::GenericEnvironment*)","signatureAssert":"Assertion failed: (!baseTy->is<LValueType>() && !baseTy->is<AnyMetatypeType>() && !baseTy->is<ErrorType>()), function getContextSubstitutions"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a extension a where Self == { extension a#^COMPLETE^# protocol b
