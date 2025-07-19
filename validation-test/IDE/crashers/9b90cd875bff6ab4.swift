// {"kind":"complete","signature":"(anonymous namespace)::Remangler::trySubstitution(swift::Demangle::Node*, swift::Demangle::SubstitutionEntry&, bool)"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a:c{ b(a=
#^^#}
protocol c:a
