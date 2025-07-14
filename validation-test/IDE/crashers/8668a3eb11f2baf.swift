// {"kind":"complete","signature":"swift::Demangle::RemanglerBase::entryForNode(swift::Demangle::Node*, bool)"}
// Actual signature: USRBasedType::fromType
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a: c {  b(a  = #^^# }
protocol c: a
