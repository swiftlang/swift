// {"kind":"complete","original":"747a5bcd","signature":"swift::constraints::inference::BindingSet::addBinding(swift::constraints::inference::PotentialBinding)","signatureAssert":"Assertion failed: (std::find(Bindings.begin(), Bindings.end(), binding) == Bindings.end()), function addBinding","signatureNext":"BindingSet::BindingSet"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
( [Int:Int ]) {
a, b in b= b < b
}#^^#
