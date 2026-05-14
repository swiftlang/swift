// {"kind":"complete","original":"5e941dd1","signature":"swift::constraints::TypeVarBindingProducer::computeNext()","signatureAssert":"Assertion failed: (!type->hasError()), function computeNext","signatureNext":"TypeVarBindingProducer"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a < b {
struct c {
class d extension c where b == {
e {
class f : d let a "\(#^^#x ] 0)#^A^#"
