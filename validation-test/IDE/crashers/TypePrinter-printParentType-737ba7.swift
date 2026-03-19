// {"kind":"complete","original":"894342da","signature":"(anonymous namespace)::TypePrinter::printParentType(swift::Type)","signatureAssert":"Assertion failed: (!hasTypeParameter() && \"already have an interface type\"), function mapTypeOutOfEnvironment","signatureNext":"TypePrinter"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a<b {
  protocol c {
    associatedtype b
  }
  struct d<b {
    init<e: c>(e ) where e.b == b
  }
  struct f<b>  ; g = {
    let erasure = d(
      f(
    let  = #^^#
