// {"kind":"complete","original":"526e79b6","signature":"(anonymous namespace)::AttributeChecker::visitCustomAttr(swift::CustomAttr*)","signatureAssert":"Assertion failed: (Ctx.hadError()), function visitCustomAttr","useSourceOrderCompletion":true}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-order-completion -source-filename %s
#^^#
typealias a = . struct b {
#^c^#@a
var d =
