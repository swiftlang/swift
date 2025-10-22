// {"kind":"complete","original":"b09ce71a","signature":"swift::Decl::getResolvedCustomAttrType(swift::CustomAttr*) const","signatureAssert":"Assertion failed: (ctx.Diags.hadAnyError()), function evaluate","useSourceOrderCompletion":true}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-order-completion -source-filename %s
struct a<b
extension a where b == {struct c {
@d#^^#({ #^e^#}
var f }
@propertyWrapper
class d
