// {"kind":"complete","original":"8840879e","signature":"swift::Decl::getResolvedCustomAttrType(swift::CustomAttr*) const","signatureAssert":"Assertion failed: (lhs->getDeclContext()->getParentModule() == rhs->getDeclContext()->getParentModule() && \"conformances should be in the same module\"), function compareConformances","useSourceOrderCompletion":true}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-order-completion -source-filename %s
typealias a< b> = c<Result<b, String>> @propertyWrapper struct c<d {
#^^#
}
protocol e: Error extension String: e struct f { @a h= #^g^#
