// {"kind":"complete","original":"3907d1a2","signature":"swift::rewriting::RewriteContext::getRequirementMachine(swift::CanGenericSignature)","signatureNext":"GenericSignatureImpl::getSuperclassBound"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a { associatedtype b associatedtype c: d where c.e.f == g }
protocol h { associatedtype f: a where f.b == Self }
protocol d { associatedtype e: h }
struct g: a { typealias b = i
#^^#
struct i
