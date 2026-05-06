// {"kind":"complete","original":"5d4b3348","signature":"swift::rewriting::RequirementMachine::requiresProtocol(swift::Type, swift::ProtocolDecl const*) const","signatureNext":"Mangle::ASTMangler::gatherGenericSignatureParts"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a<b > : RangeReplaceableCollection {
associatedtype b }
extension a {
struct c<Element> :
a #^^#
typealias SubSequence = c<Element>
