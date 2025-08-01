// {"kind":"complete","signature":"swift::Mangle::ASTMangler::appendExtension(swift::ExtensionDecl const*, swift::Mangle::ASTMangler::BaseEntitySignature&, llvm::StringRef)"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
public struct Wrapper<T: P> {}
extension Wrapper: Q where T: Q {
  public protocol PBase {
    associatedtype AssocType
  }
  public protocol P: PBase {
    override associatedtype AssocType:#^^# P = Wrapper<Self>
  }
  public protocol Q: P where SelfAssocType: Q {}
  public protocol R: Q where Self.AssocType: R {}
}
