import ASTBridging

public protocol DeclContext : AnyObject {
  var bridgedDeclContext: BridgedDeclContext { get }
}

extension DeclContext {
  public var astContext: ASTContext { bridgedDeclContext.astContext }
}

public class UnknownDeclContext : DeclContext {
  public var bridgedDeclContext: BridgedDeclContext
  public init(bridged: BridgedDeclContext) { bridgedDeclContext = bridged }
}
