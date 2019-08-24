public protocol ExtensionForeign {
    func foreignMethod()
    var foreignGet: Int { get }
    var foreignGetSet: Int { get set }
}
public protocol ExtensionForeignInherit: ExtensionForeign {}
extension ExtensionForeignInherit {
    public func foreignMethod() {}
    public var foreignGet: Int { return 0 }
    public var foreignGetSet: Int {
        get { return 0 }
        set {}
    }
}


public struct ForeignStruct {}
public struct ForeignStructInherit {}
public struct ForeignStructInheritNoDefault {}
public struct ForeignStructOneExtension {}
