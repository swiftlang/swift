
// SpriteKit defines SKColor using a macro.

#if os(OSX)
public typealias SKColor = NSColor
#elseif os(iOS) || os(tvOS)
public typealias SKColor = UIColor
#endif

// this class only exists to allow AnyObject lookup of _copyImageData
// since that method only exists in a private header in SpriteKit, the lookup
// mechanism by default fails to accept it as a valid AnyObject call
@objc class _SpriteKitMethodProvider : NSObject {
  override init() { _sanityCheckFailure("don't touch me") }
  @objc func _copyImageData() -> NSData! { return nil }
}

extension SKNode {
  public subscript (name: String) -> [SKNode] {
    // Note: Don't stomp on objectForKeyedSubscript:
    @objc(_swiftObjectForKeyedSubscript:) get {
       var nodes = [SKNode]()
       enumerateChildNodesWithName(name) { node, stop in
         nodes.append(node)
       }

       return nodes
    }
  }
}

