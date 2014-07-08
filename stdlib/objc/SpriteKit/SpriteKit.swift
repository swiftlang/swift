@exported import SpriteKit

// SpriteKit defines SKColor using a macro.

#if os(OSX)
public typealias SKColor = NSColor
#elseif os(iOS)
public typealias SKColor = UIColor
#endif

#if os(OSX)
// this class only exists to allow AnyObject lookup of _copyImageData
// since that method only exists in a private header in SpriteKit, the lookup
// mechanism by default fails to accept it as a valid AnyObject call
objc class _SpriteKitMethodProvider : NSObject {
  init() { _fatalError("don't touch me") }
  objc func _copyImageData() -> NSData! { return nil }
}
#elseif os(iOS)
// FIXME: we want to interop nicely with SpriteKit on iOS as well
#endif

extension SKNode {
  public subscript (name: String) -> [SKNode] {
     var nodes = [SKNode]()
     enumerateChildNodesWithName(name) { node, stop in
       if let n = node { nodes.append(n) }
     }
     return nodes
  }
}

