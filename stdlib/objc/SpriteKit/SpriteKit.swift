@exported import SpriteKit

// SpriteKit defines SKColor using a macro.

#if os(OSX)
typealias SKColor = NSColor
#elseif os(iOS)
typealias SKColor = UIColor
#endif

#if os(OSX)
// this class only exists to allow AnyObject lookup of _copyImageData
// since that method only exists in a private header in SpriteKit, the lookup
// mechanism by default fails to accept it as a valid AnyObject call
@objc class _SpriteKitMethodProvider : NSObject {
  init() { _fatalError("don't touch me") }
  @objc func _copyImageData() -> NSData! { return nil }
}

// HACK - these four mirrors do the exact same thing, they should be one and the same
// and use an AnyObject, or some T : NSObject as their ivar type, but that causes
// the compiler to crash - as a result, four mirrors it is
struct _SKShapeNodeMirror : Mirror {
  var _value : SKShapeNode
  
  init(_ _v : SKShapeNode) {
    _value = _v
  }
  
  var value : Any { return (_value as Any)}
  
  var valueType: Any.Type { return (_value as Any).dynamicType }
  
  var objectIdentifier: ObjectIdentifier? { return .None }
  
  var count: Int { return 0 }
  
  subscript(_: Int) -> (String,Mirror) { _fatalError("don't ask") }

  var summary: String { return _value.description }
  
  var quickLookObject: QuickLookObject? {
      // this code comes straight from the quicklooks
      
      if let data = (_value as AnyObject)._copyImageData?() {
        // we could send a Raw, but I don't want to make a copy of the bytes for no good reason
        // make an NSImage out of them and send that
        let img = NSImage(data: data)
        return .Some(.Sprite(img))
      }
      
      return nil
  }
  
  var disposition : MirrorDisposition { get { return .Aggregate } }
}

struct _SKSpriteNodeMirror : Mirror {
  var _value : SKSpriteNode
  
  init(_ _v : SKSpriteNode) {
    _value = _v
  }
  
  var value : Any { return (_value as Any)}
  
  var valueType: Any.Type { return (_value as Any).dynamicType }
  
  var objectIdentifier: ObjectIdentifier? { return .None }
  
  var count: Int { return 0 }
  
  subscript(_: Int) -> (String,Mirror) { _fatalError("don't ask") }

  var summary: String { return _value.description }
  
  var quickLookObject: QuickLookObject? {
      // this code comes straight from the quicklooks
      
      if let data = (_value as AnyObject)._copyImageData?() {
        // we could send a Raw, but I don't want to make a copy of the bytes for no good reason
        // make an NSImage out of them and send that
        let img = NSImage(data: data)
        return .Some(.Sprite(img))
      }
      
      return nil
  }
  
  var disposition : MirrorDisposition { get { return .Aggregate } }
}

struct _SKTextureAtlasMirror : Mirror {
  var _value : SKTextureAtlas
  
  init(_ _v : SKTextureAtlas) {
    _value = _v
  }
  
  var value : Any { return (_value as Any)}
  
  var valueType: Any.Type { return (_value as Any).dynamicType }
  
  var objectIdentifier: ObjectIdentifier? { return .None }
  
  var count: Int { return 0 }
  
  subscript(_: Int) -> (String,Mirror) { _fatalError("don't ask") }

  var summary: String { return _value.description }
  
  var quickLookObject: QuickLookObject? {
      // this code comes straight from the quicklooks
      
      if let data = (_value as AnyObject)._copyImageData?() {
        // we could send a Raw, but I don't want to make a copy of the bytes for no good reason
        // make an NSImage out of them and send that
        let img = NSImage(data: data)
        return .Some(.Sprite(img))
      }
      
      return nil
  }
  
  var disposition : MirrorDisposition { get { return .Aggregate } }
}

struct _SKTextureMirror : Mirror {
  var _value : SKTexture
  
  init(_ _v : SKTexture) {
    _value = _v
  }
  
  var value : Any { return (_value as Any)}
  
  var valueType: Any.Type { return (_value as Any).dynamicType }
  
  var objectIdentifier: ObjectIdentifier? { return .None }
  
  var count: Int { return 0 }
  
  subscript(_: Int) -> (String,Mirror) { _fatalError("don't ask") }

  var summary: String { return _value.description }
  
  var quickLookObject: QuickLookObject? {
      // this code comes straight from the quicklooks
      
      if let data = (_value as AnyObject)._copyImageData?() {
        // we could send a Raw, but I don't want to make a copy of the bytes for no good reason
        // make an NSImage out of them and send that
        let img = NSImage(data: data)
        return .Some(.Sprite(img))
      }
      
      return nil
  }
  
  var disposition : MirrorDisposition { get { return .Aggregate } }
}

extension SKShapeNode : Reflectable {
  func getMirror() -> Mirror {
    return _SKShapeNodeMirror(self)
  }
}

extension SKSpriteNode : Reflectable {
  func getMirror() -> Mirror {
    return _SKSpriteNodeMirror(self)
  }
}
extension SKTextureAtlas : Reflectable {
  func getMirror() -> Mirror {
    return _SKTextureAtlasMirror(self)
  }
}
extension SKTexture : Reflectable {
  func getMirror() -> Mirror {
    return _SKTextureMirror(self)
  }
}
#elseif os(iOS)
// FIXME: we want to interop nicely with SpriteKit on iOS as well
#endif

extension SKNode {
  subscript (name: String) -> SKNode[] {
     var nodes = SKNode[]()
     enumerateChildNodesWithName(name) { node, stop in
       if let n = node { nodes.append(n) }
     }
     return nodes
  }
}

