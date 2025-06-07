def ObjectiveCBridgeableImplementationForNSValue(Type):
    return """
extension {Type}: _ObjectiveCBridgeable {{
  public func _bridgeToObjectiveC() -> NSValue {{
    var myself = self
    return NSValue(bytes: &myself, objCType: _getObjCTypeEncoding({Type}.self))
  }}

  public static func _forceBridgeFromObjectiveC(_ source: NSValue,
                                                result: inout {Type}?) {{
    precondition(strcmp(source.objCType,
                        _getObjCTypeEncoding({Type}.self)) == 0,
                 "NSValue does not contain the right type to bridge to {Type}")
    result = {Type}()
    if #available(OSX 13.0, iOS 16.0, tvOS 16.0, watchOS 6.0, *) {{
      source.getValue(&result!, size: MemoryLayout<{Type}>.size)
    }} else {{
      source.getValue(&result!)
    }}
  }}

  public static func _conditionallyBridgeFromObjectiveC(_ source: NSValue,
                                                        result: inout {Type}?)
      -> Bool {{
    if strcmp(source.objCType, _getObjCTypeEncoding({Type}.self)) != 0 {{
      result = nil
      return false
    }}
    result = {Type}()
    if #available(OSX 13.0, iOS 16.0, tvOS 16.0, watchOS 6.0, *) {{
      source.getValue(&result!, size: MemoryLayout<{Type}>.size)
    }} else {{
      source.getValue(&result!)
    }}
    return true
  }}

  public static func _unconditionallyBridgeFromObjectiveC(_ source: NSValue?)
      -> {Type} {{
    let unwrappedSource = source!
    precondition(strcmp(unwrappedSource.objCType,
                        _getObjCTypeEncoding({Type}.self)) == 0,
                 "NSValue does not contain the right type to bridge to {Type}")
    var result = {Type}()
    if #available(OSX 13.0, iOS 16.0, tvOS 16.0, watchOS 6.0, *) {{
      unwrappedSource.getValue(&result, size: MemoryLayout<{Type}>.size)
    }} else {{
      unwrappedSource.getValue(&result)
    }}
    return result
  }}
}}
""".format(Type=Type)


def ObjectiveCBridgeableImplementationForNSValueWithCategoryMethods(
    Type,
    initializer,
    getter,
    objCType="_getObjCTypeEncoding"
):
    return """
extension {Type}: _ObjectiveCBridgeable {{
  public func _bridgeToObjectiveC() -> NSValue {{
    return {initializer}(self)
  }}

  public static func _forceBridgeFromObjectiveC(_ source: NSValue,
                                                result: inout {Type}?) {{
    precondition(strcmp(source.objCType,
                        {objCType}({Type}.self)) == 0,
                 "NSValue does not contain the right type to bridge to {Type}")
    result = {getter}(source)
  }}

  public static func _conditionallyBridgeFromObjectiveC(_ source: NSValue,
                                                        result: inout {Type}?)
      -> Bool {{
    if strcmp(source.objCType, {objCType}({Type}.self)) != 0 {{
      result = nil
      return false
    }}
    result = {getter}(source)
    return true
  }}

  public static func _unconditionallyBridgeFromObjectiveC(_ source: NSValue?)
      -> {Type} {{
    let unwrappedSource = source!
    precondition(strcmp(unwrappedSource.objCType,
                        {objCType}({Type}.self)) == 0,
                 "NSValue does not contain the right type to bridge to {Type}")
    return {getter}(unwrappedSource)
  }}
}}
""".format(Type=Type, initializer=initializer,
           getter=getter, objCType=objCType)
