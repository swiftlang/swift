@exported import ObjectiveC // Clang module

struct ObjCBool : LogicValue {
  var value : UInt8

  /// \brief Allow use in a Boolean context.
  func getLogicValue() -> Bool {
    if value == 0 { return false }
    return true
  }

  /// \brief Implicit conversion from C Boolean type to Swift Boolean
  /// type.
  @conversion func __conversion() -> Bool {
    return self  
  }
}

extension Bool {
  /// \brief Implicit conversion from Swift Boolean type to
  /// Objective-C Boolean type.
  @conversion func __conversion() -> ObjCBool {
    var result : ObjCBool
    if self { result.value = 1 }
    else { result.value = 0 }
    return result
  }
}

struct ObjCSel : StringLiteralConvertible {
  var ptr : COpaquePointer

  typealias StringLiteralType = CString
  static func convertFromStringLiteral(val: CString) -> ObjCSel {
    var sel : ObjCSel
    sel.ptr = COpaquePointer(val._bytesPtr)
    return sel
  }
}

@asmname="swift_BoolToObjCBool"
func convertBoolToObjCBool(x: Bool) -> ObjCBool

@asmname="swift_ObjCBoolToBool"
func convertObjCBoolToBool(x: ObjCBool) -> Bool

/* FIXME: This breaks enum import; decls created by the Clang importer
   from the 'Foundation' module are able to see definitions from this Swift
   overlay, but are not able to see into the standard library.
   <rdar://problem/15410928>

func ~=(x: NSObject, y: NSObject) -> Bool {
  return true
}
 */
