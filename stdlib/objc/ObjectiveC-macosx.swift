@exported 
import ObjectiveC

/// \brief The Objective-C BOOL type.
///
/// On OS X, the Objective-C BOOL type is a typedef of "signed char".  Clang
/// importer imports it as ObjCBool.
///
/// The compiler has special knowledge of this type.
struct ObjCBool : ReplPrintable {
  var value : Int8

  /// \brief Allow use in a Boolean context.
  func getLogicValue() -> Bool {
    return value != 0
  }

  /// \brief Implicit conversion from C Boolean type to Swift Boolean
  /// type.
  @conversion func __conversion() -> Bool {
    return self.getLogicValue()
  }

  func replPrint() {
    // Dispatch to Bool.
    self.getLogicValue().replPrint()
  }
}

extension Bool {
  /// \brief Implicit conversion from Swift Boolean type to
  /// Objective-C Boolean type.
  @conversion func __conversion() -> ObjCBool {
    return ObjCBool(self ? 1 : 0)
  }
}
