@exported 
import ObjectiveC

/// \brief The Objective-C BOOL type.
///
/// On iOS, the Objective-C BOOL type is a typedef of C/C++ bool. Clang 
/// importer imports it as ObjCBool.
///
/// The compiler has special knowledge of this type.
///
/// FIXME: It may be possible to replace this with `typealias ObjCBool = Bool`.
struct ObjCBool : ReplPrintable {
  var value : Bool

  /// \brief Allow use in a Boolean context.
  func getLogicValue() -> Bool {
    return value == true
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
    return ObjCBool(self ? Bool.true : Bool.false)
  }
}
