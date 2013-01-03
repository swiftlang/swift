import ObjC

//===----------------------------------------------------------------------===//
// Objective-C Primitive Types
//===----------------------------------------------------------------------===//
// FIXME: Objective-C types belong in a layer below the Objective-C support
// libraries, not here.

/// \brief The Objective-C BOOL type.
///
/// The Objective-C BOOL type is typically a typedef of "signed char". However,
/// the Clang importer treats it as a distinct type.
///
/// Note: When BOOL is a typedef of C's _Bool/C++'s bool, this struct goes away
/// and simply becomes a typealias for CBool.
struct ObjCBool {
  var value : UInt8

  /// \brief Allow use in a Boolean context.
  func getLogicValue() -> Bool {
    if value == 0 { return false }
    return true
  }

  /// \brief Implicit conversion from C Boolean type to Swift Boolean
  /// type.
  func [conversion] __conversion() -> Bool {
    return this  
  }
}

extension Bool {
  /// \brief Implicit conversion from Swift Boolean type to
  /// Objective-C Boolean type.
  func [conversion] __conversion() -> ObjCBool {
    var result : ObjCBool
    if this { result.value = 1 }
    else { result.value = 0 }
    return result
  }
}
