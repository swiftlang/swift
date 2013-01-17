import ObjectiveC

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
  // FIXME: Ambiguity between ObjC and swift modules. Ugh.
  var value : swift.UInt8

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

/// \brief The Objective-C SEL type.
///
/// The Objective-C SEL type is typically an opaque pointer. Swift
/// treats it as a distinct struct type, with operations to
/// convert between C strings and selectors.
struct ObjCSel {
  var ptr : COpaquePointer

  /// \brief Create a selector from a string.
  constructor(str : String) {
    ptr = sel_registerName(CString(str)).ptr
  }

  /// \brief Construct a selector from a string literal.
  ///
  /// FIXME: Fast-path this in the compiler, so we don't end up with
  /// the sel_registerName call at compile time.
  static func convertFromStringLiteral(str : CString) -> ObjCSel {
    return sel_registerName(str)
  }
 
  func replPrint() {
    print(String(this))
  }
}

extension String {
  /// \brief Construct the C string representation of an Objective-C selector.
  constructor(sel : ObjCSel) {
    // FIXME: This is crazy. Adopting a C string as a Swift string should be
    // simpler. This even misses the ASCII optimization.
    var name = sel_getName(sel)
    this = String.convertFromStringLiteral(name.cstr.value, strlen(name).value)
  }
}
