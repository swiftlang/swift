import swift
import ObjectiveC

//===----------------------------------------------------------------------===//
// Objective-C Primitive Types
//===----------------------------------------------------------------------===//
// FIXME: Objective-C types belong in a layer below the Objective-C support
// libraries, not here.

/// \brief The Objective-C BOOL type.
///
/// The Objective-C BOOL type is typically a typedef of "signed char".  Clang
/// importer imports it as ObjCBool.
///
/// Note: When BOOL is a typedef of C's _Bool/C++'s bool, this struct goes away
/// and simply becomes a typealias for CBool.
///
/// The compiler has special knowledge of this type.
struct ObjCBool {
  var value : UInt8

  /// \brief Allow use in a Boolean context.
  func getLogicValue() -> Bool {
    return value != 0
  }

  /// \brief Implicit conversion from C Boolean type to Swift Boolean
  /// type.
  func [conversion] __conversion() -> Bool {
    return this.getLogicValue()
  }

  func replPrint() {
    // Dispatch to Bool.
    this.getLogicValue().replPrint()
  }
}

extension Bool {
  /// \brief Implicit conversion from Swift Boolean type to
  /// Objective-C Boolean type.
  func [conversion] __conversion() -> ObjCBool {
    return ObjCBool(this ? 1 : 0)
  }
}

/// \brief The Objective-C SEL type.
///
/// The Objective-C SEL type is typically an opaque pointer. Swift
/// treats it as a distinct struct type, with operations to
/// convert between C strings and selectors.
///
/// The compiler has special knowledge of this type.
struct ObjCSel : StringLiteralConvertible {
  var ptr : COpaquePointer

  /// \brief Create a selector from a string.
  constructor(str : String) {
    var LM = LifetimeManager()
    ptr = sel_registerName(LM.getCString(str)).ptr
    LM.release()
  }

  typealias StringLiteralType = CString

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

extension ObjCSel : Equatable, Hashable {
  func __equal__ (rhs : ObjCSel) -> Bool {
    return sel_isEqual(this, rhs)
  }
  func hashValue() -> Int {
    return ptr.hashValue()
  }
}

extension String {
  /// \brief Construct the C string representation of an Objective-C selector.
  constructor(sel : ObjCSel) {
    // FIXME: This misses the ASCII optimization.
    this = String.fromCString(sel_getName(sel))
  }
}

// Functions used to implicitly bridge ObjCBool types to Swift's Bool type.

func convertBoolToObjCBool(x:Bool) -> ObjCBool {
  return x
}
func convertObjCBoolToBool(x:ObjCBool) -> Bool {
  return x
}
