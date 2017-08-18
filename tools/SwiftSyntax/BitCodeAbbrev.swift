
import Foundation

typealias BitCodeAbbrev = [BitCodeAbbrevOp]

enum BitCodeAbbrevOp {
  /// A literal value (emitted as a VBR8 field).
  case literal(UInt64)

  /// A fixed-width field.
  case fixed(bitWidth: UInt8)

  /// A VBR-encoded value with the provided chunk width.
  case vbr(chunkBitWidth: UInt8)

  /// An array of values. This expects another operand encoded
  /// directly after indicating the element type.
  /// The array will begin with a vbr6 value indicating the length of
  /// the following array.
  indirect case array(BitCodeAbbrevOp)

  /// A char6-encoded ASCII character.
  case char6

  /// Emitted as a vbr6 value, padded to a 32-bit boundary and then
  /// an array of 8-bit objects.
  case blob

  /// Whether this case is the `literal` case.
  var isLiteral: Bool {
    if case .literal = self { return true }
    return false
  }

  /// The llvm::BitCodeAbbrevOp::Encoding value this
  /// enum case represents.
  /// - note: Must match the encoding in
  ///         http://llvm.org/docs/BitCodeFormat.html#define-abbrev-encoding
  var encodedKind: UInt8 {
    switch self {
    case .literal(_): return 0
    case .fixed(_): return 1
    case .vbr(_): return 2
    case .array: return 3
    case .char6: return 4
    case .blob: return 5
    }
  }

  /// Turns a literal value of a RawRepresentable type into a literal abbrev.
  static func literalCode<CodeType: RawRepresentable>
    (_ code: CodeType) -> BitCodeAbbrevOp where CodeType.RawValue == UInt8 {
    return .literal(numericCast(code.rawValue))
  }
}
