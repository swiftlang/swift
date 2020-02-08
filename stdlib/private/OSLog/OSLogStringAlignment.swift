@frozen
public enum OSLogCollectionBound {
  case start
  case end
}

@frozen
public struct OSLogStringAlignment {
  /// Minimum number of characters to be displayed. If the value to be printed
  /// is shorter than this number, the result is padded with spaces. The value
  /// is not truncated even if the result is larger.
  public var minimumColumnWidth: Int
  /// This captures right/left alignment.
  public var anchor: OSLogCollectionBound

  /// - Parameters:
  ///   - minimumColumnWidth: Minimum number of characters to be displayed. If the value to be
  ///    printed is shorter than this number, the result is padded with spaces. The value is not truncated
  ///    even if the result is larger.
  ///   - anchor: Use `.end` for right alignment and `.start` for left.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  public init(
    minimumColumnWidth: Int = 0,
    anchor: OSLogCollectionBound = .end
  ) {
    self.minimumColumnWidth = minimumColumnWidth
    self.anchor = anchor
  }

  /// Right alignment formatter.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  public static var right: OSLogStringAlignment {
    OSLogStringAlignment(anchor: .end)
  }

  /// Left alignment formatter.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  public static var left: OSLogStringAlignment {
    OSLogStringAlignment(anchor: .start)
  }

  /// Use default alignment, which is right alignment.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  public static var none: OSLogStringAlignment { .right  }

  /// Right align and display at least`columns` characters.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  public static func right(columns: Int = 0) -> OSLogStringAlignment {
    OSLogStringAlignment(minimumColumnWidth: columns, anchor: .end)
  }

  /// Left align and display at least`columns` characters.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  public static func left(columns: Int = 0) -> OSLogStringAlignment {
    OSLogStringAlignment(minimumColumnWidth: columns, anchor: .start)
  }
}
