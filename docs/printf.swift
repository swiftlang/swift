/// \brief A thing into which we can stream text
protocol FormattedOutputStream {
  func append(text: String)
}

/// \brief Every String can be used as a FormattedOutputStream
/// directly
extension String: FormattedOutputStream {
  func append(text: String)
}

/// \brief A thing that can be printed in the REPL and the Debugger
///
/// Everything compiler-magically conforms to this protocol.  To
/// change the debug representation for a type, you don't need to
/// declare conformance: simply give the type a debugFormat().
protocol DebugPrintable {
  typealias DebugFormatter: Formattable = String

  /// \brief Produce a textual representation for the REPL and
  /// Debugger.
  ///
  /// Because String is a Formattable, your implementation of
  /// debugFormat can just return a String.  If you don't like
  /// String's default response to width, precision, and/or alignment,
  /// or if you want to write directly to the FormattedOutputStream
  /// for efficiency reasons, (e.g. if your representation is huge),
  /// you can return a custom DebugFormatter type.
  ///
  /// NOTE: producing a representation that can be consumed by the
  /// REPL to produce an equivalent object is strongly encouraged
  /// where possible!  For example, String.debugFormat() produces a
  /// representation containing quotes, where special characters are
  /// escaped, etc.  A struct Point { var x, y: Int } might be
  /// represented as "Point(x: 3, y: 5)".
  func debugFormat() -> DebugFormatter
}

/// \brief A thing that can be print()ed toString()ed.
///
/// Conformance to Printable is explicit, but if you want to use the
/// debugFormat() results for your type's printFormat(), all you need
/// to do is declare conformance to Printable, and there's nothing to
/// implement.
///
/// Note: explicitness here keeps us from automatically polluting
/// completion results for every type with printFormat() and
/// toString() functions.
protocol Printable: DebugPrintable {
  typealias PrintFormatter: Formattable = DebugFormatter
  
  /// \brief produce a "pretty" textual representation that can be
  /// distinct from the debug format.  For example,
  /// String.printFormat returns the string itself, without quoting.
  ///
  /// In general you can return a String here, but if you need more
  /// control, we strongly recommend returning a custom Formatter
  /// type, e.g. a nested struct of your type.  If you're lazy, you
  /// can conform to Formattable directly and just implement its
  /// write() func.
  func printFormat() -> PrintFormatter {
    return debugFormat()
  }

  /// \brief Simply convert to String
  ///
  /// Don't reimplement this: the default implementation always works.
  /// If you must reimplement toString(), make sure its results are
  /// consistent with those of printFormat() (i.e. you shouldn't
  /// change the behavior).
  func toString() -> String {
    var result: String
    this.printFormat().write(result)
    return result
  }
}

/// \brief A thing that can write into a FormattedOutputStream while
/// responding to width, precision, and alignment.  Every Formattable
/// is also a Printable, naturally.
protocol Formattable: Printable {
  func write(
    target: [byref] FormattedOutputStream, 
    width: Int? = None, precision: Int? = None, right_align: Bool? = None)

  // You'll never want to reimplement this
  func printFormat() -> PrintFormatter {
    return this
  }
  
  /// \brief get the debug representation.  
  ///
  /// A Formattable will usually want to override this, so that in the
  /// debugger and REPL, it doesn't appear to be the thing on whose
  /// behalf it is formatting.
  func debugFormat() -> DebugFormatter {
    return this
  }
}

extension String: Formattable {
  func write(
    target: [byref] FormattedOutputStream, knobs
    width: Int? = None, precision: Int? = None, right_align: Bool? = None
  ) {
    ...
  }

  func debugFormat() -> String {
    ...escape all the CodePoints...
  }

  // Swift may get us this default automatically from the protocol,
  // but it's here as a reminder of how String behaves.
  func printFormat() -> String { 
    return this
  }
}

