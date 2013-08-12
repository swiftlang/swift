/// \brief A thing into which we can stream text
protocol OutputStream {
  func append(text: String)
}

// It would be nice if every String could be used as an OutputStream
// directly, but since Strings are value types, it becomes error-prone
// to use stream adapters, which eventually require some kind of
// manual write-back.
class StringStream : OutputStream {
  func append(text: String) { buffer += text }
  var buffer: String;
}

// \brief A thing that can be written to an OutputStream
protocol Streamable_ {
  func write<S: OutputStream>(s: [byref] S)
}

/// \brief A thing that can be printed in the REPL and the Debugger
///
/// Everything compiler-magically conforms to this protocol.  To
/// change the debug representation for a type, you don't need to
/// declare conformance: simply give the type a debugFormat().
protocol DebugPrintable {
  typealias DebugRepresentation: Streamable_/* = String*/

  /// \brief Produce a textual representation for the REPL and
  /// Debugger.
  ///
  /// Because String is a Streamable, your implementation of
  /// debugView can just return a String.  If you don't like
  /// String's default response to width, precision, and/or alignment,
  /// or if you want to write directly to the OutputStream
  /// for efficiency reasons, (e.g. if your representation is huge),
  /// you can return a custom DebugRepresentation type.
  ///
  /// NOTE: producing a representation that can be consumed by the
  /// REPL to produce an equivalent object is strongly encouraged
  /// where possible!  For example, String.debugFormat() produces a
  /// representation containing quotes, where special characters are
  /// escaped, etc.  A struct Point { var x, y: Int } might be
  /// represented as "Point(x: 3, y: 5)".
  func debugFormat() -> DebugRepresentation
}

func toDebugString<T:DebugPrintable>(x: T) -> String {
  var result = ""
  x.debugFormat().write(&result)
  return result
}

/// \brief A thing that can be print()ed and toString()ed.
///
/// Conformance to Printable is explicit, but if you want to use the
/// debugFormat() results for your type's format(), all you need
/// to do is declare conformance to Printable, and there's nothing to
/// implement.
///
/// Note: explicitness here keeps us from automatically polluting
/// completion results for every type with format() and
/// toString() functions.
protocol Printable: DebugPrintable {
  typealias PrintRepresentation: Streamable_, DebugPrintable/* = DebugRepresentation*/
  
  /// \brief produce a "pretty" textual representation that can be
  /// distinct from the debug format.  For example,
  /// String.format returns the string itself, without quoting.
  ///
  /// In general you can return a String here, but if you need more
  /// control, we strongly recommend returning a custom View
  /// type, e.g. a nested struct of your type.  If you're lazy, you
  /// can conform to Streamable directly and just implement its
  /// write() func.
  func format() -> PrintRepresentation /*{
    return debugFormat()
  }*/
}

func toString<T:Printable>(x: T) -> String {
  var result = ""
  x.format().write(&result)
  return result
}

protocol Streamable : Streamable_, Printable {
/*
  typealias PrintRepresentation = This
  func format() -> PrintRepresentation {
    return this
  }
*/
}

extension String: Streamable {
  typealias DebugRepresentation = String

  func format() -> String {
    return this
  }
  func debugFormat() -> String {
    // FIXME: escape the characters
    return "\"" + this + "\""
  }
  func write<S: OutputStream>(s: [byref] S) {
    s.append(this)
  }
}

protocol StreamAdapter {
  static func adapt<S: OutputStream>(base: S) -> OutputStream
}

struct UpcaseStream: StreamAdapter {
  struct Stream<Base: OutputStream> : OutputStream {
    var base: GenericIVar<Base>
    func append(text: String) { base.value.append(text.uppercase) }
  }

  static func adapt<S: OutputStream>(base: S) -> OutputStream {
    return Stream(GenericIVar(base))
  }
}

/*
struct DowncaseStream<Base: OutputStream>: StreamAdapter {
  constructor(base: Base) {
    this.base = base
  }

  static func adapt(base: Base) -> DowncaseStream {
    return DowncaseStream(base)
  }

  func append(text: String) { base.append(text.lowercase) }

  var base: GenericIVar<Base>
}
*/

struct StreamableAdapter<StreamAdapt: StreamAdapter, S: Streamable> : Streamable {
  constructor(base: S) {
    this.base.value = base
  }
  func write<S: OutputStream>(s: [byref] S) {
    var s2 = StreamAdapt.adapt(s)
    base.value.write(&s2)
    s = s2.base
  }

  typealias PrintRepresentation = String
  func format() -> String {
    return toString
  }

  typealias DebugRepresentation = String
  func debugFormat() -> DebugRepresentation {
    return "<StreamableAdapter>"
  }

  var base: GenericIVar<S>
}

operator infix ․ {
  associativity left
  precedence 9999
}

func uppercase_<S: Streamable>(s: S) -> StreamableAdapter<UpcaseStream,S> {
  return StreamableAdapter<UpcaseStream,S>(s)
}

struct Type<T> {}
typealias uppercase = Type<UpcaseStream>

func [infix] ․<S: Streamable, A: StreamAdapter>(s: S, a: Type<A>) -> StreamableAdapter<A,S> {
  return StreamableAdapter<A,S>(s)
}

