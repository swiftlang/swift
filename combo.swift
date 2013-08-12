/// \brief A thing into which we can stream text
protocol OutputStream {
  func append(s: String)
  
  // debugging only
  func dump() -> String
}

/// \brief A thing that can write itself into an OutputStream
protocol Streamable {
  func writeTo<Target: OutputStream>(target: [byref] Target)
}

/// \brief Used to apply an arbitrary transformation to text that is
/// written into an OutputStream
protocol OutputStreamAdapter {

  /// \brief Accept incoming text and write it to the stream
  func write<Stream: OutputStream>(text: String, stream: [byref] Stream)

  /// \brief Flush any text that may still be buffered in the OutputStreamAdapter
  func close<Stream: OutputStream>(stream: [byref] Stream) // {}
}

/// \brief An OutputStream that applies an arbitrary transformation to
/// text before forwarding it on to an underlying OutputStream
struct AdaptedStream<
  UnderlyingStream: OutputStream, 
  Adapter: OutputStreamAdapter
> : OutputStream {

  constructor(base: UnderlyingStream, adapter: Adapter) {
    this.base = GenericIVar(base)
    this.adapter = GenericIVar(adapter)
  }

  func append(text: String) {
    adapter.value.write(text, &base.value)
  }

  func close() -> UnderlyingStream {
    adapter.value.close(&base.value)
    return base.value
  }

  // debugging only
  func dump() -> String {
    return "AdaptedStream " + base.value.dump()
  }

  var adapter: GenericIVar<Adapter>
  var base: GenericIVar<UnderlyingStream>
}

/// \brief An OutputStreamAdapter that upcases all incoming text
struct Uppercase : OutputStreamAdapter {
  func write<Stream: OutputStream>(text: String, stream: [byref] Stream) {
    stream.append(text.uppercase)
  }
  func close<Stream: OutputStream>(stream: [byref] Stream) {} // No buffering here
}

/// \brief A Streamable that applies a transformation to the
/// representation of some underlying Streamable object.
struct AdaptedStreamable<
  Base: Streamable, Adapter: OutputStreamAdapter
> : Streamable {

  constructor(base: Base, adapter: Adapter) {
    this.base = GenericIVar(base)
    this.adapter = GenericIVar(adapter)
  }

  func writeTo<Target: OutputStream>(target: [byref] Target) {
    // create the stream that transforms the representation
    var adaptedTarget = AdaptedStream(target, adapter.value);
    // write the Streamable object to the target stream
    base.value.writeTo(&adaptedTarget)
    // Flush the adapted stream and, in case Target is a value type,
    // write its new value
    target = adaptedTarget.close()
  }

  var base: GenericIVar<Base>
  var adapter: GenericIVar<Adapter>
}

operator infix %% {
  associativity left
  precedence 0
}

func %%<Source:Streamable, Adapter:OutputStreamAdapter>(s: Source, a: Adapter) 
  -> AdaptedStreamable<Source, Adapter> {
  return AdaptedStreamable(s, a)
}

extension String: Streamable {
  func writeTo<Target: OutputStream>(target: [byref] Target) {
    target.append(this)
  }
}

extension String: OutputStream {
  func append(text: String) {
    this += text
  }

  // debugging only
  func dump() -> String {
    return "<" + this + ">"
  }
}

var target = ""
var adaptedStream = AdaptedStream(target, Uppercase())
var fubar = "fuBar"
adaptedStream.append(fubar)
target = adaptedStream.close()
println(target)
println("------------")
("Baz" %% Uppercase()).writeTo(&target)
println(target)
println("------------")
adaptedStream = AdaptedStream(target, Uppercase())
fubar.writeTo(&adaptedStream)
println(adaptedStream.close())
