/// \brief A thing into which we can stream text
protocol OutputStream {
  func append(text: String)
}

struct StreamAdaptation<S: Streamable, G: StreamAdapterGenerator> {
  G.
}

struct StatelessOutputStreamAdaptor<Base: OutputStream> {
  func append(text: String) {
    base.value.append(text)
  }
  
  func flush() -> Base {
    return base.value
  }
  
  var base: GenericIVar<Base>
  var transform : (String) -> String
}

// x %% uppercase() %% whatever

operator infix %% {
  associativity left
  precedence 0
}

// \brief A thing that can be written to an OutputStream
protocol Streamable {
  func write<S: OutputStream>(s: [byref] S)
}

class OutputStreamAdapter<Base: OutputStream> : OutputStream {
  func append(text: String) {
    
  }
}

struct StreamableAdapter<Base: Streamable, Transformer: StreamAdaptation> {
  constructor(base: Base) {
    this.base.value = base
  }

  func write<S: OutputStream>(s: [byref] S) {
    var adaptedStream = Transformer.adapt(s)
    base.value.write(adaptedStream)
    s = adaptedStream.flush()
  }
  
  base: GenericIVar<Base>
}
