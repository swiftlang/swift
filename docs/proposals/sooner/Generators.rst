:orphan:

Python popularized the concept of generators, resumable functions that lazily
produce a sequence of values. For example, one could write a lazy UTF8 encoder
like so::

  func encodeUTF8<
    S: Sequence where S.Element == UnicodeScalar
  >(s: Sequence) -> GeneratorFunc<UInt8> {
    for scalar in s {
      let value = UInt32(scalar)
      switch value {
      case 0..0x7F:
        yield UInt8(value)
      case 0x80..0x7FF:
        yield 0xC0 | UInt8(value >>  6       )
        yield 0x80 | UInt8(value       & 0x3F)
      case 0x800..0xFFFF:
        yield 0xE0 | UInt8(value >> 12       )
        yield 0x80 | UInt8(value >>  6 & 0x3F)
        yield 0x80 | UInt8(value       & 0x3F)
      case 0x1_0000..0x10_FFFF:
        yield 0xF0 | UInt8(value >> 18       )
        yield 0x80 | UInt8(value >> 12 & 0x3F)
        yield 0x80 | UInt8(value >>  6 & 0x3F)
        yield 0x80 | UInt8(value       & 0x3F)
      }
    }
  }

Generators are extremely popular in Python and have since been ported to other
popular languages including C# and Javascript. Generators provide a solution
to an instance of the **inversion of control** problem, wherein an algorithm
with a naturally sequential definition, such as UTF-8 decoding above, needs to
yield control to another algorithm in order. Without generators, awkward state
machine types need to be manually written, obscuring the algorithm being
expressed. Python and Javascript have extended their generators so that
``yield`` is a bidirectional operation, and C# has added ``async`` functions,
which use a similar inversion-of-control transform to make asynchronous code
readable.

This proposal aims to provide a single language-level feature for inversion of
control that is general enough to support both traditional generator functions
as well as other useful inversion-of-control patterns, such as sink functions,
``async/await``-like asynchronous tasks, and general monadic control flow.

Yieldable types
---------------

A type is a **yieldable type** if it conforms to the ``Yieldable`` protocol::

  protocol Yieldable {
    // The type that is output at each continuation point of the generator.
    typealias ContinuationType

    // Prime the generator with the entry point continuation.
    class func start(entry: () -> ContinuationType) -> Self
  }

In addition to the formal requirements of the ``Yieldable`` protocol, the
type must have class methods ``\`yield\``` and ``\`return\```, which may be
overloaded arbitrarily as long as they follow these patterns (where ``_``
can be an arbitrary type)::

    // Yield a value from the generator, and accept a continuation function
    // pointer to resume the generator.
    class func `yield`(value: _, next: _ -> ContinuationType)
    -> ContinuationType

    // Return the final value from the generator.
    class func `return`(value: _) -> ContinuationType

The ``yield`` Expression and Generator Transform
------------------------------------------------

If a function returns a yieldable type, and the function body contains one or
both of:

- a ``yield`` expression, or
- a ``return`` statement matching the yieldable type's ``\`return\``` method's
  argument type,

then the function body undergoes generator transformation. The function is
turned "inside out" at every ``yield`` expression; the ``yield`` expression
becomes a call to the yieldable type's ``\`yield\``` method with the operand
to yield and a continuation closure that resumes execution of the function body.
``return`` statements in the generator get mapped to a call to the yieldable
type's ``\`return\``` method. At the top level, the entry point to the
function body is passed as a closure into the yieldable type's ``start`` method
to form the formal result of the generator function.

As an example, this function::

  func foo() -> YieldableType {
    return yield (yield (yield 1) + 2) + 3
  }

would be turned into the equivalent of::

  func foo() -> YieldableType {
    return YieldableType.start {
      return YieldableType.`yield`(1) {
        return YieldableType.`yield`($0 + 2) {
          return YieldableType.`return`($0 + 3)
        }
      }
    }
  }

When ``yield`` appears amid more complex control flow, the transformation can
be viewed as wrapping the function body in a Duff's device. For example,
this generator::

  func fibonacci() -> GeneratorFunc<Int> {
    var (a, b) = (0, 1)
    while true {
      yield b
      (a, b) = (b, a + b)
    }
  }

could be thought of as being rewritten to::

  func fibonacci() -> GeneratorFunc<Int> {
    enum State {
      case S0
      case S1
    }
    var state: State = .S0
    func body() -> GeneratorFunc<Int>.ContinuationType {
      // Pretend we had Duff's device in Swift
      switch state {
      case S0:
        var (a, b) = (0, 1)
        while true {
          return GeneratorFunc<Int>.`yield`(b) { state = .S1; return body() }
      case S1:
          (a, b) = (b, a + b)
        }
      }
    }
    return GeneratorFunc<Int>.start(body)
  }

This transformation is done by a late-stage SIL pass, which converts a SIL
function containing special ``callcc`` instructions into the state machine or
chained-closure representation.  Doing the transformation in SIL keeps the
frontend codegen for ``yield`` simple, and allows optimization passes to
operate over the local state of the generator normally before it is obscured by
the coroutine representation.

Examples
--------

GeneratorFunc<T>
~~~~~~~~~~~~~~~~

The most obvious yieldable type is the generator function, which
emits a sequence of the ``yield``-ed values from its body::

  struct GeneratorFunc<T>: Generator {
    typealias ContinuationType = (GeneratorFunc, T?)
    var _next: () -> ContinuationType

    static func start(entry: () -> ContinuationType) -> GeneratorFunc {
      return GeneratorFunc(entry)
    }

    static func `yield`(value: T, next: () -> ContinuationType)
    -> ContinuationType {
      return (GeneratorFunc(next), value)
    }

    static func `return`() -> ContinuationType {
      return (GeneratorFunc({ fatal("generator exhausted") }), nil)
    }

    mutating func next() -> T? {
      let (nextSelf, nextVal) = _next()
      self = nextSelf
      return nextVal
    }
  }

An example::

  func fibonacci() -> GeneratorFunc<Int> {
    var (a, b) = (0, 1)
    while true {
      yield b
      (a, b) = (b, a + b)
    }
  }

  // Print the Fibonacci sequence up to 1000
  for x in fibonacci() {
    print(x)
    if x > 1000 { break }
  }

SinkFunc<T>
~~~~~~~~~~~

Sink functions, the duals to generators, can also be implemented as a yieldable
type::

  struct SinkFunc<T>: Sink {
    typealias ContinuationType = SinkFunc
    var _next: (T -> SinkFunc)?

    static func start(entry: () -> SinkFunc) -> SinkFunc {
      return entry()
    }

    /// yield takes void and produces the next 'put' value
    static func `yield`((), next: T -> SinkFunc) -> SinkFunc {
      return SinkFunc(next)
    }

    static func `return`() -> SinkFunc {
      return SinkFunc(nil)
    }

    mutating func put(value: T) {
      if let next = _next {
        self = _next(value)
      }
    }
  }

A sink function receives the next value ``put`` into the sink after every
yield. This can be used to implement stateful event handlers::

  enum MouseEvent {
    case MouseDown(time: Double)
    case MouseUp(time: Double)
  }

  // A sink that accepts click events and detects double-clicks.
  func senseDoubleClicks(clickDelay: Double,
                         doubleClickDelay: Double,
                         onDoubleClick: Double -> ()) -> Sink<MouseEvent> {
    while true {
      // Wait for the first mouse-down.
      var time: Double
      switch yield() {
      case .MouseDown(let t):
        time = t
      default:
        continue
      }

      // Wait for the first mouse-up.
      switch yield() {
      // Was it short enough to be considered a click?
      case .MouseUp(let t) where t - time < clickDelay:
        time = t
      default:
        continue
      }

      // Wait for the second mouse-down.
      switch yield() {
      // Was it soon enough to be considered a double-click?
      case .MouseDown(let t) where t - time < doubleClickDelay:
        time = t
      default:
        continue
      }

      // Wait for the second mouse-up.
      switch yield() {
      // Was it short enough to be considered a click?
      case .MouseUp(let t) where t - time < clickDelay:
        // Double-click successful!
        onDoubleClick(t)
      }
    }
  }

  let handler = senseDoubleClicks(clickDelay: 0.1,
                                  doubleClickDelay: 1.0,
                                  onDoubleClick: { print("double-click!") })
  // Simulate an event sequence
  handler.put(.MouseDown(0.0))
  handler.put(.MouseUp(0.05))
  handler.put(.MouseDown(0.5))
  handler.put(.MouseDown(0.55))

Monadic Control Flow
~~~~~~~~~~~~~~~~~~~~

For a monadic type, such as ``Optional`` or ``Error``,
``yield`` and ``return`` can map to the monadic "bind" and "return"
concepts::

  protocol Monad<T>: Yieldable {
    // The monad operations:
    func bind<U>(T -> Self<U>) -> Self<U>
    func mreturn(T) -> Self<T>

    // Default implementation of Yieldable for a monad.
    typealias ReturnType = T
    typealias ContinuationType = Self<T>

    class func start(entry: () -> Self<T>) -> Self<T> {
      return entry()
    }
    class func `yield`<U>(value: Self<U>, next: U -> Self<T>) -> Self<T> {
      return value.bind(next)
    }
    class func `return`(value: T) -> Self<T> {
      return mreturn(value)
    }
  }

This means that generators can be used as a shorthand for monadic control flow.
For example, given an ``Error`` monad type defined thus::

  enum Result<T, Error>: Monad<T> {
    case OK(T)
    case Fail(Error)

    func bind<U>(f: T -> Result<U, Error>) -> Result<U, Error> {
      switch self {
      case OK(let val):
        return f(val)
      case Fail:
        return self
      }
    }
    func return(x: T) {
      return OK(x)
    }

  }

one can implement a "try" block using ``Result<T, Error>`` as a yieldable type::

  // Try a computation that may produce an error.
  func try<Error>(block: () -> Result<(), Error>) -> Result<(), Error> {
    return block()
  }

  extension Result {
    // Handle an error.
    func catch(f: Error -> T) -> T {
      switch self {
      case OK(let val):
        return val
      case Fail(let err):
        return f(err)
      }
    }
  }

  try {
    // InputFile.open(String) -> Result<InputFile, IOError>
    let file = yield InputFile.open("/usr/share/dict/words")
    // InputFile.contents: Result<String, IOError>
    let contents = yield file.contents

    for line in contents.lines {
      if line.endswith("gry") {
        print(line)
      }
    }
  }.catch { err in
    print("failed to read /usr/share/dict/words: \(err)")
  }

The equivalent of C#'s ``async``/``await`` can also be implemented using
monadic promise objects.
