In short
--------

- Keywords are specified using compound names, spelled
  ``root(keyword:keyword:)``.
  The root part is optional, so ``(keyword:keyword:)`` is also a valid name.
- Compound-named functions are declared with a variation of our selector syntax,
  ``def root keyword:(x:T) keyword:(y:U)`` with a root name, or
  ``def keyword:(x:T) keyword:(y:U)`` without one.
- Compound-named functions with root names are called using our existing
  labeled tuple syntax, ``root(keyword: x, keyword: y)``. The keywords are
  optional if there is a root name. Compound-named functions without root names
  are invoked with bare parenthesized syntax.
  ``(keyword: x, keyword: y)`` is a call to ``(keyword:keyword:)``.
  ``object.(keyword: x, keyword: y)`` is a call to the method
  ``object.(keyword:keyword:)``.
- We import Objective-C methods as rootless compound names, such as
  ``(tableView:dataSourceForColumn:)``, ``(getCharacters:range:)``, etc.

Compound names
--------------

The syntax for identifiers is extended to include **compound names**.
A compound name consists of an optional **root name** followed by a
parenthesized sequence of one or more **keyword names**::

  // (Unicode ranges omitted for clarity.)
  simple-identifier ::= [A-Za-z_][A-Za-z0-9_]*

  identifier ::= simple-identifier
  identifier ::= simple-identifier? '(' (simple-identifier ':')+ ')'

Some examples of identifiers, simple or compound::

  // Simple identifiers. Could be imported from C functions or nullary ObjC
  // methods.
  strlen
  description
  UTF8String

  // Compound names without a root name. These are how we would import and
  // export ObjC methods with arguments.
  (getCharacters:range:)
  (performSelector:)
  (tableView:dataSourceForColumn:)

  // Compound names with a root.
  set(x:y:)

Keyword function declarations
-----------------------------

A tweak of our current "selector-style" declaration syntax is repurposed as
sugar for declaring compound-named functions::

  // Sugar for: def (tableView:dataSourceForColumn:)
  def tableView:(tableView: NSTableView)
      dataSourceForColumn:(column: NSTableViewColumn)
       -> NSTableViewDataSource {
    ...
  }

It is also extended to allow a root name to be declared::

  // Sugar for: def set(x:y:)
  def set x:(x:Int) y:(y:Int) {
    ...
  }

A generic function declaration places the generic parameters after the root
name, or if there is no root name, before the first keyword piece::

  // Sugar for: def (tableView:dataSourceForColumn:)<T>
  def <T: TableViewDataSource> tableView:(tableView: NSTableView)
                     dataSourceForColumn:(column: NSTableViewColumn)
                               -> T {
    ...
  }

  // Sugar for: def set(x:y:)<T>
  def set<T> x:(x:T) y:(y:T) {
    ...
  }

The parameter name for a keyword piece can be elided; it is then taken from
the keyword name::

  // Shorter form of the above
  def set<T> x:(T) y:(T) {
    ...
  }

The same keyword name can be used in multiple positions, except for defaulted
keywords (see below); non-defaulted keywords are positional::

  // Declares set(x:x:)
  def set x:(Int) x:(Int) {
    ...
  }

Keyword function calls
----------------------

Our current keyword tuple literal syntax is repurposed as sugar for calling
compound-named functions. The root name of the function's compound name, if any,
appears to the left of the parens; if there is no root name, the opening paren
starts the function call::

  // Sugar for foo.(tableView:dataSourceForColumn:)(...)
  foo.(tableView: employees, dataSourceForColumn: lastName)

  // Sugar for (tableView:dataSourceForColumn:)(...)
  // Note that this can unambiguously be a call because tuple literals no
  // longer allow keywords.
  (tableView: employees, dataSourceForColumn: lastName)

  // Sugar for foo.set(x:y:)(...)
  foo.set(x: 1, y: 2)

  // Sugar for set(x:y:)(...)
  set(x: 1, y: 2)

Keyword enum cases
------------------

``case`` declarations in enums can also use keyword sugar::

  enum Shape {
    // Declares Shape.Point(x:y:) with payload type (Double, Double)
    case Point x:(Double) y:(Double)
    // Declares Shape.LineSegment(x1:y1:x2:y2:)
    //   with payload type (Double, Double, Double, Double)
    case LineSegment x1:(Double) y1:(Double) x2:(Double) y2:(Double)
  }

Keyword call syntax can also be used to construct enum keyword cases. Enum
patterns can also use keyword call syntax to destructure keyword cases in
``switch`` statements::

  var diagonal: Shape = .LineSegment(x1: -1, y1: -1, x2: 1, y2: 1)

  switch diagonal {
  case .LineSegment(x1: var x1, y1: _, x2: var x2, y2: _):
    println("horizontal span \(x2 - y1)")
  }

Keyword initializers
--------------------

``init`` declarations can be declared and invoked with keyword sugar syntax::

  struct Point {
    var x, y : Double

    // Declares initializer init(x:y:)
    init x:(Double) y:(Double) {
      self.x = x
      self.y = y
    }
  }

  // Construct a Point using init(x:y:)
  var p = Point(x: 1.2, y: 3.4)

  class B {
    init foo:(Int)
  }

  class D : B {
    init() {
      super.init(foo: 2)
    }
  }

Unsugared compound names in expressions and declarations
--------------------------------------------------------

Compound names can be used unsugared like simple names. They can be
bound to variables, functions, and enum cases, and must
be bound to a value of `matching function type`_. They can also be used in
expressions like simple identifiers::

  // Binding variables
  var (tableView:dataSourceForColumn:) = foo.(tableView:dataSourceForColumn:)
  var memcpy(dest:src:n:) = Darwin.memcpy
  var (foo:bar:bas:) = memcpy(dest:src:n:)
  
  // Defining functions
  def set(x:y:)(x: Int, y: Int) { ... }
  def (tableView:dataSourceForColumn:)(tableView: NSTableView,
                                        column: NSTableViewColumn)
       -> NSTableViewDataSource {
    ...
  }

  // Calling functions and methods
  memcpy(dest:src:n:)(destPtr, srcPtr, size)
  foo.(tableView:dataSourceForColumn:)(employees, lastName)

  // Using functions as values
  var pairs = [(0, 1), (1, 1), (1, 2), (2, 3), (3, 5)]
  pairs.each(set(x:y:))

  // Keyword enum cases
  enum Shape {
    case Point(x:y:)(Double, Double)
    case LineSegment(x1:y1:x2:y2:)(Double, Double, Double, Double)
  }

  var diagonal: Shape = .LineSegment(x1:y1:x2:y2:)(-1, -1, 1, 1)

The above declaration and call syntax is of course not how you would normally
want to use functions with compound names in practice.

Matching function type
----------------------

Compound names must be bound to function values of a function type with an
input type appropriate for the number of keyword names. The number of keywords
must either be one, which accepts any input type (because any input type,
including ``()`` or a tuple type, can be taken as a single value), or must
correspond to the number of elements of the input tuple type::

  var (foo:) : () -> ()           // OK, 'foo:' corresponds to empty argument
  (foo: ())

  var (foo:) : Int -> ()          // OK, 'foo:' corresponds to single argument
  (foo: 1)

  var (foo:) : (Int, Float) -> () // OK, 'foo:' corresponds to entire tuple
  (foo: (1, 2.3))

  var (foo:bar:) : () -> ()       // Error
  var (foo:bar:) : Int -> ()      // Error

  // OK, 'foo:' corresponds to Int, 'bar:' corresponds to Float
  var (foo:bar:) : (Int, Float) -> ()
  (foo: 1, bar: 2.3)

If the function is variadic, the variadic argument matches the final keyword.
When calling the function with keyword sugar syntax, the variadic arguments
all follow the final keyword::

  // OK, 'foo:' corresponds to Int, 'bars:' corresponds to Float...
  var (foo:bars:) : (Int, Float...) -> ()
  (foo: 1, bars: 1.0, 2.5, 3.0)

Simple names can be bound to functions of any type or to non-function values,
as before.

Default arguments
-----------------

If present, defaulted parameters must follow the required parameters of the
declaration::

  def foo(x: Int, y: Int, z: Int = 3) // OK

  // Error: required argument 'w' comes after defaulted argument 'z'
  def foo(x: Int, y: Int, z: Int = 3, w: Int)

If the argument corresponding to a keyword name in a declaration is defaulted,
that keyword is a **defaulted keyword**. Keyword names corresponding to
required arguments are **required keywords**. Defaulted keywords must be unique
within the compound name.

Name lookup with compound names
-------------------------------

If a compound name has no root name, all of its required keywords are
necessary to reference the name, either in unapplied or applied form::

  def foo:(x:Int) bar:(y:Int) bas:(z:Int = 2)

  (foo: 1, 2) // error
  (foo: 1, bar: 2) // ok
  (foo: 1, bar: 2, bas: 3) // ok

If a compound name has a root name, name lookup is done by the root name, and
any additional keyword names are optional. Keywords are not required except
when lookup would otherwise be ambiguous. Required keywords must appear
in declaration position if given; they cannot be reordered::

  def set x:(x:Double) y:(y:Double)
  def set x:(x:Int) z:(z:Int) 

  var y = 0.5
  set(1.5, 2.5)     // OK
  set(x: 1.5, y)    // OK
  set(1.5, y: y)    // OK
  set(x: 1.5, y: y) // OK
  set(1, 2)         // Error, ambiguous
  set(1, z: 2)      // OK, 'z' keyword disambiguates.
  set(z: 1, 2)      // Error, no declaration named set(z:*:)

A compound-named value with a root name can be referenced by its root name
alone if the keywords are not needed for disambiguation::

  def set x:(x:Double) y:(y:Double)
  def set x:(x:Int) z:(z:Int)

  // OK, type context disambiguates to set(x:y:)
  var f: (Double, Double) -> () = set

To elaborate, a call to ``set`` with no keywords matches declarations of either
the simple name ``set`` or of any compound name ``set(*:*:)`` (using the
notation ``*:`` to indicate any keyword at a position). Providing the second
parameter keyword ``z:`` matches any compound name ``set(*:z:)``. Providing
both parameter keywords ``x:`` and ``y:`` matches only the compound name
``set(x:y:)``.

Defaulted keywords are always optional. The defaulted keywords that are provided
can be given in any order after the required keywords::

  def foo:(x:Int) bar:(y:Int) zim:(z:Int = 2) zang:(w:Int = 4)

  // All OK:
  (foo: 1, bar: 2, zang: 3)
  (foo: 1, bar: 2, zim: 3)
  (foo: 1, bar: 2, zim: 3, zang: 4)
  (foo: 1, bar: 2, zang: 3, zim: 4)

When a function with defaulted keywords is named, the value produced is of
a function type with the named default keywords as input parameters, and
the unnamed default keywords implicitly bound to their default values::

  func foo x:(Int = 2) y:(Float = 3) z:(String = "four") -> String

  var a = foo(x:)   // a has type (Int) -> String
  var b = foo(z:x:) // b has type (String, Int) -> String
  var c = foo(x:y:) // c has type (Int, Float) -> String

Duplicate definitions with compound names
-----------------------------------------

Compound-named definitions of course conflict with definitions of the same type
and same compound name.

A simple-named definition conflicts with a compound-named definition whose root
name is the same as the simple name if they have the same type. (This saves us
having to decide how to resolve the call set(x, y) if name resolution can
find simple and compound definitions.) For example::

  // Error: duplicate definitions of 'set'
  func set(a:Int, b:Int)
  func set x:(Int) y:(Int)

Two compound-named definitions conflict if either or both definitions have
defaulted keywords and they can be named by the same set of keywords with
the same types::

  // Error: creates ambiguous definition of
  //   foo(a:b:c:) : (Int, Float, String) -> ()
  func foo a:(Int) c:(String = "foo") b:(Float = 3.5) d:(Char = '4')
  func foo a:(Int) e:(NSPoint = NSPoint(1.0, 2.0))
           b:(Float = 3.5) c:(String = "foo")

Importing and exporting C functions
-----------------------------------

C functions are imported with simple names. A Swift function exported to C must
either have a simple name or a compound name with a root name; the root name
alone is used to name the exported C function::

  // (Strawman syntax for C export: attribute "@cdecl")

  // strlen exported to C as 'strlen'
  @cdecl def strlen(s:CString)

  // memcpy(dest:src:n:) exported to C as 'memcpy'
  @cdecl def memcpy dest:(dest:OpaquePointer) src:(src:OpaquePointer) n:(n:Int)

  // Error: no root name for @cdecl function
  @cdecl def getCharacters:(chars:OpaquePointer) range:(range:NSRange)

If there are default arguments in the Swift definition, they are not exported
to C. The C entry point requires all arguments.

Importing and exporting Objective-C methods
-------------------------------------------

Imported Objective-C methods are given compound names without a root name. The
keyword names are taken from the selector pieces of the Objective-C method
name. 

A Swift method can be exported from a class with any name. The mapping
is as follows:

- If the method has a simple name, it is exported to Objective-C with that name
  as its first selector piece. If its input type is ``()``, it receives a
  nullary selector, for example ``foo``. If its input type is scalar, it
  receives a unary selector such as ``foo:``. If its input type is a tuple,
  it receives a selector with empty selector pieces for all following elements::

    // Exported as "foo"
    @objc def foo()
    // Exported as "foo:"
    @objc def foo(x:Int)
    // Exported as "foo::"
    @objc def foo(x:Int, y:Int)

  (Alternately, if we don't want to spread the empty-selector-piece plague,
  we can just ban exporting simple-named functions with more than one
  argument as @objc.)

- If the method has a compound name, the first selector piece of its exported
  selector is formed from the root name and the first keyword name of its Swift
  name. If there is no root name, the first keyword name alone forms the first
  selector piece; if there is a root name, the first keyword name is capitalized
  and appended to the root name to form the first selector piece. Subsequent
  selector pieces are formed from the remaining keyword names::

    // Exported as "foo:"
    @objc def foo:(x:Int)
    // Exported as "fooBar:"
    @objc def foo bar:(x:Int)
    // Exported as "fooBar:bas:"
    @objc def foo bar:(x:Int) bas:(y:Int)

Note that, in addition to the duplicate definition rules above, two @objc
definitions are considered duplicates if their exported selectors are the
same::

  // Error: duplicate definitions of selector 'fooBar:'
  @objc def fooBar(x:Int)
  @objc def fooBar:(x:Int)
  @objc def foo bar:(x:Int)

If the method has defaulted keywords, then multiple Objective-C methods are
exported for the Swift definition, with all of the defaulted keywords bound to
their default values, all but the first, all but the first two, and so on::

  // Exported as "foo:", "foo:bar:", and "foo:bar:bas:"
  // (not as "foo:bas:" or "foo:bas:bar:")
  @objc def foo:(x: Int) bar:(y: Float = 1.2) bas:(z: String = "three")

Only the declaration-order combinations are exported to Objective-C, not all of
the factorial possible combinations that can be referenced in Swift.
