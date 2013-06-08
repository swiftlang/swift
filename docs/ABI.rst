.. @raise litre.TestsAreMissing
.. _ABI:

The Swift ABI
=============

Hard Constraints on Resilience
------------------------------

The root of a class hierarchy must remain stable, at pain of
invalidating the metaclass hierarchy.  Note a Swift class without an
explicit base class is implicitly rooted in the SwiftObject
Objective-C class.

Mangling
--------
::

  mangled-name ::= '_T' global

All Swift-mangled names begin with this prefix.

::

  global ::= 'M' directness type             // type metadata
  global ::= 'MP' directness type            // type metadata pattern
  global ::= 'Mm' type                       // class metaclass
  global ::= 'w' value-witness-kind type     // value witness
  global ::= 'WV' type                       // value witness table
  global ::= 'Wo' entity                     // witness table offset
  global ::= 'Wv' directness entity          // field offset
  global ::= local-marker? entity            // some identifiable thing
  global ::= 'To' global                     // swift-as-ObjC thunk
  global ::= 'Tb' type                       // swift-to-ObjC block converter
  entity ::= context 'D'                     // deallocating destructor
  entity ::= context 'd'                     // non-deallocating destructor
  entity ::= context 'C' type                // allocating constructor
  entity ::= context 'c' type                // non-allocating constructor
  entity ::= declaration 'g'                 // getter
  entity ::= declaration 's'                 // setter
  entity ::= declaration 'a'                 // addressor
  entity ::= declaration                     // other declaration
  declaration ::= declaration-name type
  declaration-name ::= context identifier
  local-marker ::= 'L'

Entity manglings all start with a nominal-type-kind ([COPV]), an
identifier ([0-9oX]), or a substitution ([S]).  Global manglings start
with any of those or [MTWw].

::

  directness ::= 'd'                         // direct
  directness ::= 'i'                         // indirect

A direct symbol resolves directly to the address of an object.  An
indirect symbol resolves to the address of a pointer to the object.
They are distinct manglings to make a certain class of bugs
immediately obvious.

The terminology is slightly overloaded when discussing offsets.  A
direct offset resolves to a variable holding the true offset.  An
indirect offset resolves to a variable holding an offset to be applied
to type metadata to get the address of the true offset.  (Offset
variables are required when the object being accessed lies within a
resilient structure.  When the layout of the object may depend on
generic arguments, these offsets must be kept in metadata.  Indirect
field offsets are therefore required when accessing fields in generic
types where the metadata itself has unknown layout.)

::

  context ::= module
  context ::= function
  context ::= nominal-type
  context ::= protocol-context
  module ::= substitution                    // other substitution
  module ::= identifier                      // module name
  module ::= known-module                    // abbreviation
  function ::= entity

  type ::= 'A' natural type                  // fixed-size array
  type ::= 'Bf' natural '_'                  // Builtin.Float
  type ::= 'Bi' natural '_'                  // Builtin.Integer
  type ::= 'BO'                              // Builtin.ObjCPointer
  type ::= 'Bo'                              // Builtin.ObjectPointer
  type ::= 'Bp'                              // Builtin.RawPointer
  type ::= 'Bu'                              // Builtin.OpaquePointer
  type ::= nominal-type
  type ::= 'b' type type                     // objc block function type
  type ::= 'F' type type                     // function type
  type ::= 'f' type type                     // uncurried function type
  type ::= 'G' type <type>+ '_'              // generic type application
  type ::= 'M' type                          // metatype
  type ::= 'P' protocol-list '_'             // protocol type
  type ::= 'Q' index                         // archetype with depth=0
  type ::= 'Qd' index index                  // archetype with depth=M+1
  type ::= 'R' type                          // byref
  type ::= 'T' tuple-element* '_'            // tuple
  type ::= 't' tuple-element* '_'            // variadic tuple
  type ::= 'U' generics '_' type             // generic type
  nominal-type ::= known-nominal-type
  nominal-type ::= substitution
  nominal-type ::= nominal-type-kind declaration-name
  nominal-type-kind ::= 'C'                  // class
  nominal-type-kind ::= 'O'                  // oneof
  nominal-type-kind ::= 'V'                  // struct
  protocol-context ::= 'P' protocol
  tuple-element ::= identifier? type

<type> never begins or ends with a number.
<type> never begins with an underscore.

Note that protocols mangle differently as types and as contexts. A protocol
context always consists of a single protocol name and so mangles without a
trailing underscore. A protocol type can have zero, one, or many protocol bounds
which are juxtaposed and terminated with a trailing underscore.

::

  generics ::= generic-parameter+
  generic-parameter ::= protocol-list '_'
  protocol-list ::= protocol*
  protocol ::= substitution
  protocol ::= declaration-name

<protocol-list> is unambiguous because protocols are always top-level,
so the structure is quite simple.

::

  value-witness-kind ::= 'al'                // allocateBuffer
  value-witness-kind ::= 'ac'                // assignWithCopy
  value-witness-kind ::= 'at'                // assignWithTake
  value-witness-kind ::= 'de'                // deallocateBuffer
  value-witness-kind ::= 'xx'                // destroy
  value-witness-kind ::= 'XX'                // destroyBuffer
  value-witness-kind ::= 'CP'                // initializeBufferWithCopyOfBuffer
  value-witness-kind ::= 'Cp'                // initializeBufferWithCopy
  value-witness-kind ::= 'cp'                // initializeWithCopy
  value-witness-kind ::= 'Tk'                // initializeBufferWithTake
  value-witness-kind ::= 'tk'                // initializeWithTake
  value-witness-kind ::= 'pr'                // projectBuffer
  value-witness-kind ::= 'ty'                // typeof

<value-witness-kind> differentiates the kinds of function value
witnesses for a type.

::

  identifier ::= natural identifier-start-char identifier-char*
  identifier ::= 'o' operator-fixity natural operator-char+

  operator-fixity ::= 'p'                    // prefix operator
  operator-fixity ::= 'P'                    // postfix operator
  operator-fixity ::= 'i'                    // infix operator

  operator-char ::= 'a'                      // & 'and'
  operator-char ::= 'c'                      // @ 'commercial at'
  operator-char ::= 'd'                      // / 'divide'
  operator-char ::= 'e'                      // = 'equals'
  operator-char ::= 'g'                      // > 'greater'
  operator-char ::= 'l'                      // < 'less'
  operator-char ::= 'm'                      // * 'multiply'
  operator-char ::= 'n'                      // ! 'not'
  operator-char ::= 'o'                      // | 'or'
  operator-char ::= 'p'                      // + 'plus'
  operator-char ::= 'r'                      // % 'remainder'
  operator-char ::= 's'                      // - 'subtract'
  operator-char ::= 't'                      // ~ 'tilde'
  operator-char ::= 'x'                      // ^ 'xor'
  operator-char ::= 'z'                      // . 'zperiod'

<identifier> is run-length encoded: the natural indicates how many
characters follow.  Operator characters are mapped to letter characters as
given. In neither case can an identifier start with a digit, so
there's no ambiguity with the run-length.

::

  identifier ::= 'X' natural identifier-start-char identifier-char*
  identifier ::= 'X' 'o' operator-fixity natural identifier-char*

Identifiers that contain non-ASCII characters are encoded using the Punycode
algorithm specified in RFC 3492, with the modifications that ``_`` is used
as the encoding delimiter, and uppercase letters A through J are used in place
of digits 0 through 9 in the encoding character set. The mangling then
consists of an ``X`` followed by the run length of the encoded string and the
encoded string itself. For example, the identifier ``vergüenza`` is mangled
to ``X12vergenza_JFa``. (The encoding in standard Punycode would be
``vergenza-95a``)

Operators that contain non-ASCII characters are mangled by first mapping the
ASCII operator characters to letters as for pure ASCII operator names, then
Punycode-encoding the substituted string. The mangling then consists of
``Xo`` followed by the fixity, run length of the encoded string, and the encoded
string itself. For example, the infix operator ``«+»`` is mangled to
``Xoi7p_qcaDc`` (``p_qcaDc`` being the encoding of the substituted
string ``«p»``).

::

  substitution ::= 'S' index

<substitution> is a back-reference to a previously mangled entity. The mangling
algorithm maintains a mapping of entities to substitution indices as it runs.
When an entity that can be represented by a substitution (a module, nominal
type, or protocol) is mangled, a substitution is first looked for in the
substitution map, and if it is present, the entity is mangled using the
associated substitution index. Otherwise, the entity is mangled normally, and
it is then added to the substitution map and associated with the next
available substitution index.

For example,  in mangling a function type
``(zim.zang.zung, zim.zang.zung, zim.zippity) -> zim.zang.zoo`` (with module
``zim`` and class ``zim.zang``),
the recurring contexts ``zim``, ``zim.zang``, and ``zim.zang.zung``
will be mangled using substitutions after being mangled
for the first time. The first argument type will mangle in long form,
``CC3zim4zang4zung``, and in doing so, ``zim`` will acquire substitution ``S_``,
``zim.zang`` will acquire substitution ``S0_``, and ``zim.zang.zung`` will
acquire ``S1_``. The second argument is the same as the first and will mangle
using its substitution, ``CS1_``. The
third argument type will mangle using the substitution for ``zim``,
``CS_7zippity``. (It also acquires substitution ``S2_`` which would be used
if it mangled again.) The result type will mangle using the substitution for
``zim.zang``, ``CS0_zoo`` (and acquire substitution ``S3_``). The full
function type thus mangles as ``fTCC3zim4zang4zungCS1_CS_7zippity_CS0_zoo``.

::

  known-module ::= 'So'                      // Objective-C
  known-module ::= 'Ss'                      // swift
  known-nominal-type ::= 'Sa'                // swift.Slice
  known-nominal-type ::= 'Sb'                // swift.Bool
  known-nominal-type ::= 'Sc'                // swift.Char
  known-nominal-type ::= 'Sd'                // swift.Float64
  known-nominal-type ::= 'Sf'                // swift.Float32
  known-nominal-type ::= 'Si'                // swift.Int64
  known-nominal-type ::= 'SS'                // swift.String
  known-nominal-type ::= 'Su'                // swift.UInt64

<known-module> and <known-nominal-type> are built-in substitutions for
certain common entities.  Like any other substitution, they all start
with 'S'.

The Objective-C module is used as the context for mangling Objective-C
classes as <type>s.

::

  index ::= '_'                              // 0
  index ::= natural '_'                      // N+1
  natural ::= [0-9]+

<index> is a production for encoding numbers in contexts that can't
end in a digit; it's optimized for encoding smaller numbers.

