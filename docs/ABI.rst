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
  entity ::= context 'D'                     // deallocating destructor
  entity ::= context 'd'                     // non-deallocating destructor
  entity ::= context 'C' type                // allocating constructor
  entity ::= context 'c' type                // non-allocating constructor
  entity ::= declaration 'g'                 // getter
  entity ::= declaration 's'                 // setter
  entity ::= declaration                     // other declaration
  declaration ::= context identifier type
  local-marker ::= 'L'

Entity manglings all start with a nominal-type-kind ([COV]), an
identifier ([0-9o]), or a substitution ([S]).  Global manglings start
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
  type ::= 'F' type type                     // function type
  type ::= 'f' type type                     // uncurried function type
  type ::= 'G' type <type>+ '_'              // generic type application
  type ::= 'M' type                          // metatype
  type ::= 'P' protocol-list '_'             // protocol
  type ::= 'Q' index                         // archetype with depth=0
  type ::= 'Qd' index index                  // archetype with depth=M+1
  type ::= 'R' type                          // l-value
  type ::= 'T' tuple-element* '_'            // tuple
  type ::= 'U' generics '_' type             // generic type
  nominal-type ::= known-nominal-type
  nominal-type ::= substitution
  nominal-type ::= nominal-type-kind entity
  nominal-type-kind ::= 'C'                  // class
  nominal-type-kind ::= 'O'                  // oneof
  nominal-type-kind ::= 'V'                  // struct
  tuple-element ::= identifier? type

<type> never begins or ends with a number.
<type> never begins with an underscore.

::

  generics ::= generic-parameter+
  generic-parameter ::= protocol-list '_'
  protocol-list ::= protocol*
  protocol ::= substitution
  protocol ::= entity

<protocol-list> is unambiguous because protocols are always top-level,
and so the structure is quite simple.

::

  known-module ::= 'So'                      // Objective-C
  known-module ::= 'Ss'                      // swift
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

<value-witness-kind> differentiates the kinds of function value
witnesses for a type.

::

  identifier ::= natural identifier-start-char identifier-char*
  identifier ::= 'op' natural operator-char+
  operator-char ::= 'a'                      // &
  operator-char ::= 'd'                      // /
  operator-char ::= 'e'                      // =
  operator-char ::= 'g'                      // >
  operator-char ::= 'l'                      // <
  operator-char ::= 'm'                      // *
  operator-char ::= 'n'                      // !
  operator-char ::= 'o'                      // |
  operator-char ::= 'p'                      // +
  operator-char ::= 'r'                      // %
  operator-char ::= 's'                      // -
  operator-char ::= 'x'                      // ^
  operator-char ::= 't'                      // ~
  operator-char ::= 'z'                      // .

<identifier> is run-length encoded: the natural indicates how many
characters follow.  Operator characters are mapped into ASCII as
given.  Non-ASCII identifier characters are... translated into ASCII
somehow?  In neither case can an identifier start with a digit, so
there's no ambiguity with the run-length.

::

  index ::= '_'                              // 0
  index ::= natural '_'                      // N+1
  natural ::= [0-9]+

<index> is a production for encoding numbers in contexts that can't
end in a digit; it's optimized for encoding smaller numbers.
