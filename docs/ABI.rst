:orphan:

.. @raise litre.TestsAreMissing
.. _ABI:

The Swift ABI
=============

.. contents::

Hard Constraints on Resilience
------------------------------

The root of a class hierarchy must remain stable, at pain of
invalidating the metaclass hierarchy.  Note a Swift class without an
explicit base class is implicitly rooted in the SwiftObject
Objective-C class.

Type Layout
-----------

Fragile Struct and Tuple Layout
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Structs and tuples currently share the same layout algorithm, noted as the
"Universal" layout algorithm in the compiler implementation. The algorithm
is as follows:

- Start with a **size** of **0** and an **alignment** of **1**.
- Iterate through the fields, in element order for tuples, or in ``var`` 
  declaration order for structs. For each field:

  * Update **size** by rounding up to the **alignment of the field**, that is,
    increasing it to the least value greater or equal to **size** and evenly
    divisible by the **alignment of the field**.
  * Assign the **offset of the field** to the current value of **size**.
  * Update **size** by adding the **size of the field**.
  * Update **alignment** to the max of **alignment** and the
    **alignment of the field**.

- The final **size** and **alignment** are the size and alignment of the
  aggregate. The **stride** of the type is the final **size** rounded up to 
  **alignment**.

Note that this differs from C or LLVM's normal layout rules in that *size*
and *stride* are distinct; whereas C layout requires that an embedded struct's
size be padded out to its alignment and that nothing be laid out there,
Swift layout allows an outer struct to lay out fields in the inner struct's
tail padding, alignment permitting. Unlike C, zero-sized structs and tuples
are also allowed, and take up no storage in enclosing aggregates. The Swift
compiler emits LLVM packed struct types with manual padding to get the
necessary control over the binary layout. Some examples:

::

  // LLVM <{ i64, i8 }>
  struct S {
    var x: Int
    var y: UInt8
  }

  // LLVM <{ i8, [7 x i8], <{ i64, i8 }>, i8 }>
  struct S2 {
    var x: UInt8
    var s: S
    var y: UInt8
  }

  // LLVM <{}>
  struct Empty {}

  // LLVM <{ i64, i64 }>
  struct ContainsEmpty {
    var x: Int
    var y: Empty
    var z: Int
  }

Class Layout
~~~~~~~~~~~~

Swift relies on the following assumptions about the Objective-C runtime,
which are therefore now part of the Objective-C ABI:

- 32-bit platforms never have tagged pointers.  ObjC pointer types are
  either nil or an object pointer.

- On x86-64, a tagged pointer either sets the lowest bit of the pointer
  or the highest bit of the pointer.  Therefore, both of these bits are
  zero if and only if the value is not a tagged pointer.

- On ARM64, a tagged pointer always sets the highest bit of the pointer.

- 32-bit platforms never perform any isa masking.  ``object_getClass``
  is always equivalent to ``*(Class*)object``.

- 64-bit platforms perform isa masking only if the runtime exports a
  symbol ``uintptr_t objc_debug_isa_class_mask;``.  If this symbol
  is exported, ``object_getClass`` on a non-tagged pointer is always
  equivalent to ``(Class)(objc_debug_isa_class_mask & *(uintptr_t*)object)``.

- The superclass field of a class object is always stored immediately
  after the isa field.  Its value is either nil or a pointer to the
  class object for the superclass; it never has other bits set.

The following assumptions are part of the Swift ABI:

- Swift class pointers are never tagged pointers.

TODO

Fragile Enum Layout
~~~~~~~~~~~~~~~~~~~

In laying out enum types, the ABI attempts to avoid requiring additional
storage to store the tag for the enum case. The ABI chooses one of five
strategies based on the layout of the enum:

Empty Enums
```````````

In the degenerate case of an enum with no cases, the enum is an empty type.

::

  enum Empty {} // => empty type

Single-Case Enums
`````````````````

In the degenerate case of an enum with a single case, there is no
discriminator needed, and the enum type has the exact same layout as its
case's data type, or is empty if the case has no data type.

::

  enum EmptyCase { case X }             // => empty type
  enum DataCase { case Y(Int, Double) } // => LLVM <{ i64, double }>

C-Like Enums
````````````

If none of the cases has a data type (a "C-like" enum), then the enum
is laid out as an integer tag with the minimal number of bits to contain
all of the cases. The machine-level layout of the type then follows LLVM's
data layout rules for integer types on the target platform. The cases are
assigned tag values in declaration order.

::

  enum EnumLike2 { // => LLVM i1
    case A         // => i1 0
    case B         // => i1 1
  }

  enum EnumLike8 { // => LLVM i3
    case A         // => i3 0
    case B         // => i3 1
    case C         // => i3 2
    case D         // etc.
    case E
    case F
    case G
    case H
  }

Discriminator values after the one used for the last case become *extra
inhabitants* of the enum type (see `Single-Payload Enums`_).

Single-Payload Enums
````````````````````

If an enum has a single case with a data type and one or more no-data cases
(a "single-payload" enum), then the case with data type is represented using
the data type's binary representation, with added zero bits for tag if
necessary. If the data type's binary representation
has **extra inhabitants**, that is, bit patterns with the size and alignment of
the type but which do not form valid values of that type, they are used to
represent the no-data cases, with extra inhabitants in order of ascending
numeric value matching no-data cases in declaration order. If the type
has *spare bits* (see `Multi-Payload Enums`_), they are used to form extra
inhabitants. The enum value is then represented as an integer with the storage
size in bits of the data type. Extra inhabitants of the payload type not used
by the enum type become extra inhabitants of the enum type itself.

::

  enum CharOrSectionMarker { => LLVM i32
    case Paragraph            => i32 0x0020_0000
    case Char(UnicodeScalar)  => i32 (zext i21 %Char to i32)
    case Chapter              => i32 0x0020_0001
  }

  CharOrSectionMarker.Char('\x00') => i32 0x0000_0000
  CharOrSectionMarker.Char('\u10FFFF') => i32 0x0010_FFFF

  enum CharOrSectionMarkerOrFootnoteMarker { => LLVM i32
    case CharOrSectionMarker(CharOrSectionMarker) => i32 %CharOrSectionMarker
    case Asterisk                                 => i32 0x0020_0002
    case Dagger                                   => i32 0x0020_0003
    case DoubleDagger                             => i32 0x0020_0004
  }

If the data type has no extra inhabitants, or there are not enough extra
inhabitants to represent all of the no-data cases, then a tag bit is added
to the enum's representation. The tag bit is set for the no-data cases, which
are then assigned values in the data area of the enum in declaration order.

::

  enum IntOrInfinity { => LLVM <{ i64, i1 }>
    case NegInfinity    => <{ i64, i1 }> {    0, 1 }
    case Int(Int)       => <{ i64, i1 }> { %Int, 0 }
    case PosInfinity    => <{ i64, i1 }> {    1, 1 }
  }

  IntOrInfinity.Int(    0) => <{ i64, i1 }> {     0, 0 }
  IntOrInfinity.Int(20721) => <{ i64, i1 }> { 20721, 0 }

Multi-Payload Enums
```````````````````

If an enum has more than one case with data type, then a tag is necessary to
discriminate the data types. The ABI will first try to find common
**spare bits**, that is, bits in the data types' binary representations which are
either fixed-zero or ignored by valid values of all of the data types. The tag
will be scattered into these spare bits as much as possible. Currently only
spare bits of primitive integer types, such as the high bits of an ``i21``
type, are considered. The enum data is represented as an integer with the
storage size in bits of the largest data type.

::

  enum TerminalChar {             => LLVM i32
    case Plain(UnicodeScalar)     => i32     (zext i21 %Plain     to i32)
    case Bold(UnicodeScalar)      => i32 (or (zext i21 %Bold      to i32), 0x0020_0000)
    case Underline(UnicodeScalar) => i32 (or (zext i21 %Underline to i32), 0x0040_0000)
    case Blink(UnicodeScalar)     => i32 (or (zext i21 %Blink     to i32), 0x0060_0000)
    case Empty                    => i32 0x0080_0000
    case Cursor                   => i32 0x0080_0001
  }

If there are not enough spare bits to contain the tag, then additional bits are
added to the representation to contain the tag. Tag values are
assigned to data cases in declaration order. If there are no-data cases, they
are collected under a common tag, and assigned values in the data area of the
enum in declaration order.

::

  class Bignum {}

  enum IntDoubleOrBignum { => LLVM <{ i64, i2 }>
    case Int(Int)           => <{ i64, i2 }> {           %Int,            0 }
    case Double(Double)     => <{ i64, i2 }> { (bitcast  %Double to i64), 1 }
    case Bignum(Bignum)     => <{ i64, i2 }> { (ptrtoint %Bignum to i64), 2 }
  }

Existential Container Layout
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Values of protocol type, protocol composition type, or "any" type
(``protocol<>``) are laid out using **existential containers** (so-called
because these types are "existential types" in type theory). 

Opaque Existential Containers
`````````````````````````````

If there is no class constraint on a protocol or protocol composition type,
the existential container has to accommodate a value of arbitrary size and
alignment. It does this using a **fixed-size buffer**, which is three pointers
in size and pointer-aligned. This either directly contains the value, if its
size and alignment are both less than or equal to the fixed-size buffer's, or
contains a pointer to a side allocation owned by the existential container.
The type of the contained value is identified by its `type metadata` record,
and witness tables for all of the required protocol conformances are included.
The layout is as if declared in the following C struct::

  struct OpaqueExistentialContainer {
    void *fixedSizeBuffer[3];
    Metadata *type;
    WitnessTable *witnessTables[NUM_WITNESS_TABLES];
  };

Class Existential Containers
````````````````````````````

If one or more of the protocols in a protocol or protocol composition type
have a class constraint, then only class values can be stored in the existential
container, and a more efficient representation is used. Class instances are
always a single pointer in size, so a fixed-size buffer and potential side
allocation is not needed, and class instances always have a reference to their
own type metadata, so the separate metadata record is not needed. The
layout is thus as if declared in the following C struct::

  struct ClassExistentialContainer {
    HeapObject *value;
    WitnessTable *witnessTables[NUM_WITNESS_TABLES];
  };

Note that if no witness tables are needed, such as for the "any class" type
``protocol<class>`` or an Objective-C protocol type, then the only element of
the layout is the heap object pointer. This is ABI-compatible with ``id``
and ``id <Protocol>`` types in Objective-C.

Type Metadata
-------------

The Swift runtime keeps a **metadata record** for every type used in a program,
including every instantiation of generic types. These metadata records can
be used by (TODO: reflection and) debugger tools to discover information about
types. For non-generic nominal types, these metadata records are generated
statically by the compiler. For instances of generic types, and for intrinsic
types such as tuples, functions, protocol compositions, etc., metadata records
are vended by the runtime as needed. Every type has a unique metadata record;
two **metadata pointer** values are equal iff the types are equivalent.

In the layout descriptions below, offsets are given relative to the
metadata pointer as an index into an array of pointers. On a 32-bit platform,
**offset 1** means an offset of 4 bytes, and on 64-bit platforms, it means
an offset of 8 bytes.

Common Metadata Layout
~~~~~~~~~~~~~~~~~~~~~~

All metadata records share a common header, with the following fields:

- The **value witness table** pointer references a vtable of functions
  that implement the value semantics of the type, providing fundamental
  operations such as allocating, copying, and destroying values of the type.
  The value witness table also records the size, alignment, stride, and other
  fundamental properties of the type. The value witness table pointer is at
  **offset -1** from the metadata pointer, that is, the pointer-sized word
  **immediately before** the pointer's referenced address.

- The **kind** field is a pointer-sized integer that describes the kind of type
  the metadata describes. This field is at **offset 0** from the metadata
  pointer.

  The current kind values are as follows:

  * `Struct metadata`_ has a kind of **1**.
  * `Enum metadata`_ has a kind of **2**.
  * **Opaque metadata** has a kind of **8**. This is used for compiler
    ``Builtin`` primitives that have no additional runtime information.
  * `Tuple metadata`_ has a kind of **9**.
  * `Function metadata`_ has a kind of **10**.
  * `Protocol metadata`_ has a kind of **12**. This is used for
    protocol types, for protocol compositions, and for the "any" type
    ``protocol<>``.
  * `Metatype metadata`_ has a kind of **13**.
  * `Class metadata`_, instead of a kind, has an *isa pointer* in its kind slot,
    pointing to the class's metaclass record. This isa pointer is guaranteed
    to have an integer value larger than **4096** and so can be discriminated
    from non-class kind values.

Struct Metadata
~~~~~~~~~~~~~~~

In addition to the `common metadata layout`_ fields, struct metadata records
contain the following fields:

- The `nominal type descriptor`_ is referenced at **offset 1**.

- A reference to the **parent** metadata record is stored at **offset 2**. For
  structs that are members of an enclosing nominal type, this is a reference
  to the enclosing type's metadata. For top-level structs, this is null.

  TODO: The parent pointer is currently always null.

- A vector of **field offsets** begins at **offset 3**. For each field of the
  struct, in ``var`` declaration order, the field's offset in bytes from the
  beginning of the struct is stored as a pointer-sized integer.

- If the struct is generic, then the
  `generic parameter vector`_ begins at **offset 3+n**, where **n** is the
  number of fields in the struct.

Enum Metadata
~~~~~~~~~~~~~

In addition to the `common metadata layout`_ fields, enum metadata records
contain the following fields:

- The `nominal type descriptor`_ is referenced at **offset 1**.

- A reference to the **parent** metadata record is stored at **offset 2**. For
  enums that are members of an enclosing nominal type, this is a reference to
  the enclosing type's metadata. For top-level enums, this is null.

  TODO: The parent pointer is currently always null.

- If the enum is generic, then the
  `generic parameter vector`_ begins at **offset 3**.

Tuple Metadata
~~~~~~~~~~~~~~

In addition to the `common metadata layout`_ fields, tuple metadata records
contain the following fields:

- The **number of elements** in the tuple is a pointer-sized integer at
  **offset 1**.
- The **labels string** is a pointer to a list of consecutive null-terminated
  label names for the tuple at **offset 2**. Each label name is given as a
  null-terminated, UTF-8-encoded string in sequence. If the tuple has no
  labels, this is a null pointer.

  TODO: The labels string pointer is currently always null, and labels are
  not factored into tuple metadata uniquing.

- The **element vector** begins at **offset 3** and consists of a vector of
  type–offset pairs. The metadata for the *n*\ th element's type is a pointer
  at **offset 3+2*n**. The offset in bytes from the beginning of the tuple to
  the beginning of the *n*\ th element is at **offset 3+2*n+1**.

Function Metadata
~~~~~~~~~~~~~~~~~

In addition to the `common metadata layout`_ fields, function metadata records
contain the following fields:

- The number of arguments to the function is stored at **offset 1**.
- A reference to the **result type** metadata record is stored at
  **offset 2**. If the function has multiple returns, this references a
  `tuple metadata`_ record.
- The **argument vector** begins at **offset 3** and consists of pointers to
  metadata records of the function's arguments.

  If the function takes any **inout** arguments, a pointer to each argument's
  metadata record will be appended separately, the lowest bit being set if it is
  **inout**. Because of pointer alignment, the lowest bit will always be free to
  hold this tag.

  If the function takes no **inout** arguments, there will be only one pointer in
  the vector for the following cases:

  * 0 arguments: a `tuple metadata`_ record for the empty tuple
  * 1 argument: the first and only argument's metadata record
  * >1 argument: a `tuple metadata`_ record containing the arguments

Protocol Metadata
~~~~~~~~~~~~~~~~~

In addition to the `common metadata layout`_ fields, protocol metadata records
contain the following fields:

- A **layout flags** word is stored at **offset 1**. The bits of this word
  describe the `existential container layout`_ used to represent
  values of the type. The word is laid out as follows:

  * The **number of witness tables** is stored in the least significant 31 bits.
    Values of the protocol type contain this number of witness table pointers
    in their layout.
  * The **class constraint** is stored at bit 31. This bit is set if the type
    is **not** class-constrained, meaning that struct, enum, or class values
    can be stored in the type. If not set, then only class values can be stored
    in the type, and the type uses a more efficient layout.

  Note that the field is pointer-sized, even though only the lowest 32 bits are
  currently inhabited on all platforms. These values can be derived from the
  `protocol descriptor`_ records, but are pre-calculated for convenience.

- The **number of protocols** that make up the protocol composition is stored at
  **offset 2**. For the "any" types ``protocol<>`` or ``protocol<class>``, this
  is zero. For a single-protocol type ``P``, this is one. For a protocol
  composition type ``protocol<P, Q, ...>``, this is the number of protocols.

- The **protocol descriptor vector** begins at **offset 3**. This is an inline
  array of pointers to the `protocol descriptor`_ for every protocol in the
  composition, or the single protocol descriptor for a protocol type. For
  an "any" type, there is no protocol descriptor vector.

Metatype Metadata
~~~~~~~~~~~~~~~~~

In addition to the `common metadata layout`_ fields, metatype metadata records
contain the following fields:

- A reference to the metadata record for the **instance type** that the metatype
  represents is stored at **offset 1**.

Class Metadata
~~~~~~~~~~~~~~

Class metadata is designed to interoperate with Objective-C; all class metadata
records are also valid Objective-C ``Class`` objects. Class metadata pointers
are used as the values of class metatypes, so a derived class's metadata
record also serves as a valid class metatype value for all of its ancestor
classes.

- The **destructor pointer** is stored at **offset -2** from the metadata
  pointer, behind the value witness table. This function is invoked by Swift's
  deallocator when the class instance is destroyed.
- The **isa pointer** pointing to the class's Objective-C-compatible metaclass
  record is stored at **offset 0**, in place of an integer kind discriminator.
- The **super pointer** pointing to the metadata record for the superclass is
  stored at **offset 1**. If the class is a root class, it is null.
- Two words are reserved for use by the Objective-C runtime at **offset 2**
  and **offset 3**.
- The **rodata pointer** is stored at **offset 4**; it points to an Objective-C
  compatible rodata record for the class. This pointer value includes a tag.
  The **low bit is always set to 1** for Swift classes and always set to 0 for
  Objective-C classes.
- The **class flags** are a 32-bit field at **offset 5**.
- The **instance address point** is a 32-bit field following the class flags.
  A pointer to an instance of this class points this number of bytes after the
  beginning of the instance.
- The **instance size** is a 32-bit field following the instance address point.
  This is the number of bytes of storage present in every object of this type.
- The **instance alignment mask** is a 16-bit field following the instance size.
  This is a set of low bits which must not be set in a pointer to an instance
  of this class.
- The **runtime-reserved field** is a 16-bit field following the instance
  alignment mask.  The compiler initializes this to zero.
- The **class object size** is a 32-bit field following the runtime-reserved
  field.  This is the total number of bytes of storage in the class metadata
  object.
- The **class object address point** is a 32-bit field following the class
  object size.  This is the number of bytes of storage in the class metadata
  object.
- The `nominal type descriptor`_ for the most-derived class type is referenced
  at an offset immediately following the class object address point. This is
  **offset 8** on a 64-bit platform or **offset 11** on a 32-bit platform.
- For each Swift class in the class's inheritance hierarchy, in order starting
  from the root class and working down to the most derived class, the following
  fields are present:

  * First, a reference to the **parent** metadata record is stored.
    For classes that are members of an enclosing nominal type, this is a
    reference to the enclosing type's metadata. For top-level classes, this is
    null.

    TODO: The parent pointer is currently always null.

  * If the class is generic, its `generic parameter vector`_ is stored inline.
  * The **vtable** is stored inline and contains a function pointer to the
    implementation of every method of the class in declaration order.
  * If the layout of a class instance is dependent on its generic parameters,
    then a **field offset vector** is stored inline, containing offsets in
    bytes from an instance pointer to each field of the class in declaration
    order. (For classes with fixed layout, the field offsets are accessible
    statically from global variables, similar to Objective-C ivar offsets.)

  Note that none of these fields are present for Objective-C base classes in
  the inheritance hierarchy.

Generic Parameter Vector
~~~~~~~~~~~~~~~~~~~~~~~~

Metadata records for instances of generic types contain information about their
generic parameters. For each parameter of the type, a reference to the metadata
record for the type argument is stored.  After all of the type argument
metadata references, for each type parameter, if there are protocol
requirements on that type parameter, a reference to the witness table for each
protocol it is required to conform to is stored in declaration order.

For example, given a generic type with the parameters ``<T, U, V>``, its
generic parameter record will consist of references to the metadata records
for ``T``, ``U``, and ``V`` in succession, as if laid out in a C struct::

  struct GenericParameterVector {
    TypeMetadata *T, *U, *V;
  };

If we add protocol requirements to the parameters, for example,
``<T: Runcible, U: protocol<Fungible, Ansible>, V>``, then the type's generic
parameter vector contains witness tables for those protocols, as if laid out::

  struct GenericParameterVector {
    TypeMetadata *T, *U, *V;
    RuncibleWitnessTable *T_Runcible;
    FungibleWitnessTable *U_Fungible;
    AnsibleWitnessTable *U_Ansible;
  };

Nominal Type Descriptor
~~~~~~~~~~~~~~~~~~~~~~~

The metadata records for class, struct, and enum types contain a pointer to a
**nominal type descriptor**, which contains basic information about the nominal
type such as its name, members, and metadata layout. For a generic type, one
nominal type descriptor is shared for all instantiations of the type. The
layout is as follows:

- The **kind** of type is stored at **offset 0**, which is as follows:

  * **0** for a class,
  * **1** for a struct, or
  * **2** for an enum.

- The mangled **name** is referenced as a null-terminated C string at
  **offset 1**. This name includes no bound generic parameters.
- The following four fields depend on the kind of nominal type.

  * For a struct or class:

    + The **number of fields** is stored at **offset 2**. This is the length
      of the field offset vector in the metadata record, if any.
    + The **offset to the field offset vector** is stored at **offset 3**.
      This is the offset in pointer-sized words of the field offset vector for
      the type in the metadata record. If no field offset vector is stored
      in the metadata record, this is zero.
    + The **field names** are referenced as a doubly-null-terminated list of
      C strings at **offset 4**. The order of names corresponds to the order
      of fields in the field offset vector.
    + The **field type accessor** is a function pointer at **offset 5**. If
      non-null, the function takes a pointer to an instance of type metadata
      for the nominal type, and returns a pointer to an array of type metadata
      references for the types of the fields of that instance. The order matches
      that of the field offset vector and field name list.

  * For an enum:

    + TODO: Offsets 2-5 are always zero.

- If the nominal type is generic, a pointer to the **metadata pattern** that
  is used to form instances of the type is stored at **offset 6**. The pointer
  is null if the type is not generic.

- The **generic parameter descriptor** begins at **offset 7**. This describes
  the layout of the generic parameter vector in the metadata record:

  * The **offset of the generic parameter vector** is stored at **offset 7**.
    This is the offset in pointer-sized words of the generic parameter vector
    inside the metadata record. If the type is not generic, this is zero.
  * The **number of type parameters** is stored at **offset 8**. This count
    includes associated types of type parameters with protocol constraints.
  * The **number of type parameters** is stored at **offset 9**. This count
    includes only the primary formal type parameters.
  * For each type parameter **n**, the following fields are stored:

    + The **number of witnesses** for the type parameter is stored at
      **offset 10+n**. This is the number of witness table pointers that are
      stored for the type parameter in the generic parameter vector.

Note that there is no nominal type descriptor for protocols or protocol types.
See the `protocol descriptor`_ description below.

Protocol Descriptor
~~~~~~~~~~~~~~~~~~~

`Protocol metadata` contains references to zero, one, or more **protocol
descriptors** that describe the protocols values of the type are required to
conform to. The protocol descriptor is laid out to be compatible with
Objective-C ``Protocol`` objects. The layout is as follows:

- An **isa** placeholder is stored at **offset 0**. This field is populated by
  the Objective-C runtime.
- The mangled **name** is referenced as a null-terminated C string at
  **offset 1**.
- If the protocol inherits one or more other protocols, a pointer to the
  **inherited protocols list** is stored at **offset 2**. The list starts with
  the number of inherited protocols as a pointer-sized integer, and is followed
  by that many protocol descriptor pointers. If the protocol inherits no other
  protocols, this pointer is null.
- For an ObjC-compatible protocol, its **required instance methods** are stored
  at **offset 3** as an ObjC-compatible method list. This is null for native
  Swift protocols.
- For an ObjC-compatible protocol, its **required class methods** are stored
  at **offset 4** as an ObjC-compatible method list. This is null for native
  Swift protocols.
- For an ObjC-compatible protocol, its **optional instance methods** are stored
  at **offset 5** as an ObjC-compatible method list. This is null for native
  Swift protocols.
- For an ObjC-compatible protocol, its **optional class methods** are stored
  at **offset 6** as an ObjC-compatible method list. This is null for native
  Swift protocols.
- For an ObjC-compatible protocol, its **instance properties** are stored
  at **offset 7** as an ObjC-compatible property list. This is null for native
  Swift protocols.
- The **size** of the protocol descriptor record is stored as a 32-bit integer
  at **offset 8**. This is currently 72 on 64-bit platforms and 40 on 32-bit
  platforms.
- **Flags** are stored as a 32-bit integer after the size. The following bits
  are currently used (counting from least significant bit zero):

  * **Bit 0** is the **Swift bit**. It is set for all protocols defined in
    Swift and unset for protocols defined in Objective-C.
  * **Bit 1** is the **class constraint bit**. It is set if the protocol is
    **not** class-constrained, meaning that any struct, enum, or class type
    may conform to the protocol. It is unset if only classes can conform to
    the protocol. (The inverted meaning is for compatibility with Objective-C
    protocol records, in which the bit is never set. Objective-C protocols can
    only be conformed to by classes.)
  * **Bit 2** is the **witness table bit**. It is set if dispatch to the
    protocol's methods is done through a witness table, which is either passed
    as an extra parameter to generic functions or included in the `existential
    container layout`_ of protocol types. It is unset if dispatch is done
    through ``objc_msgSend`` and requires no additional information to accompany
    a value of conforming type.
  * **Bit 31** is set by the Objective-C runtime when it has done its
    initialization of the protocol record. It is unused by the Swift runtime.

Heap Objects
------------

Heap Metadata
~~~~~~~~~~~~~

Heap Object Header
~~~~~~~~~~~~~~~~~~

Mangling
--------
::

  mangled-name ::= '_T' global

All Swift-mangled names begin with this prefix.

Globals
~~~~~~~

::

  global ::= 't' type                    // standalone type (for DWARF)
  global ::= 'M' directness type         // type metadata
  global ::= 'MP' directness type        // type metadata pattern
  global ::= 'Ma' type                   // type metadata access function
  global ::= 'ML' type                   // type metadata lazy cache variable
  global ::= 'Mm' type                   // class metaclass
  global ::= 'Mn' nominal-type           // nominal type descriptor
  global ::= 'Mp' protocol               // protocol descriptor
  global ::= 'PA' .*                     // partial application forwarder
  global ::= 'PAo' .*                    // ObjC partial application forwarder
  global ::= 'w' value-witness-kind type // value witness
  global ::= 'WV' type                   // value witness table
  global ::= 'Wo' entity                 // witness table offset
  global ::= 'Wv' directness entity      // field offset
  global ::= 'WP' protocol-conformance   // protocol witness table
  global ::= 'WZ' protocol-conformance   // lazy protocol witness table accessor
  global ::= 'Wz' protocol-conformance   // lazy protocol witness table template
  global ::= 'WD' protocol-conformance   // dependent proto witness table generator
  global ::= 'Wd' protocol-conformance   // dependent proto witness table template
  global ::= entity                      // some identifiable thing
  global ::= 'TO' global                 // ObjC-as-swift thunk
  global ::= 'To' global                 // swift-as-ObjC thunk
  global ::= 'TD' global                 // dynamic dispatch thunk
  global ::= 'Td' global                 // direct method reference thunk
  global ::= 'TR' reabstract-signature   // reabstraction thunk helper function
  global ::= 'Tr' reabstract-signature   // reabstraction thunk

  global ::= 'TS' specializationinfo '_' mangled-name
  specializationinfo ::= 'g' passid (type protocol-conformance* '_')+            // Generic specialization info.
  specializationinfo ::= 'f' passid (funcspecializationarginfo '_')+             // Function signature specialization kind
  passid ::= integer                                                             // The id of the pass that generated this specialization.
  funcsigspecializationarginfo ::= 'cl' closurename type*                        // Closure specialized with closed over types in argument order.
  funcsigspecializationarginfo ::= 'n'                                           // Unmodified argument
  funcsigspecializationarginfo ::= 'cp' funcsigspecializationconstantproppayload // Constant propagated argument
  funcsigspecializationarginfo ::= 'd'                                           // Dead argument
  funcsigspecializationarginfo ::= 'g' 's'?                                      // Owned => Guaranteed and Exploded if 's' present.
  funcsigspecializationarginfo ::= 's'                                           // Exploded
  funcsigspecializationconstantpropinfo ::= 'fr' mangled-name
  funcsigspecializationconstantpropinfo ::= 'g' mangled-name
  funcsigspecializationconstantpropinfo ::= 'i' 64-bit-integer
  funcsigspecializationconstantpropinfo ::= 'fl' float-as-64-bit-integer
  funcsigspecializationconstantpropinfo ::= 'se' stringencoding 'v' md5hash

  global ::= 'TV' global                 // vtable override thunk
  global ::= 'TW' protocol-conformance entity
                                         // protocol witness thunk
  entity ::= nominal-type                // named type declaration
  entity ::= static? entity-kind context entity-name
  entity-kind ::= 'F'                    // function (ctor, accessor, etc.)
  entity-kind ::= 'v'                    // variable (let/var)
  entity-kind ::= 's'                    // subscript itself (not the individual accessors)
  entity-kind ::= 'I'                    // initializer
  entity-name ::= decl-name type         // named declaration
  entity-name ::= 'A' index              // default argument generator
  entity-name ::= 'a' addressor-kind decl-name type     // mutable addressor
  entity-name ::= 'C' type               // allocating constructor
  entity-name ::= 'c' type               // non-allocating constructor
  entity-name ::= 'D'                    // deallocating destructor; untyped
  entity-name ::= 'd'                    // non-deallocating destructor; untyped
  entity-name ::= 'g' decl-name type     // getter
  entity-name ::= 'i'                    // non-local variable initializer
  entity-name ::= 'l' addressor-kind decl-name type     // non-mutable addressor
  entity-name ::= 'm' decl-name type     // materializeForSet
  entity-name ::= 's' decl-name type     // setter
  entity-name ::= 'U' index type         // explicit anonymous closure expression
  entity-name ::= 'u' index type         // implicit anonymous closure
  entity-name ::= 'w' decl-name type     // willSet
  entity-name ::= 'W' decl-name type     // didSet
  static ::= 'Z'                         // entity is a static member of a type
  decl-name ::= identifier
  decl-name ::= local-decl-name
  decl-name ::= private-decl-name
  local-decl-name ::= 'L' index identifier  // locally-discriminated declaration
  private-decl-name ::= 'P' identifier identifier  // file-discriminated declaration
  reabstract-signature ::= ('G' generic-signature)? type type
  addressor-kind ::= 'u'                 // unsafe addressor (no owner)
  addressor-kind ::= 'O'                 // owning addressor (non-native owner)
  addressor-kind ::= 'o'                 // owning addressor (native owner)
  addressor-kind ::= 'p'                 // pinning addressor (native owner)

An ``entity`` starts with a ``nominal-type-kind`` (``[COPV]``), a
substitution (``[S]``) of a nominal type, or an ``entity-kind``
(``[FIsv]``).

An ``entity-name`` starts with ``[AaCcDggis]`` or a ``decl-name``.
A ``decl-name`` starts with ``[LP]`` or an ``identifier`` (``[0-9oX]``).

A ``context`` starts with either an ``entity``, an ``extension`` (which starts
with ``[E]``), or a ``module``, which might be an ``identifier`` (``[0-9oX]``)
or a substitution of a module (``[S]``).

A global mangling starts with an ``entity`` or ``[MTWw]``.

If a partial application forwarder is for a static symbol, its name will
start with the sequence ``_TPA_`` followed by the mangled symbol name of the
forwarder's destination.

A generic specialization mangling consists of a header, specifying the types
and conformances used to specialize the generic function, followed by the
full mangled name of the original unspecialized generic symbol.

The first identifier in a ``<private-decl-name>`` is a string that represents
the file the original declaration came from. It should be considered unique
within the enclosing module. The second identifier is the name of the entity.

Not all declarations marked ``private`` declarations will use the 
``<private-decl-name>`` mangling; if the entity's context is enough to uniquely
identify the entity, the simple ``identifier`` form is preferred.

The types in a ``<reabstract-signature>`` are always non-polymorphic
``<impl-function-type>`` types.

Direct and Indirect Symbols
~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

Declaration Contexts
~~~~~~~~~~~~~~~~~~~~

::

  context ::= module
  context ::= extension
  context ::= entity
  module ::= substitution                    // other substitution
  module ::= identifier                      // module name
  module ::= known-module                    // abbreviation
  extension ::= 'E' module entity

These manglings identify the enclosing context in which an entity was declared,
such as its enclosing module, function, or nominal type.

An ``extension`` mangling is used whenever an entity's declaration context is
an extension *and* the entity being extended is in a different module. In this
case the extension's module is mangled first, followed by the entity being
extended. If the extension and the extended entity are in the same module, the
plain ``entity`` mangling is preferred.

When mangling the context of a local entity within a constructor or
destructor, the non-allocating or non-deallocating variant is used.

Types
~~~~~

::

  type ::= 'Bb'                              // Builtin.BridgeObject
  type ::= 'BB'                              // Builtin.UnsafeValueBuffer
  type ::= 'Bf' natural '_'                  // Builtin.Float<n>
  type ::= 'Bi' natural '_'                  // Builtin.Int<n>
  type ::= 'BO'                              // Builtin.ObjCPointer
  type ::= 'Bo'                              // Builtin.ObjectPointer
  type ::= 'Bp'                              // Builtin.RawPointer
  type ::= 'Bv' natural type                 // Builtin.Vec<n>x<type>
  type ::= 'Bw'                              // Builtin.Word
  type ::= nominal-type
  type ::= associated-type
  type ::= 'a' context identifier            // Type alias (DWARF only)
  type ::= 'b' type type                     // objc block function type
  type ::= 'c' type type                     // C function pointer type
  type ::= 'F' throws-annotation? type type  // function type
  type ::= 'f' throws-annotation? type type  // uncurried function type  
  type ::= 'G' type <type>+ '_'              // generic type application
  type ::= 'K' type type                     // @auto_closure function type
  type ::= 'M' type                          // metatype without representation
  type ::= 'XM' metatype-repr type           // metatype with representation
  type ::= 'P' protocol-list '_'             // protocol type
  type ::= 'PM' type                         // existential metatype without representation
  type ::= 'XPM' metatype-repr type          // existential metatype with representation
  type ::= archetype
  type ::= 'R' type                          // inout
  type ::= 'T' tuple-element* '_'            // tuple
  type ::= 't' tuple-element* '_'            // variadic tuple
  type ::= 'U' generics '_' type             // generic type (old)
  type ::= 'Xo' type                         // @unowned type
  type ::= 'Xu' type                         // @unowned(unsafe) type
  type ::= 'Xw' type                         // @weak type
  type ::= 'XF' impl-function-type           // function implementation type
  type ::= 'Xf' type type                    // @thin function type
  nominal-type ::= known-nominal-type
  nominal-type ::= substitution
  nominal-type ::= nominal-type-kind declaration-name
  nominal-type-kind ::= 'C'                  // class
  nominal-type-kind ::= 'O'                  // enum
  nominal-type-kind ::= 'V'                  // struct
  archetype ::= 'Q' index                    // archetype with depth=0, idx=N
  archetype ::= 'Qd' index index             // archetype with depth=M+1, idx=N
  archetype ::= associated-type
  archetype ::= qualified-archetype
  associated-type ::= substitution
  associated-type ::= 'Q' protocol-context     // self type of protocol
  associated-type ::= 'Q' archetype identifier // associated type
  qualified-archetype ::= 'Qq' index context   // archetype+context (DWARF only)
  protocol-context ::= 'P' protocol
  tuple-element ::= identifier? type
  metatype-repr ::= 't'                      // Thin metatype representation
  metatype-repr ::= 'T'                      // Thick metatype representation
  metatype-repr ::= 'o'                      // ObjC metatype representation
  throws-annotation ::= 'z'                  // 'throws' annotation on function types


  type ::= 'u' generic-signature type        // generic type (new)
  type ::= 'q' index                         // dependent generic parameter
                                             //   with depth = 0, idx = N
  type ::= 'qd' index index                  // dependent generic parameter
                                             //   with depth = M+1, idx = N
  type ::= 'q' type protocol identifier      // associated type

``<type>`` never begins or ends with a number.
``<type>`` never begins with an underscore.
``<type>`` never begins with ``d``.
``<type>`` never begins with ``z``.

Note that protocols mangle differently as types and as contexts. A protocol
context always consists of a single protocol name and so mangles without a
trailing underscore. A protocol type can have zero, one, or many protocol bounds
which are juxtaposed and terminated with a trailing underscore.

::

  impl-function-type ::=
    impl-callee-convention impl-function-attribute* generic-signature? '_'
    impl-parameter* '_' impl-result* '_'
  impl-callee-convention ::= 't'              // thin
  impl-callee-convention ::= impl-convention  // thick, callee transfered with given convention
  impl-convention ::= 'a'                     // direct, autoreleased
  impl-convention ::= 'd'                     // direct, no ownership transfer
  impl-convention ::= 'D'                     // direct, no ownership transfer,
                                              // dependent on 'self' parameter
  impl-convention ::= 'g'                     // direct, guaranteed
  impl-convention ::= 'e'                     // direct, deallocating
  impl-convention ::= 'i'                     // indirect, ownership transfer
  impl-convention ::= 'l'                     // indirect, inout
  impl-convention ::= 'G'                     // indirect, guaranteed
  impl-convention ::= 'o'                     // direct, ownership transfer
  impl-function-attribute ::= 'Cb'            // compatible with C block invocation function
  impl-function-attribute ::= 'Cc'            // compatible with C global function
  impl-function-attribute ::= 'Cm'            // compatible with Swift method
  impl-function-attribute ::= 'CO'            // compatible with ObjC method
  impl-function-attribute ::= 'Cw'            // compatible with protocol witness
  impl-function-attribute ::= 'N'             // noreturn
  impl-function-attribute ::= 'G'             // generic
  impl-parameter ::= impl-convention type
  impl-result ::= impl-convention type

For the most part, manglings follow the structure of formal language
types.  However, in some cases it is more useful to encode the exact
implementation details of a function type.

Any ``<impl-function-attribute>`` productions must appear in the order
in which they are specified above: e.g. a noreturn C function is
mangled with ``CcN``.

Note that the convention and function-attribute productions do not
need to be disambiguated from the start of a ``<type>``.

Generics
~~~~~~~~

::

  generics ::= generic-parameter+ ('U' generic-parameter+)?
  generic-parameter ::= protocol-list '_'
  protocol-list ::= protocol*
  protocol ::= substitution
  protocol ::= declaration-name

``<protocol-list>`` is unambiguous because protocols are always top-level,
so the structure is quite simple. If there are associated types in the signature,
they are mangled following a second ``U``.

``<protocol>`` must not start with ``M``.

::

  protocol-conformance ::= ('U' generics '_')? type protocol module
  protocol-conformance ::= ('u' generic-signature)? type protocol module

``<protocol-conformance>`` refers to a type's conformance to a protocol. The named
module is the one containing the extension or type declaration that declared
the conformance.

::

  generic-signature ::= (generic-param-count+)? 'R' requirement* '_'
  generic-signature ::= (generic-param-count+)? 'r' // no requirements
  generic-param-count ::= 'z'       // zero parameters
  generic-param-count ::= index     // N+1 parameters
  requirement ::= type protocol     // protocol requirement
  requirement ::= 'd' type type     // 'd'erived class requirement
  requirement ::= 'z' type type     // 'z'ame-type requirement

A generic signature begins by describing the number of generic parameters at
each depth of the signature, followed by the requirements. As a special case,
no ``generic-param-count`` values indicates a single generic parameter at
the outermost depth::

  urFq_q_                           // <T_0_0> T_0_0 -> T_0_0
  u_0_rFq_qd_0_                     // <T_0_0><T_1_0, T_1_1> T_0_0 -> T_1_1

Value Witnesses
~~~~~~~~~~~~~~~

::

  value-witness-kind ::= 'al'           // allocateBuffer
  value-witness-kind ::= 'ca'           // assignWithCopy
  value-witness-kind ::= 'ta'           // assignWithTake
  value-witness-kind ::= 'de'           // deallocateBuffer
  value-witness-kind ::= 'xx'           // destroy
  value-witness-kind ::= 'XX'           // destroyBuffer
  value-witness-kind ::= 'Xx'           // destroyArray
  value-witness-kind ::= 'CP'           // initializeBufferWithCopyOfBuffer
  value-witness-kind ::= 'Cp'           // initializeBufferWithCopy
  value-witness-kind ::= 'cp'           // initializeWithCopy
  value-witness-kind ::= 'TK'           // initializeBufferWithTakeOfBuffer
  value-witness-kind ::= 'Tk'           // initializeBufferWithTake
  value-witness-kind ::= 'tk'           // initializeWithTake
  value-witness-kind ::= 'pr'           // projectBuffer
  value-witness-kind ::= 'xs'           // storeExtraInhabitant
  value-witness-kind ::= 'xg'           // getExtraInhabitantIndex
  value-witness-kind ::= 'ug'           // getEnumTag
  value-witness-kind ::= 'up'           // inplaceProjectEnumData
  value-witness-kind ::= 'Cc'           // initializeArrayWithCopy
  value-witness-kind ::= 'Tt'           // initializeArrayWithTakeFrontToBack
  value-witness-kind ::= 'tT'           // initializeArrayWithTakeBackToFront

``<value-witness-kind>`` differentiates the kinds of value
witness functions for a type.

Identifiers
~~~~~~~~~~~

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
  operator-char ::= 'q'                      // ? 'question'
  operator-char ::= 'r'                      // % 'remainder'
  operator-char ::= 's'                      // - 'subtract'
  operator-char ::= 't'                      // ~ 'tilde'
  operator-char ::= 'x'                      // ^ 'xor'
  operator-char ::= 'z'                      // . 'zperiod'

``<identifier>`` is run-length encoded: the natural indicates how many
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

Substitutions
~~~~~~~~~~~~~

::

  substitution ::= 'S' index

``<substitution>`` is a back-reference to a previously mangled entity. The mangling
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

Predefined Substitutions
~~~~~~~~~~~~~~~~~~~~~~~~

::

  known-module ::= 'SC'                      // C
  known-module ::= 'So'                      // Objective-C
  known-module ::= 'Ss'                      // Swift
  known-nominal-type ::= 'Sa'                // Swift.Array
  known-nominal-type ::= 'Sb'                // Swift.Bool
  known-nominal-type ::= 'Sc'                // Swift.UnicodeScalar
  known-nominal-type ::= 'Sd'                // Swift.Float64
  known-nominal-type ::= 'Sf'                // Swift.Float32
  known-nominal-type ::= 'Si'                // Swift.Int
  known-nominal-type ::= 'SQ'                // Swift.ImplicitlyUnwrappedOptional
  known-nominal-type ::= 'Sq'                // Swift.Optional
  known-nominal-type ::= 'SS'                // Swift.String
  known-nominal-type ::= 'Su'                // Swift.UInt

``<known-module>`` and ``<known-nominal-type>`` are built-in substitutions for
certain common entities.  Like any other substitution, they all start
with 'S'.

The Objective-C module is used as the context for mangling Objective-C
classes as ``<type>``\ s.

Indexes
~~~~~~~

::

  index ::= '_'                              // 0
  index ::= natural '_'                      // N+1
  natural ::= [0-9]+

``<index>`` is a production for encoding numbers in contexts that can't
end in a digit; it's optimized for encoding smaller numbers.
