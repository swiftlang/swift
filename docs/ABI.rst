:orphan:

.. @raise litre.TestsAreMissing
.. _ABI:

.. highlight:: none

The Swift ABI
=============

.. contents::

Hard Constraints on Resilience
------------------------------

The root of a class hierarchy must remain stable, at pain of
invalidating the metaclass hierarchy.  Note that a Swift class without an
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

Values of protocol type, protocol composition type, or ``Any`` type are laid
out using **existential containers** (so-called because these types are
"existential types" in type theory).

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
are lazily created by the runtime as required. Every type has a unique metadata
record; two **metadata pointer** values are equal iff the types are equivalent.

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
    protocol types, for protocol compositions, and for the ``Any`` type.
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
  type-offset pairs. The metadata for the *n*\ th element's type is a pointer
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
  **offset 2**. For the "any" types ``Any`` or ``Any : class``, this
  is zero. For a single-protocol type ``P``, this is one. For a protocol
  composition type ``P & Q & ...``, this is the number of protocols.

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
``<T: Runcible, U: Fungible & Ansible, V>``, then the type's generic
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

    + The **number of payload cases** and **payload size offset** are stored
      at **offset 2**. The least significant 24 bits are the number of payload
      cases, and the most significant 8 bits are the offset of the payload
      size in the type metadata, if present.
    + The **number of no-payload cases** is stored at **offset 3**.
    + The **case names** are referenced as a doubly-null-terminated list of
      C strings at **offset 4**. The names are ordered such that payload cases
      come first, followed by no-payload cases. Within each half of the list,
      the order of names corresponds to the order of cases in the enum
      declaration.
    + The **case type accessor** is a function pointer at **offset 5**. If
      non-null, the function takes a pointer to an instance of type metadata
      for the enum, and returns a pointer to an array of type metadata
      references for the types of the cases of that instance. The order matches
      that of the case name list. This function is similar to the field type
      accessor for a struct, except also the least significant bit of each
      element in the result is set if the enum case is an **indirect case**.

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

  mangled-name ::= '_S' global

All Swift-mangled names begin with this prefix.

The basic mangling scheme is a list of 'operators' where the operators are
structured in a post-fix order. For example the mangling may start with an
identifier but only later in the mangling a type-like operator defines how this
identifier has to be interpreted::

  4Test3FooC   // The trailing 'C' says that 'Foo' is a class in module 'Test'



Operators are either identifiers or a sequence of one or more characters,
like ``C`` for class.
All operators share the same name-space. Important operators are a single
character, which means that no other operator may start with the same
character.

Some less important operators are longer and may also contain one or more
natural numbers. But it's always important that the demangler can identify the
end (the last character) of an operator. For example, it's not possible to
determine the last character if there are two operators ``M`` and ``Ma``:
``a`` could belong to ``M`` or it could be the first character of the next
operator.

The intention of the post-fix order is to optimize for common pre-fixes.
Regardless, if it's the mangling for a metatype or a function in a module, the
mangled name will start with the module name (after the ``_S``).

In the following, productions which are only _part_ of an operator, are
named with uppercase letters.

Globals
~~~~~~~

::

  global ::= type 'N'                    // type metadata (address point)
                                         // -- type starts with [BCOSTV]
  global ::= type 'Mf'                   // 'full' type metadata (start of object)
  global ::= type 'MP'                   // type metadata pattern
  global ::= type 'Ma'                   // type metadata access function
  global ::= type 'ML'                   // type metadata lazy cache variable
  global ::= nominal-type 'Mm'           // class metaclass
  global ::= nominal-type 'Mn'           // nominal type descriptor
  global ::= protocol 'Mp'               // protocol descriptor
  global ::= type 'MF'                   // metadata for remote mirrors: field descriptor
  global ::= type 'MB'                   // metadata for remote mirrors: builtin type descriptor
  global ::= protocol-conformance 'MA'   // metadata for remote mirrors: associated type descriptor
  global ::= nominal-type 'MC'           // metadata for remote mirrors: superclass descriptor

  // TODO check this::
  global ::= mangled-name 'TA'                     // partial application forwarder
  global ::= mangled-name 'Ta'                     // ObjC partial application forwarder

  global ::= type 'w' VALUE-WITNESS-KIND // value witness
  global ::= protocol-conformance 'Wa'   // protocol witness table accessor
  global ::= protocol-conformance 'WG'   // generic protocol witness table
  global ::= protocol-conformance 'WI'   // generic protocol witness table instantiation function
  global ::= type protocol-conformance 'WL'   // lazy protocol witness table cache variable
  global ::= entity 'Wo'                 // witness table offset
  global ::= protocol-conformance 'WP'   // protocol witness table

  global ::= protocol-conformance identifier 'Wt' // associated type metadata accessor
  global ::= protocol-conformance identifier nominal-type 'WT' // associated type witness table accessor
  global ::= type protocol-conformance 'Wl' // lazy protocol witness table accessor
  global ::= type 'WV'                   // value witness table
  global ::= entity 'Wv' DIRECTNESS      // field offset

  global ::= type 'Wy' // Outlined Copy Function Type
  global ::= type 'We' // Outlined Consume Function Type

  DIRECTNESS ::= 'd'                         // direct
  DIRECTNESS ::= 'i'                         // indirect

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

  global ::= global 'TO'                 // ObjC-as-swift thunk
  global ::= global 'To'                 // swift-as-ObjC thunk
  global ::= global 'TD'                 // dynamic dispatch thunk
  global ::= global 'Td'                 // direct method reference thunk
  global ::= global 'TV'                 // vtable override thunk
  global ::= type 'D'                    // type mangling for the debugger. TODO: check if we really need this
  global ::= protocol-conformance entity 'TW' // protocol witness thunk
  global ::= context identifier identifier 'TB' // property behavior initializer thunk (not used currently)
  global ::= context identifier identifier 'Tb' // property behavior setter thunk (not used currently)
  global ::= global 'T_' specialization  // reset substitutions before demangling specialization
  global ::= entity                      // some identifiable thing
  global ::= type type generic-signature? 'T' REABSTRACT-THUNK-TYPE   // reabstraction thunk helper function

  REABSTRACT-THUNK-TYPE ::= 'R'          // reabstraction thunk helper function
  REABSTRACT-THUNK-TYPE ::= 'r'          // reabstraction thunk

The types in a reabstraction thunk helper function are always non-polymorphic
``<impl-function-type>`` types.

::

  VALUE-WITNESS-KIND ::= 'al'           // allocateBuffer
  VALUE-WITNESS-KIND ::= 'ca'           // assignWithCopy
  VALUE-WITNESS-KIND ::= 'ta'           // assignWithTake
  VALUE-WITNESS-KIND ::= 'de'           // deallocateBuffer
  VALUE-WITNESS-KIND ::= 'xx'           // destroy
  VALUE-WITNESS-KIND ::= 'XX'           // destroyBuffer
  VALUE-WITNESS-KIND ::= 'Xx'           // destroyArray
  VALUE-WITNESS-KIND ::= 'CP'           // initializeBufferWithCopyOfBuffer
  VALUE-WITNESS-KIND ::= 'Cp'           // initializeBufferWithCopy
  VALUE-WITNESS-KIND ::= 'cp'           // initializeWithCopy
  VALUE-WITNESS-KIND ::= 'TK'           // initializeBufferWithTakeOfBuffer
  VALUE-WITNESS-KIND ::= 'Tk'           // initializeBufferWithTake
  VALUE-WITNESS-KIND ::= 'tk'           // initializeWithTake
  VALUE-WITNESS-KIND ::= 'pr'           // projectBuffer
  VALUE-WITNESS-KIND ::= 'xs'           // storeExtraInhabitant
  VALUE-WITNESS-KIND ::= 'xg'           // getExtraInhabitantIndex
  VALUE-WITNESS-KIND ::= 'Cc'           // initializeArrayWithCopy
  VALUE-WITNESS-KIND ::= 'Tt'           // initializeArrayWithTakeFrontToBack
  VALUE-WITNESS-KIND ::= 'tT'           // initializeArrayWithTakeBackToFront
  VALUE-WITNESS-KIND ::= 'ug'           // getEnumTag
  VALUE-WITNESS-KIND ::= 'up'           // destructiveProjectEnumData
  VALUE-WITNESS-KIND ::= 'ui'           // destructiveInjectEnumTag

``<VALUE-WITNESS-KIND>`` differentiates the kinds of value
witness functions for a type.

Entities
~~~~~~~~

::

  entity ::= nominal-type                    // named type declaration
  entity ::= context entity-spec static? curry-thunk?

  static ::= 'Z'
  curry-thunk ::= 'Tc'

  // The leading type is the function type
  entity-spec ::= type 'fC'                  // allocating constructor
  entity-spec ::= type 'fc'                  // non-allocating constructor
  entity-spec ::= type 'fU' INDEX            // explicit anonymous closure expression
  entity-spec ::= type 'fu' INDEX            // implicit anonymous closure
  entity-spec ::= 'fA' INDEX                 // default argument N+1 generator
  entity-spec ::= 'fi'                       // non-local variable initializer
  entity-spec ::= 'fD'                       // deallocating destructor; untyped
  entity-spec ::= 'fd'                       // non-deallocating destructor; untyped
  entity-spec ::= 'fE'                       // ivar destroyer; untyped
  entity-spec ::= 'fe'                       // ivar initializer; untyped

  entity-spec ::= decl-name function-signature generic-signature? 'F'    // function
  entity-spec ::= decl-name type 'i'                 // subscript ('i'ndex) itself (not the individual accessors)
  entity-spec ::= decl-name type 'v'                 // variable
  entity-spec ::= decl-name type 'f' ACCESSOR
  entity-spec ::= decl-name type 'fp'                // generic type parameter (not used?)
  entity-spec ::= decl-name type 'fo'                // enum element (currently not used)

  ACCESSOR ::= 'm'                           // materializeForSet
  ACCESSOR ::= 's'                           // setter
  ACCESSOR ::= 'g'                           // getter
  ACCESSOR ::= 'G'                           // global getter
  ACCESSOR ::= 'w'                           // willSet
  ACCESSOR ::= 'W'                           // didSet
  ACCESSOR ::= 'a' ADDRESSOR-KIND            // mutable addressor
  ACCESSOR ::= 'l' ADDRESSOR-KIND            // non-mutable addressor
                                         
  ADDRESSOR-KIND ::= 'u'                     // unsafe addressor (no owner)
  ADDRESSOR-KIND ::= 'O'                     // owning addressor (non-native owner)
  ADDRESSOR-KIND ::= 'o'                     // owning addressor (native owner)
  ADDRESSOR-KIND ::= 'p'                     // pinning addressor (native owner)

  decl-name ::= identifier
  decl-name ::= identifier 'L' INDEX         // locally-discriminated declaration
  decl-name ::= identifier identifier 'LL'    // file-discriminated declaration

The first identifier in a file-discriminated ``<decl-name>>`` is a string that
represents the file the original declaration came from.
It should be considered unique within the enclosing module.
The second identifier is the name of the entity.
Not all declarations marked ``private`` declarations will use this mangling;
if the entity's context is enough to uniquely identify the entity, the simple
``identifier`` form is preferred.

Declaration Contexts
~~~~~~~~~~~~~~~~~~~~

These manglings identify the enclosing context in which an entity was declared,
such as its enclosing module, function, or nominal type.

::

  context ::= module
  context ::= entity
  context ::= entity module generic-signature? 'E'

An ``extension`` mangling is used whenever an entity's declaration context is
an extension *and* the entity being extended is in a different module. In this
case the extension's module is mangled first, followed by the entity being
extended. If the extension and the extended entity are in the same module, the
plain ``entity`` mangling is preferred. If the extension is constrained, the
constraints on the extension are mangled in its generic signature.

When mangling the context of a local entity within a constructor or
destructor, the non-allocating or non-deallocating variant is used.

::

  module ::= identifier                      // module name
  module ::= known-module                    // abbreviation

  known-module ::= 's'                       // Swift
  known-module ::= 'SC'                      // C
  known-module ::= 'So'                      // Objective-C

The Objective-C module is used as the context for mangling Objective-C
classes as ``<type>``\ s.


Types
~~~~~

::

  nominal-type ::= substitution
  nominal-type ::= context decl-name 'C'     // nominal class type
  nominal-type ::= context decl-name 'O'     // nominal enum type
  nominal-type ::= context decl-name 'V'     // nominal struct type
  nominal-type ::= protocol 'P'              // nominal protocol type

  nominal-type ::= 'S' KNOWN-TYPE-KIND       // known nominal type substitution
  nominal-type ::= 'S' NATURAL KNOWN-TYPE-KIND    // repeated known type substitutions of the same kind

  KNOWN-TYPE-KIND ::= 'a'                    // Swift.Array
  KNOWN-TYPE-KIND ::= 'b'                    // Swift.Bool
  KNOWN-TYPE-KIND ::= 'c'                    // Swift.UnicodeScalar
  KNOWN-TYPE-KIND ::= 'd'                    // Swift.Float64
  KNOWN-TYPE-KIND ::= 'f'                    // Swift.Float32
  KNOWN-TYPE-KIND ::= 'i'                    // Swift.Int
  KNOWN-TYPE-KIND ::= 'V'                    // Swift.UnsafeRawPointer
  KNOWN-TYPE-KIND ::= 'v'                    // Swift.UnsafeMutableRawPointer
  KNOWN-TYPE-KIND ::= 'P'                    // Swift.UnsafePointer
  KNOWN-TYPE-KIND ::= 'p'                    // Swift.UnsafeMutablePointer
  KNOWN-TYPE-KIND ::= 'Q'                    // Swift.ImplicitlyUnwrappedOptional
  KNOWN-TYPE-KIND ::= 'q'                    // Swift.Optional
  KNOWN-TYPE-KIND ::= 'R'                    // Swift.UnsafeBufferPointer
  KNOWN-TYPE-KIND ::= 'r'                    // Swift.UnsafeMutableBufferPointer
  KNOWN-TYPE-KIND ::= 'S'                    // Swift.String
  KNOWN-TYPE-KIND ::= 'u'                    // Swift.UInt

  protocol ::= context decl-name

  type ::= 'Bb'                              // Builtin.BridgeObject
  type ::= 'BB'                              // Builtin.UnsafeValueBuffer
  type ::= 'Bf' NATURAL '_'                  // Builtin.Float<n>
  type ::= 'Bi' NATURAL '_'                  // Builtin.Int<n>
  type ::= 'BO'                              // Builtin.UnknownObject
  type ::= 'Bo'                              // Builtin.NativeObject
  type ::= 'Bp'                              // Builtin.RawPointer
  type ::= type 'Bv' NATURAL '_'             // Builtin.Vec<n>x<type>
  type ::= 'Bw'                              // Builtin.Word
  type ::= context decl-name 'a'             // Type alias (DWARF only)
  type ::= function-signature 'c'            // function type
  type ::= function-signature 'X' FUNCTION-KIND // special function type
  type ::= bound-generic-type
  type ::= type 'Sg'                         // optional type, shortcut for: type 'ySqG'
  type ::= type 'Xo'                         // @unowned type
  type ::= type 'Xu'                         // @unowned(unsafe) type
  type ::= type 'Xw'                         // @weak type
  type ::= impl-function-type 'XF'           // function implementation type (currently unused)
  type ::= type 'Xb'                         // SIL @box type (deprecated)
  type ::= type-list 'Xx'                    // SIL box type
  type ::= type-list type-list generic-signature 'XX'
                                             // Generic SIL box type
  type ::= type 'XD'                         // dynamic self type
  type ::= type 'm'                          // metatype without representation
  type ::= type 'XM' METATYPE-REPR           // metatype with representation
  type ::= type 'Xp'                         // existential metatype without representation
  type ::= type 'Xm' METATYPE-REPR           // existential metatype with representation
  type ::= 'Xe'                              // error or unresolved type
 
  bound-generic-type ::= type 'y' (type* '_')* type* 'G'   // one type-list per nesting level of type
  bound-generic-type ::= substitution

  FUNCTION-KIND ::= 'f'                      // @thin function type
  FUNCTION-KIND ::= 'U'                      // uncurried function type (currently not used) 
  FUNCTION-KIND ::= 'K'                      // @auto_closure function type
  FUNCTION-KIND ::= 'B'                      // objc block function type
  FUNCTION-KIND ::= 'C'                      // C function pointer type

  function-signature ::= params-type params-type throws? // results and parameters

  params-type := type                        // tuple in case of multiple parameters
  params-type := empty-list                  // shortcut for no parameters

  throws ::= 'K'                             // 'throws' annotation on function types

  type-list ::= list-type '_' list-type*     // list of types
  type-list ::= empty-list

  list-type ::= type identifier? 'z'? 'd'?   // type with optional label, inout convention and variadic specifier

  METATYPE-REPR ::= 't'                      // Thin metatype representation
  METATYPE-REPR ::= 'T'                      // Thick metatype representation
  METATYPE-REPR ::= 'o'                      // ObjC metatype representation

  type ::= archetype
  type ::= associated-type
  type ::= nominal-type
  type ::= protocol-list 'p'                 // existential type
  type ::= type-list 't'                     // tuple
  type ::= type generic-signature 'u'        // generic type
  type ::= 'x'                               // generic param, depth=0, idx=0
  type ::= 'q' GENERIC-PARAM-INDEX           // dependent generic parameter
  type ::= type assoc-type-name 'qa'         // associated type of non-generic param
  type ::= assoc-type-name 'Qy' GENERIC-PARAM-INDEX  // associated type
  type ::= assoc-type-name 'Qz'                      // shortcut for 'Qyz'
  type ::= assoc-type-list 'QY' GENERIC-PARAM-INDEX  // associated type at depth
  type ::= assoc-type-list 'QZ'                      // shortcut for 'QYz'

  protocol-list ::= protocol '_' protocol*
  protocol-list ::= empty-list

  assoc-type-list ::= assoc-type-name '_' assoc-type-name*

  archetype ::= context 'Qq' INDEX           // archetype+context (DWARF only)
  archetype ::= associated-type

  associated-type ::= substitution
  associated-type ::= protocol 'QP'          // self type of protocol
  associated-type ::= archetype identifier 'Qa' // associated type
  
  assoc-type-name ::= identifier                // associated type name without protocol
  assoc-type-name ::= identifier protocol 'P'   //

  empty-list ::= 'y'

Associated types use an abbreviated mangling when the base generic parameter
or associated type is constrained by a single protocol requirement. The
associated type in this case can be referenced unambiguously by name alone.
If the base has multiple conformance constraints, then the protocol name is
mangled in to disambiguate.

::

  impl-function-type ::= type* 'I' FUNC-ATTRIBUTES '_'
  impl-function-type ::= type* generic-signature 'I' PSEUDO-GENERIC? FUNC-ATTRIBUTES '_'

  FUNC-ATTRIBUTES ::= CALLEE-CONVENTION? FUNC-REPRESENTATION PARAM-CONVENTION* RESULT-CONVENTION* ('z' RESULT-CONVENTION)

  PSEUDO-GENERIC ::= 'P'

  CALLEE-CONVENTION ::= 'y'                  // @callee_unowned
  CALLEE-CONVENTION ::= 'g'                  // @callee_guaranteed
  CALLEE-CONVENTION ::= 'x'                  // @callee_owned
  CALLEE-CONVENTION ::= 't'                  // thin

  FUNC-REPRESENTATION ::= 'B'                // C block invocation function
  FUNC-REPRESENTATION ::= 'C'                // C global function
  FUNC-REPRESENTATION ::= 'M'                // Swift method
  FUNC-REPRESENTATION ::= 'J'                // ObjC method
  FUNC-REPRESENTATION ::= 'K'                // closure
  FUNC-REPRESENTATION ::= 'W'                // protocol witness

  PARAM-CONVENTION ::= 'i'                   // indirect in
  PARAM-CONVENTION ::= 'l'                   // indirect inout
  PARAM-CONVENTION ::= 'b'                   // indirect inout aliasable
  PARAM-CONVENTION ::= 'n'                   // indirect in guaranteed
  PARAM-CONVENTION ::= 'x'                   // direct owned
  PARAM-CONVENTION ::= 'y'                   // direct unowned
  PARAM-CONVENTION ::= 'g'                   // direct guaranteed
  PARAM-CONVENTION ::= 'e'                   // direct deallocating

  RESULT-CONVENTION ::= 'r'                  // indirect
  RESULT-CONVENTION ::= 'o'                  // owned
  RESULT-CONVENTION ::= 'd'                  // unowned
  RESULT-CONVENTION ::= 'u'                  // unowned inner pointer
  RESULT-CONVENTION ::= 'a'                  // auto-released

For the most part, manglings follow the structure of formal language
types.  However, in some cases it is more useful to encode the exact
implementation details of a function type.

The ``type*`` list contains parameter and return types (including the error
result), in that order.
The number of parameters and results must match with the number of
``<PARAM-CONVENTION>`` and ``<RESULT-CONVENTION>`` characters after the
``<FUNC-REPRESENTATION>``.
The ``<generic-signature>`` is used if the function is polymorphic.

Generics
~~~~~~~~

::

  protocol-conformance ::= type protocol module generic-signature?

``<protocol-conformance>`` refers to a type's conformance to a protocol. The
named module is the one containing the extension or type declaration that
declared the conformance.

::

  protocol-conformance ::= context identifier protocol identifier generic-signature?  // Property behavior conformance

Property behaviors are implemented using private protocol conformances.

::

  generic-signature ::= requirement* 'l'     // one generic parameter
  generic-signature ::= requirement* 'r' GENERIC-PARAM-COUNT* 'l'

  GENERIC-PARAM-COUNT ::= 'z'                // zero parameters
  GENERIC-PARAM-COUNT ::= INDEX              // N+1 parameters

  requirement ::= protocol 'R' GENERIC-PARAM-INDEX                  // protocol requirement
  requirement ::= protocol assoc-type-name 'Rp' GENERIC-PARAM-INDEX // protocol requirement on associated type
  requirement ::= protocol assoc-type-list 'RP' GENERIC-PARAM-INDEX // protocol requirement on associated type at depth
  requirement ::= protocol substitution 'RQ'                        // protocol requirement with substitution
  requirement ::= type 'Rb' GENERIC-PARAM-INDEX                     // base class requirement
  requirement ::= type assoc-type-name 'Rc' GENERIC-PARAM-INDEX     // base class requirement on associated type
  requirement ::= type assoc-type-list 'RC' GENERIC-PARAM-INDEX     // base class requirement on associated type at depth
  requirement ::= type substitution 'RB'                            // base class requirement with substitution
  requirement ::= type 'Rs' GENERIC-PARAM-INDEX                     // same-type requirement
  requirement ::= type assoc-type-name 'Rt' GENERIC-PARAM-INDEX     // same-type requirement on associated type
  requirement ::= type assoc-type-list 'RT' GENERIC-PARAM-INDEX     // same-type requirement on associated type at depth
  requirement ::= type substitution 'RS'                            // same-type requirement with substitution
  requirement ::= type 'Rl' GENERIC-PARAM-INDEX LAYOUT-CONSTRAINT   // layout requirement
  requirement ::= type assoc-type-name 'Rm' GENERIC-PARAM-INDEX LAYOUT-CONSTRAINT    // layout requirement on associated type
  requirement ::= type assoc-type-list 'RM' GENERIC-PARAM-INDEX LAYOUT-CONSTRAINT    // layout requirement on associated type at depth
  requirement ::= type substitution 'RM' LAYOUT-CONSTRAINT                           // layout requirement with substitution

  GENERIC-PARAM-INDEX ::= 'z'                // depth = 0,   idx = 0
  GENERIC-PARAM-INDEX ::= INDEX              // depth = 0,   idx = N+1
  GENERIC-PARAM-INDEX ::= 'd' INDEX INDEX    // depth = M+1, idx = N

  LAYOUT-CONSTRAINT ::= 'N'  // NativeRefCountedObject 
  LAYOUT-CONSTRAINT ::= 'R'  // RefCountedObject 
  LAYOUT-CONSTRAINT ::= 'T'  // Trivial 
  LAYOUT-CONSTRAINT ::= 'C'  // Class
  LAYOUT-CONSTRAINT ::= 'D'  // NativeClass 
  LAYOUT-CONSTRAINT ::= 'E' LAYOUT-SIZE-AND-ALIGNMENT  // Trivial of exact size 
  LAYOUT-CONSTRAINT ::= 'e' LAYOUT-SIZE  // Trivial of exact size 
  LAYOUT-CONSTRAINT ::= 'M' LAYOUT-SIZE-AND-ALIGNMENT  // Trivial of size at most N bits 
  LAYOUT-CONSTRAINT ::= 'm' LAYOUT-SIZE  // Trivial of size at most N bits 
  LAYOUT-CONSTRAINT ::= 'U'  // Unknown layout

  LAYOUT-SIZE ::= INDEX // Size only
  LAYOUT-SIZE-AND-ALIGNMENT ::= INDEX INDEX // Size followed by alignment



A generic signature begins with an optional list of requirements.
The ``<GENERIC-PARAM-COUNT>`` describes the number of generic parameters at
each depth of the signature. As a special case, no ``<GENERIC-PARAM-COUNT>``
values indicates a single generic parameter at the outermost depth::

  x_xCru                           // <T_0_0> T_0_0 -> T_0_0
  d_0__xCr_0_u                     // <T_0_0><T_1_0, T_1_1> T_0_0 -> T_1_1

A generic signature must only precede an operator character which is different
from any character in a ``<GENERIC-PARAM-COUNT>``.

Identifiers
~~~~~~~~~~~

::

  identifier ::= substitution
  identifier ::= NATURAL IDENTIFIER-STRING   // identifier without word substitutions
  identifier ::= '0' IDENTIFIER-PART         // identifier with word substitutions

  IDENTIFIER-PART ::= NATURAL IDENTIFIER-STRING
  IDENTIFIER-PART ::= [a-z]                  // word substitution (except the last one)
  IDENTIFIER-PART ::= [A-Z]                  // last word substitution in identifier

  IDENTIFIER-STRING ::= IDENTIFIER-START-CHAR IDENTIFIER-CHAR*
  IDENTIFIER-START-CHAR ::= [_a-zA-Z]
  IDENTIFIER-CHAR ::= [_$a-zA-Z0-9]

``<identifier>`` is run-length encoded: the natural indicates how many
characters follow. Operator characters are mapped to letter characters as
given. In neither case can an identifier start with a digit, so
there's no ambiguity with the run-length.

If the run-length start with a ``0`` the identifier string contains
word substitutions. A word is a sub-string of an identifier which contains
letters and digits ``[A-Za-z0-9]``. Words are separated by underscores
``_``. In addition a new word begins with an uppercase letter ``[A-Z]``
if the previous character is not an uppercase letter::

  Abc1DefG2HI          // contains four words 'Abc1', 'Def' and 'G2' and 'HI'
  _abc1_def_G2hi       // contains three words 'abc1', 'def' and G2hi

The words of all identifiers, which are encoded in the current mangling are
enumerated and assigned to a letter: a = first word, b = second word, etc.

An identifier containing word substitutions is a sequence of run-length encoded
sub-strings and references to previously mangled words.
All but the last word-references are lowercase letters and the last one is an
uppercase letter. If there is no literal sub-string after the last
word-reference, the last word-reference is followed by a ``0``.

Let's assume the current mangling already encoded the identifier ``AbcDefGHI``::

  02Myac1_B    // expands to: MyAbcGHI_Def

A maximum of 26 words in a mangling can be used for substitutions.

::

  identifier ::= '00' natural '_'? IDENTIFIER-CHAR+  // '_' is inserted if the identifier starts with a digit or '_'.

Identifiers that contain non-ASCII characters are encoded using the Punycode
algorithm specified in RFC 3492, with the modifications that ``_`` is used
as the encoding delimiter, and uppercase letters A through J are used in place
of digits 0 through 9 in the encoding character set. The mangling then
consists of an ``00`` followed by the run length of the encoded string and the
encoded string itself. For example, the identifier ``vergüenza`` is mangled
to ``0012vergenza_JFa``. (The encoding in standard Punycode would be
``vergenza-95a``)

If the encoded string starts with a digit or an ``_``, an additional ``_`` is
inserted between the run length and the encoded string.

::

  identifier ::= identifier 'o' OPERATOR-FIXITY

  OPERATOR-FIXITY ::= 'p'                    // prefix operator
  OPERATOR-FIXITY ::= 'P'                    // postfix operator
  OPERATOR-FIXITY ::= 'i'                    // infix operator

  OPERATOR-CHAR ::= 'a'                      // & 'and'
  OPERATOR-CHAR ::= 'c'                      // @ 'commercial at'
  OPERATOR-CHAR ::= 'd'                      // / 'divide'
  OPERATOR-CHAR ::= 'e'                      // = 'equals'
  OPERATOR-CHAR ::= 'g'                      // > 'greater'
  OPERATOR-CHAR ::= 'l'                      // < 'less'
  OPERATOR-CHAR ::= 'm'                      // * 'multiply'
  OPERATOR-CHAR ::= 'n'                      // ! 'not'
  OPERATOR-CHAR ::= 'o'                      // | 'or'
  OPERATOR-CHAR ::= 'p'                      // + 'plus'
  OPERATOR-CHAR ::= 'q'                      // ? 'question'
  OPERATOR-CHAR ::= 'r'                      // % 'remainder'
  OPERATOR-CHAR ::= 's'                      // - 'subtract'
  OPERATOR-CHAR ::= 't'                      // ~ 'tilde'
  OPERATOR-CHAR ::= 'x'                      // ^ 'xor'
  OPERATOR-CHAR ::= 'z'                      // . 'zperiod'

If an identifier is followed by an ``o`` its text is interpreted as an
operator. Each lowercase character maps to an operator character
(``OPERATOR-CHAR``).

Operators that contain non-ASCII characters are mangled by first mapping the
ASCII operator characters to letters as for pure ASCII operator names, then
Punycode-encoding the substituted string.
For example, the infix operator ``«+»`` is mangled to
``007p_qcaDcoi`` (``p_qcaDc`` being the encoding of the substituted
string ``«p»``).

Substitutions
~~~~~~~~~~~~~

::

  substitution ::= 'A' INDEX                  // substitution of N+26
  substitution ::= 'A' SUBST_IDX* LAST-SUBST-IDX    // One or more consecutive substitutions of N < 26
  SUBST-IDX ::= [a-z]
  SUBST-IDX ::= NATURAL [a-z]
  LAST-SUBST-IDX ::= [A-Z]
  LAST-SUBST-IDX ::= NATURAL [A-Z]


``<substitution>`` is a back-reference to a previously mangled entity. The mangling
algorithm maintains a mapping of entities to substitution indices as it runs.
When an entity that can be represented by a substitution (a module, nominal
type, or protocol) is mangled, a substitution is first looked for in the
substitution map, and if it is present, the entity is mangled using the
associated substitution index. Otherwise, the entity is mangled normally, and
it is then added to the substitution map and associated with the next
available substitution index.

For example, in mangling a function type
``(zim.zang.zung, zim.zang.zung, zim.zippity) -> zim.zang.zoo`` (with module
``zim`` and class ``zim.zang``),
the recurring contexts ``zim``, ``zim.zang``, and ``zim.zang.zung``
will be mangled using substitutions after being mangled
for the first time. The first argument type will mangle in long form,
``3zim4zang4zung``, and in doing so, ``zim`` will acquire substitution ``AA``,
``zim.zang`` will acquire substitution ``AB``, and ``zim.zang.zung`` will
acquire ``AC``. The second argument is the same as the first and will mangle
using its substitution, ``AC``. The
third argument type will mangle using the substitution for ``zim``,
``AA7zippity``. (It also acquires substitution ``AD`` which would be used
if it mangled again.) The result type will mangle using the substitution for
``zim.zang``, ``AB3zoo`` (and acquire substitution ``AE``).

There are some pre-defined substitutions, see ``KNOWN-TYPE-KIND``.

If the mangling contains two or more consecutive substitutions, it can be
abbreviated with the ``A`` substitution. Similar to word-substitutions the
index is encoded as letters, whereas the last letter is uppercase::

  AaeB      // equivalent to A_A4_A0_

Repeated substitutions are encoded with a natural prefix number::

  A3a2B     // equivalent to AaaabB

Numbers and Indexes
~~~~~~~~~~~~~~~~~~~

::

  INDEX ::= '_'                               // 0
  INDEX ::= NATURAL '_'                       // N+1
  NATURAL ::= [1-9] [0-9]*
  NATURAL_ZERO ::= [0-9]+

``<INDEX>`` is a production for encoding numbers in contexts that can't
end in a digit; it's optimized for encoding smaller numbers.

Function Specializations
~~~~~~~~~~~~~~~~~~~~~~~~

::

  specialization ::= type '_' type* 'Tg' SPEC-INFO     // Generic re-abstracted specialization
  specialization ::= type '_' type* 'TG' SPEC-INFO     // Generic not re-abstracted specialization

The types are the replacement types of the substitution list.

::

  specialization ::= type 'Tp' SPEC-INFO // Partial generic specialization
  specialization ::= type 'TP' SPEC-INFO // Partial generic specialization, not re-abstracted

The type is the function type of the specialized function.

::

  specialization ::= spec-arg* 'Tf' SPEC-INFO UNIQUE-ID? ARG-SPEC-KIND* '_' ARG-SPEC-KIND  // Function signature specialization kind

The ``<ARG-SPEC-KIND>`` describes how arguments are specialized.
Some kinds need arguments, which precede ``Tf``.

::

  spec-arg ::= identifier
  spec-arg ::= type

  SPEC-INFO ::= FRAGILE? PASSID

  PASSID ::= '0'                             // AllocBoxToStack,
  PASSID ::= '1'                             // ClosureSpecializer,
  PASSID ::= '2'                             // CapturePromotion,
  PASSID ::= '3'                             // CapturePropagation,
  PASSID ::= '4'                             // FunctionSignatureOpts,
  PASSID ::= '5'                             // GenericSpecializer,

  FRAGILE ::= 'q'

  UNIQUE-ID ::= NATURAL                      // Used to make unique function names

  ARG-SPEC-KIND ::= 'n'                      // Unmodified argument
  ARG-SPEC-KIND ::= 'c'                      // Consumes n 'type' arguments which are closed over types in argument order
                                             // and one 'identifier' argument which is the closure symbol name
  ARG-SPEC-KIND ::= 'p' CONST-PROP           // Constant propagated argument
  ARG-SPEC-KIND ::= 'd' 'G'? 'X'?            // Dead argument, with optional owned=>guaranteed or exploded-specifier
  ARG-SPEC-KIND ::= 'g' 'X'?                 // Owned => Guaranteed,, with optional exploded-specifier
  ARG-SPEC-KIND ::= 'x'                      // Exploded
  ARG-SPEC-KIND ::= 'i'                      // Box to value
  ARG-SPEC-KIND ::= 's'                      // Box to stack

  CONST-PROP ::= 'f'                         // Consumes one identifier argument which is a function symbol name
  CONST-PROP ::= 'g'                         // Consumes one identifier argument which is a global symbol name
  CONST-PROP ::= 'i' NATURAL_ZERO            // 64-bit-integer
  CONST-PROP ::= 'd' NATURAL_ZERO            // float-as-64-bit-integer
  CONST-PROP ::= 's' ENCODING                // string literal. Consumes one identifier argument.

  ENCODING ::= 'b'                           // utf8
  ENCODING ::= 'w'                           // utf16
  ENCODING ::= 'c'                           // utf16

If the first character of the string literal is a digit ``[0-9]`` or an
underscore ``_``, the identifier for the string literal is prefixed with an
additional underscore ``_``.
