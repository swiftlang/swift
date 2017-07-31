:orphan:

.. @raise litre.TestsAreMissing
.. _ABI:

.. highlight:: none

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

