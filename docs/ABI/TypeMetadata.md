# Type Metadata

The Swift runtime keeps a **metadata record** for every type used in a program,
including every instantiation of generic types. These metadata records can
be used by (TODO: reflection and) debugger tools to discover information about
types. For non-generic nominal types, these metadata records are generated
statically by the compiler. For instances of generic types, and for intrinsic
types such as tuples, functions, protocol compositions, etc., metadata records
are lazily created by the runtime as required. Every type has a unique metadata
record; two **metadata pointer** values are equal iff the types are equivalent.

For the older version of Swift's metadata layout:
[Old Type Metadata](OldTypeMetadata.rst)

- [Common Metadata Layout](#common-metadata-layout)
  - [Struct Metadata](#struct-metadata)
  - [Enum Metadata](#enum-metadata)
  - [Optional Metadata](#optional-metadata)
  - [Tuple Metadata](#tuple-metadata)
  - [Function Metadata](#function-metadata)
  - [Existential Metadata](#existential-metadata)
  - [Metatype Metadata](#metatype-metadata)
  - [Existential Metatype Metadata](#existential-metatype-metadata)
  - [Class Metadata](#class-metadata)
  - [Objective-C Class Wrapper Metadata](#objective-c-class-wrapper-metadata)
  - [Foreign Class Metadata](#foreign-class-metadata)
  - [Opaque Metadata](#opaque-metadata)
- [Relative Pointers](#relative-pointers)
- [Context Descriptor](#context-descriptor)
  - [Module Descriptor](#module-descriptor)
  - [Extension Descriptor](#extension-descriptor)
  - [Anonymous Descriptor](#anonymous-descriptor)
  - [Protocol Descriptor](#protocol-descriptor)
  - [Opaque Descriptor](#opaque-descriptor)
  - [Type Context Descriptor](#type-context-descriptor)
  - [Class Descriptor](#class-descriptor)
  - [Struct Descriptor](#struct-descriptor)
  - [Enum Descriptor](#enum-descriptor)
- [Protocol Conformance Descriptor](#protocol-conformance-descriptor)
- [Generic Argument Vector](#generic-argument-vector)
- [Generic Requirement Descriptor](#generic-requirement-descriptor)
- [Recursive Type Dependencies](#recursive-type-dependencies)
- [Metadata States](#metadata-states)
- [Transitive Completeness Guarentees](#transitive-completeness-guarantees)
- [Completeness Requirements](#completeness-requirements)
- [Metadata Requests and Responses](#metadata-requests-and-responses)
- [Metadata Allocation and Initialization](#metadata-allocation-and-initialization)

In the layout descriptions below, offsets are given relative to the
metadata pointer as an index into an array of pointers. On a 32-bit platform,
**offset 1** means an offset of 4 bytes, and on 64-bit platforms, it means
an offset of 8 bytes. For layouts that require a different sized offset, it will
be marked with the appropriate byte count. For example: **offset 1 (4 byte)**

## Common Metadata Layout

All metadata records share a common header, with the following fields:

#### Value Witness Table (offset -1)

The **value witness table** pointer references a vtable of functions
that implement the value semantics of the type, providing fundamental
operations such as allocating, copying, and destroying values of the type.
The value witness table also records the size, alignment, stride, and other
fundamental properties of the type. The value witness table pointer is at
**offset -1** from the metadata pointer, that is, the pointer-sized word
**immediately before** the pointer's referenced address.

#### Kind (offset 0)

The **kind** field is a pointer-sized integer that describes the kind of type
the metadata describes.

|                               Metadata                               | Kind |
| -------------------------------------------------------------------- | ---- |
| [Class](#Class-Metadata)*                                            |    0 |
| [Struct](#Struct-Metadata)                                           |  512 |
| [Enum](#Enum-Metadata)                                               |  513 |
| [Optional](#Optional-Metadata)                                       |  514 |
| [Foreign Class](#Foreign-Class-Metadata)                             |  515 |
| [Opaque](#Opaque-Metadata)                                           |  768 |
| [Tuple](#Tuple-Metadata)                                             |  769 |
| [Function](#Function-Metadata)                                       |  770 |
| [Existential](#Existential-Metadata)                                 |  771 |
| [Metatype](#Metatype-Metadata)                                       |  772 |
| [ObjC Class Wrapper](#ObjC-Class-Wrapper-Metadata)                   |  773 |
| [Existential Metatype](#Existential-Metatype-Metadata)               |  774 |
| [Heap Local Variable](#Heap-Local-Variable-Metadata)                 | 1024 |
| [Heap Generic Local Variable](#Heap-Generic-Local-Variable-Metadata) | 1280 |
| [Error Object](#Error-Object-Metadata)                               | 1281 |

\*: Class metadata has of kind of **0** unless the class is required to
  interoperate with Objective-C.  If the class is required to interoperate
  with Objective-C, the kind field is instead an *isa pointer* to an
  Objective-C metaclass.  Such a pointer can be distinguished from an
  enumerated metadata kind because it is guaranteed to have a value larger
  than **2047**.  Note that this is a more basic sense of interoperation
  than is meant by the ``@objc`` attribute: it is what is required to
  support Objective-C message sends and retain/release.  All classes are
  required to interoperate with Objective-C on this level when building
  for an Apple platform.

### Struct Metadata

In addition to the [common metadata layout](#Common-Metadata-Layout) fields,
struct metadata records contain the following fields:

#### Context Descriptor (offset 1)

Similar to the value witness table pointer, this is a pointer sized word that
points directly to the [context descriptor](#Context-Descriptor).

#### Generic Argument Vector (offset 2)

If the struct is generic, then the
[generic argument vector](#Generic-Argument-Vector) is found directly after the
nominal type descriptor.

#### Field Offsets (offset ?)

If the struct is generic, then the field offset vector begins immediately after
the generic argument vector. However, if the struct is not generic, then the
field offset vector begins immediately after the nominal type descriptor. The
offset for this field is marked as **offset ?** because each struct is different,
but the nominal type descriptor contains a field that describes the exact offset
(in pointer sized words) from the metadata pointer. For each field of the struct,
in `var` declaration order, the field's offset in bytes from the beginning of the
struct is stored as a `uint32_t` integer.

### Enum Metadata

In addition to the [common metadata layout](#Common-Metadata-Layout) fields,
enum metadata records contain the following fields:

#### Context Descriptor (offset 1)

Similar to the value witness table pointer, this is a pointer sized word that
points directly to the [context descriptor](#Context-Descriptor).

#### Generic Argument Vector (offset 2)

If the enum is generic, then the
[generic argument vector](#Generic-Argument-Vector) is found directly after the
nominal type descriptor.

### Optional Metadata

Optional metadata share the same layout as enum metadata. They are distinguished
from enum metadata because of the importance of the `Optional` type for various
reflection and dynamic-casting purposes.

### Tuple Metadata

In addition to the [common metadata layout](#Common-Metadata-Layout) fields,
tuple metadata records contain the following fields:

#### Number of Elements (offset 1)

The value found at this offset is a pointer-sized integer indicating the number
of tuple elements. e.g. `(Int, String)` has 2 elements.

#### Labels (offset 2)

This is a pointer to the labels for the tuple elements. Labels are encoded in
UTF-8 and separated by spaces, and the entire string is terminated with a null
character. For example, the labels in the tuple type `(x: Int, Int, z: Int)`
would be encoded as the character array `"x z \0"`. A label (possibly
zero-length) is provided for each element of the tuple, meaning that the label
string for a tuple of **n** elements always contains exactly **n** spaces. If
the tuple has no labels at all, the label string is a null pointer.

#### Elements Vector (offset 3)

Following the labels pointer is an array of type-offset pairs. The metadata for
the **n**th element's type is a pointer at **offset 3+2*n**. The offset in bytes
from the beginning of the tuple to the beginning of the **n**th element is at
**offset 3+2*n+1**.

### Function Metadata

In addition to the [common metadata layout](#Common-Metadata-Layout) fields,
function metadata records contain the following fields:

#### Function Flags (offset 1)

This includes information such as the semantic convention of the function,
whether the function throws, and how many parameters the function has.

#### Result Type Metadata (offset 2)

If the function has multiple returns, this references a
[tuple metadata](#Tuple-Metadata) record.

#### Parameter Type Metadata Vector (offset 3)

This is an array which consists of **n** references to parameter type metadata
records (where **n** is the number of parameters found in the function flags).

#### Parameter Flags Vector (offset 4)

**Note**: This vector only appears if the corresponding parameter flag has been
set in the function flags.

This is an array of `uint32_t` integers that includes information such as
whether the parameter is `inout` or whether it is variadic.

#### Specialized ABI Endpoints

Currently we have specialized ABI endpoints to retrieve metadata for functions
with 0/1/2/3 parameters - `swift_getFunctionTypeMetadata{0|1|2|3}` and the
general one `swift_getFunctionTypeMetadata` which handles all other function
types and functions with parameter flags e.g. `(inout Int) -> Void`. Based on
the usage information collected from Swift Standard Library and Overlays as well
as Source Compatibility Suite it was decided not to have specialized ABI
endpoints for functions with parameter flags due their minimal use.

### Existential Metadata

(also known as Protocol Metadata)

In addition to the [common metadata layout](#Common-Metadata-Layout) fields,
existential metadata records contain the following fields:

#### Layout Flags (offset 1)

These flags are laid out as a `uint32_t` integer which are used to describe
the existential container layout used to represent values of the type. The bits
are laid out as follows:

* The **number of witness tables** is stored in the least significant 24 bits.
  Values of the protocol type contain this number of witness table pointers
  in their layout.

* The **special protocol kind** is stored in 6 bits starting at
  bit 24. Only one special protocol kind is defined: the `Error` protocol has
  value 1.

* The **superclass constraint indicator** is stored at bit 30. When set, the
  protocol type includes a superclass constraint (described below).

* The **class constraint** is stored at bit 31. This bit is set if the type
  is **not** class-constrained, meaning that struct, enum, or class values
  can be stored in the type. If not set, then only class values can be stored
  in the type, and the type uses a more efficient layout.

#### Number of Protocols (offset 3 (4 byte))

The value here is stored as a `uint32_t` integer which indicates the
number of protocols that make up the protocol composition. For the "any" types
`Any` or `AnyObject`, this is zero. For a single-protocol type `P`, this is one.
For a protocol composition type ``P & Q & ...``, this is the number of protocols.

#### Superclass Type Metadata (offset 2)

**Note**: This field only appears if the corresponding flag has been set in the
layout flags.

A pointer to the superclass's type metadata.

#### Protocol Vector (offset ?)

The offset for this field is marked **offset ?** because depending on whether or
not a superclass constraint is present, this offset could be **offset 2** or
**offset 3**. This is an inline array of pointers to descriptors of each
protocol in the composition. Each pointer references either a Swift
[protocol descriptor](#Protocol-Descriptor) or an Objective-C `Protocol`; the
low bit will be set to indicate when it references an Objective-C protocol. For
an `Any` or `AnyObject` type, there is no protocol descriptor vector.

### Metatype Metadata

In addition to the [common metadata layout](#Common-Metadata-Layout) fields,
metatype metadata records contain the following fields:

#### Instance Type Metadata (offset 1)

The type metadata that this metatype represents. E.g. this metatype is
`Int.Type`, the instance type metadata points to `Int`'s metadata.

### Existential Metatype Metadata

In addition to the [common metadata layout](#Common-Metadata-Layout) fields,
existential metatype metadata records contain the following fields:

#### Instance Type Metadata (offset 1)

The type metadata that this metatype represents. E.g. this metatype is
`Equatable.Protocol`, the instance type metadata points to `Equatable`'s
metadata. Note that this will always be either an existential type or another
existential metatype.

#### Layout Flags (offset 2)

These are the same flags found on
[existential's layout flags](#Layout-Flags-offset-1).

### Class Metadata

Class metadata is designed to interoperate with Objective-C; all class metadata
records are also valid Objective-C `Class` objects. Class metadata pointers
are used as the values of class metatypes, so a derived class's metadata
record also serves as a valid class metatype value for all of its ancestor
classes.

In addition to the [common metadata layout](#Common-Metadata-Layout) fields,
class metadata records contain the following fields:

#### Destructor (offset -2)

This function is invoked by Swift's deallocator when the class instance is
destroyed.

#### Isa Pointer (offset 0)

If the value at offset 0 is not 0 (class metadata kind), then the value
represents a pointer to the class's Objective-C compatible metaclass.

#### Superclass Type Metadata (offset 1)

Pointer to the metadata record for the superclass. If the class is a root class,
it is null.

#### Objective-C Reserved (offset 2 & 3)

Two words are reserved for use by the Objective-C runtime.

#### Rodata Pointer (offset 4)

Pointer to the Objective-C compatible rodata record for the class.The **low bit
is always set to 1** for Swift classes and always set to 0 for
Objective-C classes.

#### Flags (offset 5)

These flags are laid out as a `uint32_t` integer to describe this class. Each
flag is bit-wised or'ed to each other.

|           Flag          | Value |
| ----------------------  | ----- |
| Is Swift Pre Stable ABI |   0x1 |
| Uses Swift Ref Counting |   0x2 |
| Has Custom ObjC Name    |   0x4 |

#### Instance Address Pointer (offset 11 (4 byte))

`uint32_t` integer that represents the address point of instances of this type.

#### Instance Size (offset 6)

The required size of instances of this type laid out as a `uint32_t` integer.

#### Instance Alignment Mask (offset 13 (4 byte))

This is a set of low bits in a `uint16_t` integer which must not be set in a
pointer to an instance of this class.

#### Runtime Reserved (offset 27 (2 byte))

This field is a `uint16_t` integer reserved for runtime usage.

#### Object Size (offset 7)

Total number of bytes of the class object as represented as a `uint32_t` integer.

#### Class Address Pointer (offset 15 (4 byte))

The offset of the address point within the class object in a `uint32_t` integer.

#### Context Descriptor (offset 8)

Similar to the value witness table pointer, this is a pointer sized word that
points directly to the [context descriptor](#Context-Descriptor) for
the most-derived class type.

#### Ivar Destroyer (offset 9)

Pointer to a function which destroys instance variables. Used to clean up after
an early return from a constructor. If null, no clean up will be performed and
all ivars must be trivial.

- For each Swift class in the class's inheritance hierarchy, in order starting
  from the root class and working down to the most derived class, the following
  fields are present:

  * First, a reference to the **parent** metadata record is stored.
    For classes that are members of an enclosing nominal type, this is a
    reference to the enclosing type's metadata. For top-level classes, this is
    null.

    TODO: The parent pointer is currently always null.

  * If the class is generic, its `generic argument vector`_ is stored inline.
  * The **vtable** is stored inline and contains a function pointer to the
    implementation of every method of the class in declaration order.
  * If the layout of a class instance is dependent on its generic parameters,
    then a **field offset vector** is stored inline, containing offsets in
    bytes from an instance pointer to each field of the class in declaration
    order. (For classes with fixed layout, the field offsets are accessible
    statically from global variables, similar to Objective-C ivar offsets.)

  Note that none of these fields are present for Objective-C base classes in
  the inheritance hierarchy.

### Objective-C Class Wrapper Metadata

Objective-C class wrapper metadata are used when an Objective-C `Class`
object is not a valid Swift type metadata.

In addition to the [common metadata layout](#Common-Metadata-Layout) fields,
Objective-C class wrapper metadata records contain the following fields:

#### Class Metadata (offset 1)

Metadata for a class type that is known not to be a Swift type.

### Foreign Class Metadata

Foreign class metadata describes "foreign" class types, which support Swift
reference counting but are otherwise opaque to the Swift runtime.

In addition to the [common metadata layout](#Common-Metadata-Layout) fields,
foreign class metadata records contain the following fields:

#### Context Descriptor (offset 1)

The context descriptor for the most-derived class type.

#### Superclass Metadata (offset 2)

Metadata for this foreign class's superclass is found at this offset. If the
class is a root class, this is null.

#### Reserved (offset 3)

A pointer-sized integer is reserved here for future use by the runtime.

### Opaque Metadata

Opaque metadata is used by the compiler `Builtin` primitives that have no
additional runtime information, thus there are no added fields beside the ones
described in the [common metadata layout](#Common-Metadata-Layout).

## Relative Pointers

Before we discuss context descriptors, it's important to understand what a
relative pointer is and how Swift metadata uses it. A relative pointer is simply
a `int32_t` integer that indicates the offset of the desired value from the
current position in the metadata. There are 3 different kinds of relative
pointers: Direct, Indirect, and Indirectable.

*If the found offset is 0, then the pointer value is considered null.*

#### Relative Direct Pointer

A relative direct pointer directly references an entity in the same image as the
metadata. For example, if the value for this pointer is 16, then 16 bytes from
this pointer's address is the value of whatever the relative pointer references.

#### Relative Indirect Pointer

A relative indirect pointer indirectly references an entity in another image from
the metadata. That means the referenced value is a pointer to the desired value.
For example, if the value for this pointer is 16, then 16 bytes from
this pointer's address is another pointer value that points to the desired value.

#### Relative Indirectable Pointer

Relative Indirectable Pointers are a little different because they encompass both
relative direct pointers and relative indirect pointers. The low bit of this
value is used as a discriminator to determine whether or not this pointer is
direct or indirect. For example, if the value for this pointer is 17, the low bit
is set which means we must access this as if it were an indirect pointer with
an offset of 16.

## Context Descriptor

Context Descriptors are a type of metadata that describe a declaration context.
For instance, a struct's context descriptor would have information about its
name, field layout, etc.

#### Flags (offset 0)

The first 32 bits of any context descriptor are the flags. This is a `uint32_t`
field which is laid out as follows:

|      Info      |        Mask      |
| -------------- | ---------------- |
| Kind           |             0x1F |
| Is Generic?    |             0x80 |
| Is Unique?     |             0x40 |
| Version        |    (>> 8) & 0xFF |
| Specific Flags | (>> 16) & 0xFFFF |

To distinguish context descriptors, below is a table of different kinds and their
kind value.

| Context Descriptor | Kind |
| ------------------ | ---- |
| Module             |    0 |
| Extension          |    1 |
| Anonymous          |    2 |
| Protocol           |    3 |
| Opaque Type        |    4 |
| Class              |   16 |
| Struct             |   17 |
| Enum               |   18 |

#### Parent Context Descriptor (offset 1 (4 byte))

Each context descriptor has a parent context descriptor field which points to
another context descriptor describing the parent (if it has one). This field is
a relative indirectable pointer to the parent descriptor.

### Module Descriptor

In addition to the context descriptor's fields, module descriptors contain the
following field:

#### Name (offset 2 (4 byte))

This is a relative direct pointer to a null-terminated C string containing the
module name.

### Extension Descriptor

In addition to the context descriptor's fields, extension descriptors contain
the following field:

#### Extended Type (offset 2 (4 byte))

A relative direct pointer to the mangled type context which this extension
extends. Note: This mangled name may contain symbolic references.

### Anonymous Descriptor

An anonymous descriptor describes a context which is anonymous, such as a
function body. Note: anonymous descriptors make use of the kind specific flags
in the context descriptor flags to indicate whether or not it contains a
mangled name.

In addition to the context descriptor's fields, anonymous descriptors contain
the following field:

#### Mangled Name (offset 2 (4 byte))

If the anonymous descriptor's flags first bit is set, then this contains a
mangled name following the parent descriptor. This is a relative direct pointer
if set.

### Protocol Descriptor

Note: protocol descriptors make use of the kind specific flags in the context
descriptor flags to indicate whether it has a class constraint, is resilient,
or also describes the special protocol kind.

|          Info         | Mask |
| --------------------- | ---- |
| Has Class Constraint  |  0x1 |
| Is Resilient          | 0x10 |
| Special Protocol Kind | 0xFC |

Currently, there are only 2 special protocol kinds:

| Special Protocol | Kind |
| ---------------- | ---- |
| None             |    0 |
| Swift.Error      |    1 |

In addition to the context descriptor's fields, protocol descriptors contain
the following fields:

#### Name (offset 2 (4 byte))

This is a relative direct pointer to a null-terminated C string containing the
protocol name.

#### Number of Requirements in Signature (offset 3 (4 byte))

The number of generic requirements in the requirement signature of the protocol.

#### Number of Requirements (offset 4 (4 byte))

The number of requirements in the protocol.

#### Associated Type Names (offset 5 (4 byte))

This is a relative direct pointer to a null-terminated C string containing the
associated type names, as a space-separated list in the same order as the
requirements.

#### Generic Requirement Vector (offset 6 (4 byte))

A vector containing generic requirement descriptors indicated by the number of
requirements in signature.

### Opaque Type Descriptor

An opaque type descriptor describes an opaque typealias. It uses the context
descriptor flag's kind specific flags to indicate the number of underlying
type arguments.

In addition to the context descriptor's fields, opaque type descriptors contain
the following field:

#### Underlying Type Argument Names (offset 2 (4 byte))

This is a vector of relative direct pointers that reference the mangled type
name indicated by the number of underlying type arguments.

### Type Context Descriptor

Type Context Descriptors are an extension of Context Descriptors because they
describe a declaration context which is also a type.

Note: Type Context Descriptors make use of the context descriptor flag's
specific kind flags for the following layout:

|                  Info                 |  Mask  |
| ------------------------------------- | ------ |
| Metadata Initialization               |    0x3 |
| Has Import Info?                      |    0x4 |
| Class Resilient Superclass Ref        |  0xE00 |
| Class Are Immediate Members Negative? | 0x1000 |
| Class Has Resilient Superclass?       | 0x2000 |
| Class Has Override Table?             | 0x4000 |
| Class Has VTable?                     | 0x8000 |

Metadata Initialization has the following kinds:

| Metadata Initialization | Kind |
| ----------------------- | ---- |
| None                    |    0 |
| Singleton               |    1 |
| Foreign                 |    2 |

Class Resilient Superclass Reference has the following type reference kinds:

|   Type Reference    | Kind |
| ------------------- | ---- |
| Direct Descriptor   |    0 |
| Indirect Descriptor |    1 |
| Direct ObjC Name    |    2 |
| Indirect ObjC Name  |    3 |

In addition to the context descriptor fields, type context descriptors contain
the following fields:

#### Name (offset 2 (4 byte))

The name is a relative direct pointer which references a null-terminated C
string. This name includes no bound generic parameters. Note: this name is not
mangled.

#### Metadata Accessor Function (offset 3 (4 byte))

A relative direct pointer referencing the metadata type accessor function for
this type.

#### Reflection Field Descriptor (offset 4 (4 byte))

A relative direct pointer referencing the field descriptor of this type's fields.
This contains information such as property names, types, etc.

### Struct Descriptor

In addition to the type context descriptor's fields, struct descriptors contain
the following fields:

#### Number of Fields (offset 5 (4 byte))

This `uint32_t` integer indicates the number of fields (properties) this struct
has.

#### Field Offset Vector Offset (offset 6 (4 byte))

This is the offset value to the field offset vector in the struct's metadata.
E.g. if this value is 16, then there are 16 bytes from the metadata's pointer
to the field offset vector.

### Enum Descriptor

In addition to the type context descriptor's fields, enum descriptors contain
the following fields:

#### Number of Payload Cases and Payload Size Offset (offset 5 (4 byte))

This is a `uint32_t` integer that stores the number of payload cases in the low
24 bits; in the high 8 bits is the offset of the payload size from the metadata
pointer, if any.

#### Number of Empty Cases (offset 6 (4 byte))

Another `uint32_t` integer value which indicates the number of cases without
payloads.

#### Class Descriptor

In addition to the type context descriptor's fields, class descriptors contain
the following fields:

#### Superclass Type (offset 5 (4 byte))

Relative direct pointer referencing the mangled type name of the superclass type.

#### Metadata Negative Size in Words/Resilient Metadata Bounds (offset 6 (4 byte))

If the descriptor does not have a resilient superclass, the value at this offset
is the negative size of metadata objects of this class (in words).

However, if the superclass is resilient, the value is a relative direct pointer
referencing a cache holding the metadata's extents.

#### Metadata Positive Size in Words/Extra Class Flags (offset 7 (4 byte))

If the descriptor does not have a resilient superclass, the value at this offset
is the positive size of metadata objects of this class (in words).

However, if the superclass is resilient, the value is `uint32_t` integer that
describes the extra class flags in the following layout:

|              Info              | Mask |
| ------------------------------ | ---- |
| Has ObjC Resilient Class Stub? |  0x1 |

## Protocol Conformance Descriptor

A protocol conformance descriptor states that a given type conforms to a
particular protocol. Protocol conformance records are emitted into their own
section, which is scanned by the Swift runtime when needed (e.g., in response to
a `swift_conformsToProtocol()` query). Each protocol conformance descriptor
contains the following fields:

#### Protocol Descriptor (offset 0)

A relative indirectable pointer referencing the protocol descriptor describing
the protocol of the conformance.

#### Type Reference (offset 1 (4 byte))

Depending on the type reference kind found in the flags field, this field can
have different values.

If the type reference kind is direct type descriptor, then this value is a
relative direct pointer to the type context descriptor.

If the type reference kind is indirect type descriptor, then this value is a
relative indirect pointer to the type context descriptor.

If the type reference kind is direct ObjC class name, then this value is a
relative direct pointer to the class name.

If the type reference kind is indirect ObjC class name, then this value is a
relative indirect pointer to the class name.

#### Witness Table (offset 2 (4 byte))

A relative direct pointer to the witness table which describes the conformance
itself.

#### Flags (offset 3 (4 byte))

This is a `uint32_t` integer that describes the conformance in the following
layout:

|             Info             |             Mask           |
| ---------------------------- | -------------------------- |
| Type Reference               |  (value & (0x7 << 3)) >> 3 |
| Is Retroactive?              |                   0x1 << 6 |
| Is Synthesized Non-Unique?   |                   0x1 << 7 |
| Num Conditional Requirements | (value & (0xFF << 8)) >> 8 |
| Has Resilient Witnesses?     |                  0x1 << 16 |
| Has Generic Witness Table?   |                  0x1 << 17 |

where Type Reference can be any of the following kinds:

|   Type Reference    | Kind |
| ------------------- | ---- |
| Direct Descriptor   |    0 |
| Indirect Descriptor |    1 |
| Direct ObjC Name    |    2 |
| Indirect ObjC Name  |    3 |

## Generic Argument Vector

Metadata records for instances of generic types contain information about their
generic arguments. For each parameter of the type, a reference to the metadata
record for the type argument is stored.  After all of the type argument
metadata references, for each type parameter, if there are protocol
requirements on that type parameter, a reference to the witness table for each
protocol it is required to conform to is stored in declaration order.

For example, given a generic type with the parameters `<T, U, V>`, its
generic parameter record will consist of references to the metadata records
for `T`, `U`, and `V` in succession, as if laid out in a C struct:

```c
struct GenericParameterVector {
  TypeMetadata *T, *U, *V;
};
```

If we add protocol requirements to the parameters, for example,
`<T: Runcible, U: Fungible & Ansible, V>`, then the type's generic
parameter vector contains witness tables for those protocols, as if laid out:

```c
struct GenericParameterVector {
  TypeMetadata *T, *U, *V;
  RuncibleWitnessTable *T_Runcible;
  FungibleWitnessTable *U_Fungible;
  AnsibleWitnessTable *U_Ansible;
};
```

## Generic Requirement Descriptor

In a generic signature, there can contain requirements which constrain types
in the signtature. This descriptor describes a single requirement.

#### Flags (offset 0)

This is a `uint32_t` integer which describes what kind of requirement this is,
whether the requirement has a key argument, has an extra argument, etc. The flags
are laid out as follows:

|      Info     | Mask |
| ------------- | ---- |
| Kind          | 0x1F |
| Has Key Arg   | 0x80 |
| Has Extra Arg | 0x40 |

The different requirement kinds:

|    Requirement   | Kind |
| ---------------- | ---- |
| Protocol         |    0 |
| Same Type        |    1 |
| Base Class       |    2 |
| Same Conformance |    3 |
| Layout           | 0x1F |

#### Parameter Name (offset 1 (4 byte))

A relative direct pointer to the type that's constrained, described as a
mangled name.

#### Requirement (offset 2 (4 byte))

Depending on the requirement kind, there are multiple different things that can
go in this field.

If the requirement is same type OR base class, then this field is a relative
direct pointer to the mangled type name.

If the requirement is a protocol, then this field is a relative indirectable
pointer int pair where the int value indicates whether its a Swift protocol or
an ObjC protocol. The pointer value references a protocol descriptor.

If the requirement is a same conformance, then this field is a relative
indirectable pointer which references a relative direct pointer to the protocol
conformance descriptor.

If the requirement is a layout, then this field indicates the layout kind as
follows:

| Layout | Kind |
| ------ | ---- |
| Class  |    0 |

## Recursive Type Metadata Dependencies

The Swift type system is built up inductively by the application of
higher-kinded type constructors (such as "tuple" or "function", as well
as user-defined generic types) to other, existing types. Crucially, it
is the "least fixed point" of that inductive system, meaning that it
does not include **infinite types** (µ-types) whose basic identity can
only be defined in terms of themselves.

That is, it is possible to write the type:

```swift
typealias IntDict = Dictionary<String, Int>
```

but it is not possible to directly express the type:

```swift
typealias RecursiveDict = Dictionary<String, RecursiveDict>
```

However, Swift does permit the expression of types that have recursive
dependencies upon themselves in ways other than their basic identity.
For example, class `A` may inherit from a superclass `Base<A>`,
or it may contain a field of type `(A, A)`. In order to support
the dynamic reification of such types into type metadata, as well as
to support the dynamic layout of such types, Swift's metadata runtime
supports a system of metadata dependency and iterative initialization.

## Metadata States

A type metadata may be in one of several different dynamic states:

- An **abstract** metadata stores just enough information to allow the
  identity of the type to be recovered: namely, the metadata's kind
  (e.g. **struct**) and any kind-specific identity information it
  entails (e.g. the `nominal type descriptor`_ and any generic arguments).

- A **layout-complete** metadata additionally stores the components of
  the type's "external layout", necessary to compute the layout of any
  type that directly stores a value of the type. In particular, a
  metadata in this state has a meaningful value witness table.

- A **non-transitively complete** metadata has undergone any additional
  initialization that is required in order to support basic operations
  on the type. For example, a metadata in this state will have undergone
  any necessary "internal layout" that might be required in order to
  create values of the type but not to allocate storage to hold them.
  For example, a class metadata will have an instance layout, which is
  not required in order to compute the external layout, but is required
  in order to allocate instances or create a subclass.

- A **complete** metadata additionally makes certain guarantees of
  transitive completeness of the metadata referenced from the metadata.
  For example, a complete metadata for `Array<T>` guarantees that
  the metadata for `T` stored in the generic arguments vector is also
  complete.

Metadata never backtrack in their state. In particular, once metadata
is complete, it remains complete forever.

## Transitive Completeness Guarantees

A complete class metadata makes the following guarantees:

- Its superclass metadata (if it has a superclass) is complete.
- Its generic arguments (if it has any) are complete.
- By implication, the generic arguments of its superclasses are complete.

A complete struct, enum, or optional metadata makes the following guarantees:

- Its generic arguments (if it has any) are complete.

A complete tuple metadata makes the following guarantees:

- Its element types are complete.

Other kinds of type metadata do not make any completeness guarantees.
The metadata kinds with transitive guarantees are the metadata kinds that
potentially require two-phase initialization anyway. Other kinds of
metadata could otherwise declare themselves complete immediately on
allocation, so the transitive completeness guarantee would add significant
complexity to both the runtime interface and its implementation, as well
as adding probably-unrecoverable memory overhead to the allocation process.

It is also true that it is far more important to be able to efficiently
recover complete metadata from the stored arguments of a generic type
than it is to be able to recover such metadata from a function metadata.

## Completeness Requirements

Type metadata are required to be transitively complete when they are
presented to most code. This allows that code to work with the metadata
without explicitly checking for its completeness. Metadata in the other
states are typically encountered only when initializing or building up
metadata.

Specifically, a type metadata record is required to be complete when:

- It is passed as a generic argument to a function (other than a metadata
  access function, witness table access function, or metadata initialization
  function).
- It is used as a metatype value, including as the ``Self`` argument to a
  ``static`` or ``class`` method, including initializers.
- It is used to build an opaque existential value.

## Metadata Requests and Responses

When calling a metadata access function, code must provide the following
information:

- the required state of the metadata, and
- whether the callee should block until the metadata is available
  in that state.

The access function will then return:

- the metadata and
- the current dynamic state of the metadata.

Access functions will always return the correct metadata record; they
will never return a null pointer. If the metadata has not been allocated
at the time of the request, it will at least be allocated before the
access function returns. The runtime will block the current thread until
the allocation completes if necessary, and there is currently no way to
avoid this.

Since access functions always return metadata that is at least in the
abstract state, it is not meaningful to make a non-blocking request
for abstract metadata.

The returned dynamic state of the metadata may be less than the requested
state if the request was non-blocking. It is not otherwise affected by
the request; it is the known dynamic state of the metadata at the time of
the call. Note that of course this dynamic state is just a lower bound
on the actual dynamic state of the metadata, since the actual dynamic
state may be getting concurrently advanced by another thread.

In general, most code should request metadata in the **complete**
state (as discussed above) and should block until the metadata is
available in that state. However:

- When requesting metadata solely to serve as a generic argument of
  another metadata, code should request **abstract** metadata. This
  can potentially unblock cycles involving the two metadata.

- Metadata initialization code should generally make non-blocking
  requests; see the next section.

Metadata access functions that cache their results should only cache
if the dynamic state is complete; this substantially simplifies the caching
logic, and in practice most metadata will be dynamically complete.
Note that this rule can be applied without considering the request.

Code outside of the runtime should never attempt to ascertain a
metadata's current state by inspecting it, e.g. to see if it has a value
witness table. Metadata initialization is not required to use
synchronization when initializing the metadata record; the necessary
synchronization is done at a higher level in the structures which record
the metadata's dynamic state. Because of this, code inspecting aspects
of the metadata that have not been guaranteed by the returned dynamic
state may observe partially-initialized state, such as a value witness
table with a meaningless size value. Instead, that code should call
the ``swift_checkMetadataState`` function.

## Metadata Allocation and Initialization

In order to support recursive dependencies between type metadata,
the creation of type metadata is divided into two phases:

- allocation, which creates an abstract metadata, and
- initialization, which advances the metadata through the progression
  of states.

Allocation cannot fail. It should return relatively quickly and
should not make any metadata requests.

The initialization phase will be repeatedly executed until it reaches
completion. It is only executed by one thread at a time.
Compiler-emitted initialization functions are given a certain amount
of scratch space that is passed to all executions; this can be used
to skip expensive or unrepeatable steps in later re-executions.

Any particular execution of the initialization phase can fail due
to an unsatisfied dependency. It does so by returning a **metadata
dependency**, which is a pair of a metadata and a required state for
that metadata. The initialization phase is expected to make only
non-blocking requests for metadata. If a response does not satisfy
the requirement, the returned metadata and the requirement should
be presented to the caller as a dependency. The runtime does two
things with this dependency:

- It attempts to add the initialization to the **completion queue**
  of the dependent metadata. If this succeeds, the initialization
  is considered blocked; it will be unblocked as soon as the
  dependent metadata reaches the required state. But it can also
  fail if the dependency is already resolved due to concurrent
  initialization; if so, the initialization is immediately resumed.

- If it succeeds in blocking the initialization on the dependency,
  it will check for an unresolvable dependency cycle. If a cycle exists,
  it will be reported on stderr and the runtime will abort the process.
  This depends on the proper use of non-blocking requests; the runtime
  does not make any effort to detect deadlock due to cycles of blocking
  requests.

Initialization must not repeatedly report failure based on stale
information about the dynamic state of a metadata.  (For example,
it must not cache metadata states from previous executions in the
initialization scratch space.) If this happens, the runtime may spin,
repeatedly executing the initialization phase only to have it fail
in the same place due to the same stale dependency.

Compiler-emitted initialization functions are only responsible for
ensuring that the metadata is **non-transitively complete**.
They signal this by returning a null dependency to the runtime.
The runtime will then ensure transitive completion. The initialization
function should not try to "help out" by requesting complete metadata
instead of non-transitively-complete metadata; it is impossible to
resolve certain recursive transitive-closure problems without the
more holistic information available to the runtime. In general, if
an initialization function seems to require transitively-complete
metadata for something, try to make it not.

If a compiler-emitted initialization function returns a dependency,
the current state of the metadata (**abstract** vs. **layout-complete**)
will be determined by inspecting the **incomplete** bit in the flags
of the value witness table. Compiler-emitted initialization functions
are therefore responsible for ensuring that this bit is set correctly.
