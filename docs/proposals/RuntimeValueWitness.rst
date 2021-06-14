##############
Layout Strings
##############

Introduction
***********
Layout strings encode the structure of a type into a bytestring that can be
interpreted by a runtime function to achieve a destroy or copy. Rather than
generating ir for a destroy/assignWithCopy/etc, we instead generate a layout
string which encodes enough information for a called runtime function to
perform the operation for us. Value witness functions tend to be quite large,
so this allows us to replace them with a single call instead. This gives us
the option of making a codesize/runtime cost trade off.

NB: Integers in this format are read and written as big endian. There's no
strong reason for this. Just that it's slightly easier to read/debug the
string when dumping it from memory.

The Runtime Functions
********************
There are 5 runtime functions which follow from the 5 similar value witness functions:

Destroy a given value at addr
::
   swift_generic_destroy(void* addr, Metadata* metadata, const char* layoutStr);

Retain src if we are not taking, release dest, then copy a value from src to dest.
::
   swift_generic_assign(void* dest, void* src, Metadata* metadata, const char* layoutStr, bool isTake);

Retain src if we are not taking, then copy a value from src to dest.
::
   swift_generic_initialize(void* dest, void* src, Metadata* metadata, const char* layoutStr, bool isTake);

Each currently takes the layout string as an additional arugment. Ideally the
layout string is stored in the metadata so that the extra arguments are not
needed. This would additionally allow us to dynamically create strings at
metadata initialization time to do things like runtime generic specialization
for the layout string.

Value types
*******

VALUE := SCALAR | ALIGNED_GROUP | SINGLE_ENUM | MULTI_ENUM | ARCHETYPE | RESILIENT_TYPE

Scalars
******

SCALAR := 'c'|'s'|'l'|'L'|'C'|'r'|'N'|'n'|'W'|'u'|'w'|'b'|'B'|'o'|'f'|'x'

For our purposes, scalars are types that are not composed of other types. In
swift, this is mostly POD types such as ints, and reference types.

We define POD types for types that just represent data. These do need to be
copied, but do not need to be released or retained.
::
   I8 = 'c',
   I16 = 's',
   I32 = 'l',
   I64 = 'L',

We also have reference types. While they are all 64bit sized, we need to
differentiate between them because they have different ways of being
released/retained.

::
   ErrorReference = 'r',
   NativeStrongReference = 'N',
   NativeUnownedReference = 'n',
   NativeWeakReference = 'W',
   UnknownUnownedReference = 'u',
   UnknownWeakReference = 'w',
   BlockReference = 'b',
   BridgeReference = 'B',
   ObjCReference = 'o',
   ExistentialReference = 'x',

Aligned Group
*************
Structs are expressed as a group of values that have required alignments.
::
   ALIGNED_GROUP:= 'a' UINT32 (ALIGNMENT,UINT32,VALUE)+
   // ALIGNED_GROUP:= 'a' numFields (alignment,fieldLength,field)+
   ALIGNMENT := '0'|'1'|'2'|'3'

The Alignment attached to the structs indicates the field should be aligned on
2^(ALIGNMENT) bytes

Enums
*******

We distinguish between the less complex single enums, and the more complex
multi payload enums. Note the no payload enums are lowered to a POD scalar
rather than an enum.

Single Enums
-------------
::
   SIZE := uint32

   // e numEmptyPayloads lengthOfPayload payload
   SINGLEENUM := 'e' SIZE SIZE VALUE

For single payload enums we need enough information to determine the overall
size of the enum and how to release/retain it. For example, to release an
single payload enum, we need to do the following:

::
   destroy SINGLEENUM:
       compute extra inhabitants of PAYLOAD
       determine if numEmptyPayloads fits in extra inhabitants
       if they don't fit, add extra tag bits
       check if any extra inhabitant bits or extra tag bits are set
       if not, we have a value:
           destroy value

Multi Enums
-----------
::
   // E numEmptyPayloads numPayloads lengthOfEachPayload payloads
   MULTIENUM := 'E' SIZE SIZE SIZE+ VALUE+

For multi payload enums we need enough information to determine the overall
size of the enum from each paylaod and how to release/retain each payload. For
example to release a multi enum, we need to do the following:

::
   destroy MULTIENUM:
       compute and merge the extra inhabitants of each possible payload
       compute the overall size of the enum (size of largest payload plus any extra tag bits)
       use the extra inhabitants and extra tag bits to get the encoded enum case
       if the case < numPayloads:
           destroy the indicated payload

Examples
********

Struct
------
::
   struct {
    let a : Int8
    let b : Int16
    let c : Int16
    let d : SomeClass
   }

byte aligned int8
2 byte aligned int16
2 byte aligned int16
8 byte aligned Native Pointer
::
   '1c2s2s8N'

Single Enum
----
::
   enum {
     case a(c: SomeClasee)
     case b
     case c
     case d
   }

A single enum with 3 no payload cases, a payload length of 1, and a payload of
a single Native Pointer
::
    'e<0x0><0x0><0x0><0x3><0x0><0x0><0x0><0x1>N'

Multi Enum
----
::
   struct MyStruct {
     let a: SomeClass
     let b: SomeClass
   }
   enum {
     case a(c: SomeClass)
     case b(c: MyStruct)
     case c
     case d
     case e
   }

A Multi enum with 3 no payload cases, two payloads,  one of a struct, the other of just a Native pointer
::
    'E<0x0><0x0><0x0><0x3><0x0><0x0><0x0><0x2><0x0><0x0><0x0><0x4><0x0><0x0><0x0><0x1>N4N4N'
     ^| Num no payloads  | num payloads      | strlength payload1 |strlen payload2   |^| MyStruct
     |----+--------Multi Enum Indicator                                               |--SomeClass
