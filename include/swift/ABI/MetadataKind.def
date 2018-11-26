//===--- MetadataKind.def ---------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This is a file that enables metaprogramming with metadata kinds.
//
//===----------------------------------------------------------------------===//

/// METADATAKIND(Name, Value)
///   Represents a swift native runtime metadata kind. Name is the Name of the
///   metadata kind and Value is the integral value used to identify the value.
#ifndef METADATAKIND
#define METADATAKIND(Name, Value)
#endif

/// ABSTRACTMETADATAKIND(Name, Start, End)
///   Represents an abstraction categorization of a range of metadata kind
///   values. Name is the identifier of the range and Start, End are the
///   beginning and end of the range.
#ifndef ABSTRACTMETADATAKIND
#define ABSTRACTMETADATAKIND(Name, Start, End)
#endif

/// NOMINALTYPEMETADATAKIND(Name, Value)
///   Represents the native metadata kind for a swift nominal type. Name is the
///   name of the kind and Value is the integral value used to identify the
///   value. Delegates to METADATAKIND if not defined.
#ifndef NOMINALTYPEMETADATAKIND
#define NOMINALTYPEMETADATAKIND(Name, Value) METADATAKIND(Name, Value)
#endif

/// A class type.
NOMINALTYPEMETADATAKIND(Class, 0)

/// A struct type.
NOMINALTYPEMETADATAKIND(Struct, 0 | MetadataKindIsNonHeap)

/// An enum type.
/// If we add reference enums, that needs to go here.
NOMINALTYPEMETADATAKIND(Enum, 1 | MetadataKindIsNonHeap)

/// An optional type.
NOMINALTYPEMETADATAKIND(Optional, 2 | MetadataKindIsNonHeap)

/// A foreign class, such as a Core Foundation class.
METADATAKIND(ForeignClass, 3 | MetadataKindIsNonHeap)

/// A type whose value is not exposed in the metadata system.
METADATAKIND(Opaque, 0 | MetadataKindIsRuntimePrivate | MetadataKindIsNonHeap)

/// A tuple.
METADATAKIND(Tuple, 1 | MetadataKindIsRuntimePrivate | MetadataKindIsNonHeap)

/// A monomorphic function.
METADATAKIND(Function, 2 | MetadataKindIsRuntimePrivate | MetadataKindIsNonHeap)

/// An existential type.
METADATAKIND(Existential, 3 | MetadataKindIsRuntimePrivate | MetadataKindIsNonHeap)

/// A metatype.
METADATAKIND(Metatype, 4 | MetadataKindIsRuntimePrivate | MetadataKindIsNonHeap)

/// An ObjC class wrapper.
METADATAKIND(ObjCClassWrapper, 5 | MetadataKindIsRuntimePrivate | MetadataKindIsNonHeap)

/// An existential metatype.
METADATAKIND(ExistentialMetatype, 6 | MetadataKindIsRuntimePrivate | MetadataKindIsNonHeap)

/// A heap-allocated local variable using statically-generated metadata.
METADATAKIND(HeapLocalVariable, 0 | MetadataKindIsNonType)

/// A heap-allocated local variable using runtime-instantiated metadata.
METADATAKIND(HeapGenericLocalVariable,
             0 | MetadataKindIsNonType | MetadataKindIsRuntimePrivate)

/// A native error object.
METADATAKIND(ErrorObject,
             1 | MetadataKindIsNonType | MetadataKindIsRuntimePrivate)

// getEnumeratedMetadataKind assumes that all the enumerated values here
// will be <= LastEnumeratedMetadataKind.

#undef ABSTRACTMETADATAKIND
#undef NOMINALTYPEMETADATAKIND
#undef METADATAKIND
