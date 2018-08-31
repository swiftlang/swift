//===--- Metadata.h - Swift Language ABI Metadata Support -------*- C++ -*-===//
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
// Swift runtime support for generating and uniquing metadata.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_METADATA_H
#define SWIFT_RUNTIME_METADATA_H

#include "swift/ABI/Metadata.h"
#include "swift/Reflection/Records.h"

namespace swift {

#if SWIFT_OBJC_INTEROP

  // Const cast shorthands for ObjC types.

  /// Cast to id, discarding const if necessary.
  template <typename T>
  static inline id id_const_cast(const T* value) {
    return reinterpret_cast<id>(const_cast<T*>(value));
  }

  /// Cast to Class, discarding const if necessary.
  template <typename T>
  static inline Class class_const_cast(const T* value) {
    return reinterpret_cast<Class>(const_cast<T*>(value));
  }

  /// Cast to Protocol*, discarding const if necessary.
  template <typename T>
  static inline Protocol* protocol_const_cast(const T* value) {
    return reinterpret_cast<Protocol *>(const_cast<T*>(value));
  }

  /// Cast from a CF type, discarding const if necessary.
  template <typename T>
  static inline T cf_const_cast(const void* value) {
    return reinterpret_cast<T>(const_cast<void *>(value));
  }

#endif

/// A standard routine, suitable for placement in the value witness
/// table, for copying an opaque POD object.
SWIFT_RUNTIME_EXPORT
OpaqueValue *swift_copyPOD(OpaqueValue *dest,
                           OpaqueValue *src,
                           const Metadata *self);
 
/// A value-witness table with extra inhabitants entry points.
/// These entry points are available only if the HasExtraInhabitants flag bit is
/// set in the 'flags' field.
struct ExtraInhabitantsValueWitnessTable : ValueWitnessTable {
#define WANT_ONLY_EXTRA_INHABITANT_VALUE_WITNESSES
#define VALUE_WITNESS(LOWER_ID, UPPER_ID) \
  ValueWitnessTypes::LOWER_ID LOWER_ID;
#include "swift/ABI/ValueWitness.def"

#define SET_WITNESS(NAME) base.NAME,

  constexpr ExtraInhabitantsValueWitnessTable()
    : ValueWitnessTable{}, extraInhabitantFlags(),
      storeExtraInhabitant(nullptr),
      getExtraInhabitantIndex(nullptr) {}
  constexpr ExtraInhabitantsValueWitnessTable(
                            const ValueWitnessTable &base,
                            ValueWitnessTypes::extraInhabitantFlags eif,
                            ValueWitnessTypes::storeExtraInhabitant sei,
                            ValueWitnessTypes::getExtraInhabitantIndex geii)
    : ValueWitnessTable(base),
      extraInhabitantFlags(eif),
      storeExtraInhabitant(sei),
      getExtraInhabitantIndex(geii) {}

  static bool classof(const ValueWitnessTable *table) {
    return table->flags.hasExtraInhabitants();
  }
};

/// A value-witness table with enum entry points.
/// These entry points are available only if the HasEnumWitnesses flag bit is
/// set in the 'flags' field.
struct EnumValueWitnessTable : ExtraInhabitantsValueWitnessTable {
#define WANT_ONLY_ENUM_VALUE_WITNESSES
#define VALUE_WITNESS(LOWER_ID, UPPER_ID) \
  ValueWitnessTypes::LOWER_ID LOWER_ID;
#include "swift/ABI/ValueWitness.def"

  constexpr EnumValueWitnessTable()
    : ExtraInhabitantsValueWitnessTable(),
      getEnumTag(nullptr),
      destructiveProjectEnumData(nullptr),
      destructiveInjectEnumTag(nullptr) {}
  constexpr EnumValueWitnessTable(
          const ExtraInhabitantsValueWitnessTable &base,
          ValueWitnessTypes::getEnumTag getEnumTag,
          ValueWitnessTypes::destructiveProjectEnumData destructiveProjectEnumData,
          ValueWitnessTypes::destructiveInjectEnumTag destructiveInjectEnumTag)
    : ExtraInhabitantsValueWitnessTable(base),
      getEnumTag(getEnumTag),
      destructiveProjectEnumData(destructiveProjectEnumData),
      destructiveInjectEnumTag(destructiveInjectEnumTag) {}

  static bool classof(const ValueWitnessTable *table) {
    return table->flags.hasEnumWitnesses();
  }
};

/// A type layout record. This is the subset of the value witness table that is
/// necessary to perform dependent layout of generic value types. It excludes
/// the value witness functions and includes only the size, alignment,
/// extra inhabitants, and miscellaneous flags about the type.
struct TypeLayout {
  ValueWitnessTypes::size size;
  ValueWitnessTypes::flags flags;
  ValueWitnessTypes::stride stride;

private:
  // Only available if the "hasExtraInhabitants" flag is set.
  ValueWitnessTypes::extraInhabitantFlags extraInhabitantFlags;

  void _static_assert_layout();
public:
  TypeLayout() = default;
  constexpr TypeLayout(ValueWitnessTypes::size size,
                       ValueWitnessTypes::flags flags,
                       ValueWitnessTypes::stride stride,
                       ValueWitnessTypes::extraInhabitantFlags eiFlags =
                         ValueWitnessTypes::extraInhabitantFlags())
    : size(size), flags(flags), stride(stride),
      extraInhabitantFlags(eiFlags) {}

  ValueWitnessTypes::extraInhabitantFlags getExtraInhabitantFlags() const {
    assert(flags.hasExtraInhabitants());
    return extraInhabitantFlags;
  }

  const TypeLayout *getTypeLayout() const { return this; }

  /// The number of extra inhabitants, that is, bit patterns that do not form
  /// valid values of the type, in this type's binary representation.
  unsigned getNumExtraInhabitants() const;
};

inline void TypeLayout::_static_assert_layout() {
  #define CHECK_TYPE_LAYOUT_OFFSET(FIELD)                               \
    static_assert(offsetof(ExtraInhabitantsValueWitnessTable, FIELD)    \
                    - offsetof(ExtraInhabitantsValueWitnessTable, size) \
                  == offsetof(TypeLayout, FIELD),                       \
                  "layout of " #FIELD " in TypeLayout doesn't match "   \
                  "value witness table")
  CHECK_TYPE_LAYOUT_OFFSET(size);
  CHECK_TYPE_LAYOUT_OFFSET(flags);
  CHECK_TYPE_LAYOUT_OFFSET(stride);
  CHECK_TYPE_LAYOUT_OFFSET(extraInhabitantFlags);

  #undef CHECK_TYPE_LAYOUT_OFFSET
}

template <>
inline void ValueWitnessTable::publishLayout(const TypeLayout &layout) {
  size = layout.size;
  stride = layout.stride;

  // Currently there is nothing in the runtime or ABI which tries to
  // asynchronously check completion, so we can just do a normal store here.
  //
  // If we decide to start allowing that (to speed up checkMetadataState,
  // maybe), we'll have to:
  //   - turn this into an store-release,
  //   - turn the load in checkIsComplete() into a load-acquire, and
  //   - do something about getMutableVWTableForInit.
  flags = layout.flags;
}

template <> inline bool ValueWitnessTable::checkIsComplete() const {
  return !flags.isIncomplete();
}

template <>
inline const ExtraInhabitantsValueWitnessTable *
ValueWitnessTable::_asXIVWT() const {
  assert(ExtraInhabitantsValueWitnessTable::classof(this));
  return static_cast<const ExtraInhabitantsValueWitnessTable *>(this);
}

template <>
inline const EnumValueWitnessTable *ValueWitnessTable::_asEVWT() const {
  assert(EnumValueWitnessTable::classof(this));
  return static_cast<const EnumValueWitnessTable *>(this);
}

template <> inline unsigned ValueWitnessTable::getNumExtraInhabitants() const {
  // If the table does not have extra inhabitant witnesses, then there are zero.
  if (!flags.hasExtraInhabitants())
    return 0;
  return this->_asXIVWT()->extraInhabitantFlags.getNumExtraInhabitants();
}

inline unsigned TypeLayout::getNumExtraInhabitants() const {
  // If the table does not have extra inhabitant witnesses, then there are zero.
  if (!flags.hasExtraInhabitants())
    return 0;
  return extraInhabitantFlags.getNumExtraInhabitants();
}

// Standard value-witness tables.

// The "Int" tables are used for arbitrary POD data with the matching
// size/alignment characteristics.
SWIFT_RUNTIME_EXPORT
const ValueWitnessTable VALUE_WITNESS_SYM(Bi8_);   // Builtin.Int8
SWIFT_RUNTIME_EXPORT
const ValueWitnessTable VALUE_WITNESS_SYM(Bi16_);  // Builtin.Int16
SWIFT_RUNTIME_EXPORT
const ValueWitnessTable VALUE_WITNESS_SYM(Bi32_);  // Builtin.Int32
SWIFT_RUNTIME_EXPORT
const ValueWitnessTable VALUE_WITNESS_SYM(Bi64_);  // Builtin.Int64
SWIFT_RUNTIME_EXPORT
const ValueWitnessTable VALUE_WITNESS_SYM(Bi128_); // Builtin.Int128
SWIFT_RUNTIME_EXPORT
const ValueWitnessTable VALUE_WITNESS_SYM(Bi256_); // Builtin.Int256
SWIFT_RUNTIME_EXPORT
const ValueWitnessTable VALUE_WITNESS_SYM(Bi512_); // Builtin.Int512

// The object-pointer table can be used for arbitrary Swift refcounted
// pointer types.
SWIFT_RUNTIME_EXPORT
const ExtraInhabitantsValueWitnessTable VALUE_WITNESS_SYM(Bo); // Builtin.NativeObject

SWIFT_RUNTIME_EXPORT
const ExtraInhabitantsValueWitnessTable VALUE_WITNESS_SYM(Bb); // Builtin.BridgeObject

SWIFT_RUNTIME_EXPORT
const ExtraInhabitantsValueWitnessTable VALUE_WITNESS_SYM(Bp); // Builtin.RawPointer

#if SWIFT_OBJC_INTEROP
// The ObjC-pointer table can be used for arbitrary ObjC pointer types.
SWIFT_RUNTIME_EXPORT
const ExtraInhabitantsValueWitnessTable VALUE_WITNESS_SYM(BO); // Builtin.UnknownObject
#endif

// The () -> () table can be used for arbitrary function types.
SWIFT_RUNTIME_EXPORT
const ExtraInhabitantsValueWitnessTable
  VALUE_WITNESS_SYM(FUNCTION_MANGLING);     // () -> ()

// The @escaping () -> () table can be used for arbitrary escaping function types.
SWIFT_RUNTIME_EXPORT
const ExtraInhabitantsValueWitnessTable
  VALUE_WITNESS_SYM(NOESCAPE_FUNCTION_MANGLING);     // @noescape () -> ()

// The @convention(thin) () -> () table can be used for arbitrary thin function types.
SWIFT_RUNTIME_EXPORT
const ExtraInhabitantsValueWitnessTable
  VALUE_WITNESS_SYM(THIN_FUNCTION_MANGLING);    // @convention(thin) () -> ()

// The () table can be used for arbitrary empty types.
SWIFT_RUNTIME_EXPORT
const ValueWitnessTable VALUE_WITNESS_SYM(EMPTY_TUPLE_MANGLING);        // ()

// The table for aligned-pointer-to-pointer types.
SWIFT_RUNTIME_EXPORT
const ExtraInhabitantsValueWitnessTable METATYPE_VALUE_WITNESS_SYM(Bo); // Builtin.NativeObject.Type

/// Return the value witnesses for unmanaged pointers.
static inline const ValueWitnessTable &getUnmanagedPointerValueWitnesses() {
#if __POINTER_WIDTH__ == 64
  return VALUE_WITNESS_SYM(Bi64_);
#else
  return VALUE_WITNESS_SYM(Bi32_);
#endif
}

/// Return value witnesses for a pointer-aligned pointer type.
static inline
const ExtraInhabitantsValueWitnessTable &
getUnmanagedPointerPointerValueWitnesses() {
  return METATYPE_VALUE_WITNESS_SYM(Bo);
}

using OpaqueMetadata = TargetOpaqueMetadata<InProcess>;

// Standard POD opaque metadata.
// The "Int" metadata are used for arbitrary POD data with the
// matching characteristics.
using FullOpaqueMetadata = FullMetadata<OpaqueMetadata>;
#define BUILTIN_TYPE(Symbol, Name) \
    SWIFT_RUNTIME_EXPORT \
    const FullOpaqueMetadata METADATA_SYM(Symbol);
#include "swift/Runtime/BuiltinTypes.def"

/// The standard metadata for the empty tuple type.
SWIFT_RUNTIME_EXPORT
const
  FullMetadata<TupleTypeMetadata> METADATA_SYM(EMPTY_TUPLE_MANGLING);

/// The standard metadata for the empty protocol composition type, Any.
SWIFT_RUNTIME_EXPORT
const
  FullMetadata<ExistentialTypeMetadata> METADATA_SYM(ANY_MANGLING);

/// The standard metadata for the empty class-constrained protocol composition
/// type, AnyObject.
SWIFT_RUNTIME_EXPORT
const
  FullMetadata<ExistentialTypeMetadata> METADATA_SYM(ANYOBJECT_MANGLING);


/// True if two context descriptors in the currently running program describe
/// the same context.
SWIFT_RUNTIME_EXPORT
bool equalContexts(const ContextDescriptor *a, const ContextDescriptor *b);

/// Compute the bounds of class metadata with a resilient superclass.
ClassMetadataBounds getResilientMetadataBounds(
                                           const ClassDescriptor *descriptor);
int32_t getResilientImmediateMembersOffset(const ClassDescriptor *descriptor);

/// \brief Fetch a uniqued metadata object for a nominal type which requires
/// singleton metadata initialization.
SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
MetadataResponse
swift_getSingletonMetadata(MetadataRequest request,
                           const TypeContextDescriptor *description);

/// \brief Fetch a uniqued metadata object for a generic nominal type.
SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
MetadataResponse
swift_getGenericMetadata(MetadataRequest request,
                         const void * const *arguments,
                         const TypeContextDescriptor *description);

/// Allocate a generic class metadata object.  This is intended to be
/// called by the metadata instantiation function of a generic class.
///
/// This function:
///   - computes the required size of the metadata object based on the
///     class hierarchy;
///   - allocates memory for the metadata object based on the computed
///     size and the additional requirements imposed by the pattern;
///   - copies information from the pattern into the allocated metadata; and
///   - fully initializes the ClassMetadata header, except that the
///     superclass pointer will be null (or SwiftObject under ObjC interop
///     if there is no formal superclass).
///
/// The instantiation function is responsible for completing the
/// initialization, including:
///   - setting the superclass pointer;
///   - copying class data from the superclass;
///   - installing the generic arguments;
///   - installing new v-table entries and overrides; and
///   - registering the class with the runtime under ObjC interop.
/// Most of this work can be achieved by calling swift_initClassMetadata.
SWIFT_RUNTIME_EXPORT
ClassMetadata *
swift_allocateGenericClassMetadata(const ClassDescriptor *description,
                                   const void *arguments,
                                   const GenericClassMetadataPattern *pattern);

/// Allocate a generic value metadata object.  This is intended to be
/// called by the metadata instantiation function of a generic struct or
/// enum.
SWIFT_RUNTIME_EXPORT
ValueMetadata *
swift_allocateGenericValueMetadata(const ValueTypeDescriptor *description,
                                   const void *arguments,
                                   const GenericValueMetadataPattern *pattern,
                                   size_t extraDataSize);

/// \brief Check that the given metadata has the right state.
SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
MetadataResponse swift_checkMetadataState(MetadataRequest request,
                                          const Metadata *type);

/// Instantiate a resilient or generic protocol witness table.
///
/// \param genericTable - The witness table template for the
///   conformance. It may either have fields that require runtime
///   initialization, or be missing requirements at the end for
///   which default witnesses are available.
///
/// \param type - The conforming type, used to form a uniquing key
///   for the conformance. If the witness table is not dependent on
///   the substituted type of the conformance, this can be set to
///   nullptr, in which case there will only be one instantiated
///   witness table per witness table template.
///
/// \param instantiationArgs - An opaque pointer that's forwarded to
///   the instantiation function, used for conditional conformances.
///   This API implicitly embeds an assumption that these arguments
///   never form part of the uniquing key for the conformance, which
///   is ultimately a statement about the user model of overlapping
///   conformances.
SWIFT_RUNTIME_EXPORT
const WitnessTable *
swift_getGenericWitnessTable(GenericWitnessTable *genericTable,
                             const Metadata *type,
                             void **const *instantiationArgs);

/// \brief Fetch a uniqued metadata for a function type.
SWIFT_RUNTIME_EXPORT
const FunctionTypeMetadata *
swift_getFunctionTypeMetadata(FunctionTypeFlags flags,
                              const Metadata *const *parameters,
                              const uint32_t *parameterFlags,
                              const Metadata *result);

SWIFT_RUNTIME_EXPORT
const FunctionTypeMetadata *
swift_getFunctionTypeMetadata0(FunctionTypeFlags flags,
                               const Metadata *result);

SWIFT_RUNTIME_EXPORT
const FunctionTypeMetadata *
swift_getFunctionTypeMetadata1(FunctionTypeFlags flags,
                               const Metadata *arg0,
                               const Metadata *result);

SWIFT_RUNTIME_EXPORT
const FunctionTypeMetadata *
swift_getFunctionTypeMetadata2(FunctionTypeFlags flags,
                               const Metadata *arg0,
                               const Metadata *arg1,
                               const Metadata *result);

SWIFT_RUNTIME_EXPORT
const FunctionTypeMetadata *swift_getFunctionTypeMetadata3(
                                                FunctionTypeFlags flags,
                                                const Metadata *arg0,
                                                const Metadata *arg1,
                                                const Metadata *arg2,
                                                const Metadata *result);

#if SWIFT_OBJC_INTEROP
SWIFT_RUNTIME_EXPORT
void
swift_instantiateObjCClass(const ClassMetadata *theClass);

SWIFT_RUNTIME_EXPORT
Class
swift_getInitializedObjCClass(Class c);

/// \brief Fetch a uniqued type metadata for an ObjC class.
SWIFT_RUNTIME_EXPORT
const Metadata *
swift_getObjCClassMetadata(const ClassMetadata *theClass);

/// \brief Get the ObjC class object from class type metadata.
SWIFT_RUNTIME_EXPORT
const ClassMetadata *
swift_getObjCClassFromMetadata(const Metadata *theClass);

SWIFT_RUNTIME_EXPORT
const ClassMetadata *
swift_getObjCClassFromObject(HeapObject *object);
#endif

/// \brief Fetch a unique type metadata object for a foreign type.
SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
MetadataResponse
swift_getForeignTypeMetadata(MetadataRequest request,
                             ForeignTypeMetadata *nonUnique);

/// \brief Fetch a unique witness table for a foreign witness table.
SWIFT_RUNTIME_EXPORT
const WitnessTable *
swift_getForeignWitnessTable(const WitnessTable *nonUniqueWitnessCandidate,
                             const TypeContextDescriptor *forForeignType,
                             const ProtocolDescriptor *forProtocol);

/// \brief Fetch a uniqued metadata for a tuple type.
///
/// The labels argument is null if and only if there are no element
/// labels in the tuple.  Otherwise, it is a null-terminated
/// concatenation of space-terminated NFC-normalized UTF-8 strings,
/// assumed to point to constant global memory.
///
/// That is, for the tuple type (a : Int, Int, c : Int), this
/// argument should be:
///   "a  c \0"
///
/// This representation allows label strings to be efficiently
/// (1) uniqued within a linkage unit and (2) compared with strcmp.
/// In other words, it's optimized for code size and uniquing
/// efficiency, not for the convenience of actually consuming
/// these strings.
///
/// \param elements - potentially invalid if numElements is zero;
///   otherwise, an array of metadata pointers.
/// \param labels - the labels string
/// \param proposedWitnesses - an optional proposed set of value witnesses.
///   This is useful when working with a non-dependent tuple type
///   where the entrypoint is just being used to unique the metadata.
SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
MetadataResponse
swift_getTupleTypeMetadata(MetadataRequest request,
                           TupleTypeFlags flags,
                           const Metadata * const *elements,
                           const char *labels,
                           const ValueWitnessTable *proposedWitnesses);

SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
MetadataResponse
swift_getTupleTypeMetadata2(MetadataRequest request,
                            const Metadata *elt0, const Metadata *elt1,
                            const char *labels,
                            const ValueWitnessTable *proposedWitnesses);
SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
MetadataResponse
swift_getTupleTypeMetadata3(MetadataRequest request,
                            const Metadata *elt0, const Metadata *elt1,
                            const Metadata *elt2, const char *labels,
                            const ValueWitnessTable *proposedWitnesses);

/// Perform layout as if for a tuple whose elements have the given layouts.
///
/// \param tupleLayout - A structure into which to write the tuple layout.
///   Must be non-null.
/// \param elementOffsets - An array into which to write the offsets of
///   the elements.  May be null.  Must have space for all elements,
///   including element 0 (which will always have offset 0).
SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
void swift_getTupleTypeLayout(TypeLayout *tupleLayout,
                              uint32_t *elementOffsets,
                              TupleTypeFlags flags,
                              const TypeLayout * const *elements);

/// Perform layout as if for a two-element tuple whose elements have
/// the given layouts.
///
/// \param tupleLayout - A structure into which to write the tuple layout.
///   Must be non-null.
/// \returns The offset of the second element.
///   The first element always has offset 0.
SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
size_t swift_getTupleTypeLayout2(TypeLayout *tupleLayout,
                                 const TypeLayout *elt0,
                                 const TypeLayout *elt1);

struct OffsetPair { size_t First; size_t Second; };

/// Perform layout as if for a three-element tuple whose elements have
/// the given layouts.
///
/// \param tupleLayout - A structure into which to write the tuple layout.
///   Must be non-null.
/// \returns The offsets of the second and third elements.
///   The first element always has offset 0.
SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
OffsetPair swift_getTupleTypeLayout3(TypeLayout *tupleLayout,
                                     const TypeLayout *elt0Layout,
                                     const TypeLayout *elt1Layout,
                                     const TypeLayout *elt2Layout);

/// Initialize the value witness table and struct field offset vector for a
/// struct, using the "Universal" layout strategy.
SWIFT_RUNTIME_EXPORT
void swift_initStructMetadata(StructMetadata *self,
                              StructLayoutFlags flags,
                              size_t numFields,
                              const TypeLayout * const *fieldTypes,
                              uint32_t *fieldOffsets);

/// Allocate the metadata for a class and copy fields from the given pattern.
/// The final size of the metadata is calculated at runtime from the metadata
/// bounds in the class descriptor.
///
/// This function is only intended to be called from the relocation function
/// of a resilient class pattern.
SWIFT_RUNTIME_EXPORT
ClassMetadata *
swift_relocateClassMetadata(ClassDescriptor *descriptor,
                            ResilientClassMetadataPattern *pattern);

/// Initialize the field offset vector for a dependent-layout class, using the
/// "Universal" layout strategy.
SWIFT_RUNTIME_EXPORT
void swift_initClassMetadata(ClassMetadata *self,
                             ClassMetadata *super,
                             ClassLayoutFlags flags,
                             size_t numFields,
                             const TypeLayout * const *fieldTypes,
                             size_t *fieldOffsets);

/// \brief Fetch a uniqued metadata for a metatype type.
SWIFT_RUNTIME_EXPORT
const MetatypeMetadata *
swift_getMetatypeMetadata(const Metadata *instanceType);

/// \brief Fetch a uniqued metadata for an existential metatype type.
SWIFT_RUNTIME_EXPORT
const ExistentialMetatypeMetadata *
swift_getExistentialMetatypeMetadata(const Metadata *instanceType);

/// \brief Fetch a uniqued metadata for an existential type. The array
/// referenced by \c protocols will be sorted in-place.
SWIFT_RUNTIME_EXPORT
const ExistentialTypeMetadata *
swift_getExistentialTypeMetadata(ProtocolClassConstraint classConstraint,
                                 const Metadata *superclassConstraint,
                                 size_t numProtocols,
                                 const ProtocolDescriptorRef *protocols);

/// \brief Perform a copy-assignment from one existential container to another.
/// Both containers must be of the same existential type representable with the
/// same number of witness tables.
SWIFT_RUNTIME_EXPORT
OpaqueValue *swift_assignExistentialWithCopy(OpaqueValue *dest,
                                             const OpaqueValue *src,
                                             const Metadata *type);

/// \brief Perform a copy-assignment from one existential container to another.
/// Both containers must be of the same existential type representable with no
/// witness tables.
OpaqueValue *swift_assignExistentialWithCopy0(OpaqueValue *dest,
                                              const OpaqueValue *src,
                                              const Metadata *type);

/// \brief Perform a copy-assignment from one existential container to another.
/// Both containers must be of the same existential type representable with one
/// witness table.
OpaqueValue *swift_assignExistentialWithCopy1(OpaqueValue *dest,
                                              const OpaqueValue *src,
                                              const Metadata *type);

/// Calculate the numeric index of an extra inhabitant of a heap object
/// pointer in memory.
inline int swift_getHeapObjectExtraInhabitantIndex(HeapObject * const* src) {
  // This must be consistent with the getHeapObjectExtraInhabitantIndex
  // implementation in IRGen's ExtraInhabitants.cpp.

  using namespace heap_object_abi;

  uintptr_t value = reinterpret_cast<uintptr_t>(*src);
  if (value >= LeastValidPointerValue)
    return -1;

  // Check for tagged pointers on appropriate platforms.  Knowing that
  // value < LeastValidPointerValue tells us a lot.
#if SWIFT_OBJC_INTEROP
  if (value & ((uintptr_t(1) << ObjCReservedLowBits) - 1))
    return -1;
  return int(value >> ObjCReservedLowBits);
#else
  return int(value);
#endif
}
  
/// Store an extra inhabitant of a heap object pointer to memory,
/// in the style of a value witness.
inline void swift_storeHeapObjectExtraInhabitant(HeapObject **dest, int index) {
  // This must be consistent with the storeHeapObjectExtraInhabitant
  // implementation in IRGen's ExtraInhabitants.cpp.

#if SWIFT_OBJC_INTEROP
  auto value = uintptr_t(index) << heap_object_abi::ObjCReservedLowBits;
#else
  auto value = uintptr_t(index);
#endif
  *dest = reinterpret_cast<HeapObject*>(value);
}

/// Return the number of extra inhabitants in a heap object pointer.
inline constexpr unsigned swift_getHeapObjectExtraInhabitantCount() {
  // This must be consistent with the getHeapObjectExtraInhabitantCount
  // implementation in IRGen's ExtraInhabitants.cpp.

  using namespace heap_object_abi;

  // The runtime needs no more than INT_MAX inhabitants.
#if SWIFT_OBJC_INTEROP
  return (LeastValidPointerValue >> ObjCReservedLowBits) > INT_MAX
    ? (unsigned)INT_MAX
    : (unsigned)(LeastValidPointerValue >> ObjCReservedLowBits);
#else
  return (LeastValidPointerValue) > INT_MAX
    ? unsigned(INT_MAX)
    : unsigned(LeastValidPointerValue);
#endif
}  

/// Calculate the numeric index of an extra inhabitant of a function
/// pointer in memory.
inline int swift_getFunctionPointerExtraInhabitantIndex(void * const* src) {
  // This must be consistent with the getFunctionPointerExtraInhabitantIndex
  // implementation in IRGen's ExtraInhabitants.cpp.
  uintptr_t value = reinterpret_cast<uintptr_t>(*src);
  return (value < heap_object_abi::LeastValidPointerValue
            ? (int) value : -1);
}
  
/// Store an extra inhabitant of a function pointer to memory, in the
/// style of a value witness.
inline void swift_storeFunctionPointerExtraInhabitant(void **dest, int index) {
  // This must be consistent with the storeFunctionPointerExtraInhabitantIndex
  // implementation in IRGen's ExtraInhabitants.cpp.
  *dest = reinterpret_cast<void*>(static_cast<uintptr_t>(index));
}

/// Return the number of extra inhabitants in a function pointer.
inline constexpr unsigned swift_getFunctionPointerExtraInhabitantCount() {
  // This must be consistent with the getFunctionPointerExtraInhabitantCount
  // implementation in IRGen's ExtraInhabitants.cpp.

  using namespace heap_object_abi;

  // The runtime needs no more than INT_MAX inhabitants.
  return (LeastValidPointerValue) > INT_MAX
    ? (unsigned)INT_MAX
    : (unsigned)(LeastValidPointerValue);
}

/// Return the type name for a given type metadata.
std::string nameForMetadata(const Metadata *type,
                            bool qualified = true);

/// Register a block of protocol records for dynamic lookup.
SWIFT_RUNTIME_EXPORT
void swift_registerProtocols(const ProtocolRecord *begin,
                             const ProtocolRecord *end);

/// Register a block of protocol conformance records for dynamic lookup.
SWIFT_RUNTIME_EXPORT
void swift_registerProtocolConformances(const ProtocolConformanceRecord *begin,
                                        const ProtocolConformanceRecord *end);

/// Register a block of type context descriptors for dynamic lookup.
SWIFT_RUNTIME_EXPORT
void swift_registerTypeMetadataRecords(const TypeMetadataRecord *begin,
                                       const TypeMetadataRecord *end);

/// Return the superclass, if any.  The result is nullptr for root
/// classes and class protocol types.
SWIFT_CC(swift)
SWIFT_RUNTIME_STDLIB_API
const Metadata *_swift_class_getSuperclass(const Metadata *theClass);

#if !NDEBUG
/// Verify that the given metadata pointer correctly roundtrips its
/// mangled name through the demangler.
void verifyMangledNameRoundtrip(const Metadata *metadata);
#endif

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_API
const TypeContextDescriptor *swift_getTypeContextDescriptor(const Metadata *type);

// Defined in KeyPath.swift in the standard library.
SWIFT_RUNTIME_EXPORT
const HeapObject *swift_getKeyPath(const void *pattern, const void *arguments);

} // end namespace swift

#endif // SWIFT_RUNTIME_METADATA_H
