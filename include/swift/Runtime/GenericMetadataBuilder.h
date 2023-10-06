//===--- GenericMetadataBuilder.h - Build generic metadata. -----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Builder for generic metadata, in-process and out-of-process.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_GENERIC_METADATA_BUILDER_H
#define SWIFT_RUNTIME_GENERIC_METADATA_BUILDER_H
#include "swift/ABI/Metadata.h"
#include "swift/Basic/MathUtils.h"
#include "swift/Demangling/TypeLookupError.h"
#include "swift/Runtime/Portability.h"
#include "llvm/Support/Casting.h"
#include <stddef.h>
#include <stdint.h>
#include <string>
#include <variant>

#define METADATA_BUILDER_LOG(...)                                              \
  readerWriter.log(__FILE_NAME__, __LINE__, __func__, __VA_ARGS__)

namespace swift {

/// Helper function to encode a size, alignment, and extra inhabitants flag into
/// a single 64-bit value for switch/case statements.
static inline constexpr uint64_t
sizeWithAlignmentMask(uint64_t size, uint64_t alignmentMask,
                      uint64_t hasExtraInhabitants) {
  return (hasExtraInhabitants << 48) | (size << 16) | alignmentMask;
}

/// An error produced when building metadata. This is a small wrapper around
/// a std::string which describes the error.
class BuilderError {
  std::string errorString;

public:
  BuilderError(std::string string) : errorString(string) {}
  BuilderError(char *string) : errorString(string) {}

  /// Make a BuilderError using a standard printf format string and arguments.
  [[gnu::format(printf, 2, 3)]] BuilderError(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    char *string = nullptr;
    swift_vasprintf(&string, fmt, args);
    if (string)
      errorString = string;
    else
      errorString = "<could not create error string>";
    std::free(string);

    va_end(args);
  }

  std::string getErrorString() const { return errorString; }

  const char *cStr() const { return errorString.c_str(); }
};

/// A value that's either a BuilderError or some other success value. Use this
/// as the return type from any function that could return an error.
///
/// If the function returns no value on success, use
/// BuilderErrorOr<std::monostate>. To return success, write `return {{}}` which
/// constructs a BuilderErrorOr with the singular monostate value. This looks
/// weird but it works.
template <typename T>
class [[nodiscard]] BuilderErrorOr {
  std::variant<T, BuilderError> storage;

public:
  BuilderErrorOr(const T &value) : storage(value) {}
  BuilderErrorOr(const BuilderError &error) : storage(error) {}

  /// Create a BuilderErrorOr from a TypeLookupError returned by runtime type
  /// lookup.
  BuilderErrorOr(const swift::TypeLookupError &error) {
    char *errorStr = error.copyErrorString();
    storage = BuilderError(errorStr);
    error.freeErrorString(errorStr);
  }

  /// Get a pointer to the wrapped success value, or NULL if the value is an
  /// error.
  T *getValue() { return std::get_if<T>(&storage); }

  /// Get a pointer to the wrapped error, or NULL if the value is a success
  /// value.
  BuilderError *getError() { return std::get_if<BuilderError>(&storage); }
};

/// This macro takes a value of BuilderErrorOr<T>. and produces an expression of
/// type T. If this value is success, then the value of the expression is the
/// wrapped success value. If it's an error, then this expression immediately
/// returns the error from the enclosing function. This works by doing a return
/// from the middle of a statement expression, which is scary, but works. We
/// ultimately end up with something that looks a bit like a Swift `try`
/// expression. Like Swift, we avoid using exceptions to propagate the error.
#pragma clang diagnostic ignored                                               \
    "-Wgnu-statement-expression-from-macro-expansion"
#define ERROR_CHECK(errorOrT)                                                  \
  ({                                                                           \
    auto error_check_tmp = (errorOrT);                                         \
    if (auto *error = error_check_tmp.getError())                              \
      return *error;                                                           \
    *error_check_tmp.getValue();                                               \
  })

/// A generic metadata builder. This is templatized on a ReaderWriter, which
/// abstracts the various operations we need for building generic metadata, such
/// as allocating memory, reading and writing pointers, looking up symbols,
/// looking up type metadata by name, etc.
template <typename ReaderWriter>
class GenericMetadataBuilder {
  using Runtime = typename ReaderWriter::Runtime;

  template <typename T>
  using Pointer = typename ReaderWriter::Runtime::template Pointer<T>;

  template <typename T>
  using Buffer = typename ReaderWriter::template Buffer<T>;

  using Size = typename Runtime::StoredSize;

  template <typename T>
  using WritableData = typename ReaderWriter::template WritableData<T>;

  using StoredPointer = typename Runtime::StoredPointer;

  using StoredSize = typename Runtime::StoredSize;

  using GenericArgument = typename ReaderWriter::GenericArgument;

  // Convenience aliases for a bunch of Swift metadata types we use.
  template <typename T>
  using FullMetadata = swift::FullMetadata<T>;
  template <typename T, template <typename> class U>
  using ConstTargetMetadataPointer = swift::ConstTargetMetadataPointer<T, U>;
  using ClassDescriptor = swift::TargetClassDescriptor<Runtime>;
  using ClassMetadataType = swift::TargetClassMetadataType<Runtime>;
  using EnumDescriptor = swift::TargetEnumDescriptor<Runtime>;
  using EnumMetadata = swift::TargetEnumMetadata<Runtime>;
  using EnumValueWitnessTable = swift::TargetEnumValueWitnessTable<Runtime>;
  using GenericMetadataPattern = swift::TargetGenericMetadataPattern<Runtime>;
  using GenericValueMetadataPattern =
      swift::TargetGenericValueMetadataPattern<Runtime>;
  using Metadata = swift::TargetMetadata<Runtime>;
  using StructDescriptor = swift::TargetStructDescriptor<Runtime>;
  using StructMetadata = swift::TargetStructMetadata<Runtime>;
  using TypeContextDescriptor = swift::TargetTypeContextDescriptor<Runtime>;
  using TypeLayout = swift::TypeLayout;
  using ValueMetadata = swift::TargetValueMetadata<Runtime>;
  using ValueTypeDescriptor = swift::TargetValueTypeDescriptor<Runtime>;
  using ValueWitnessFlags =
      swift::TargetValueWitnessFlags<typename Runtime::StoredSize>;
  using ValueWitnessTable = swift::TargetValueWitnessTable<Runtime>;

  // Start of member variables.
  ReaderWriter &readerWriter;

  // Various functions and witness tables needed from the Swift runtime. These
  // are used to create the witness table for certain kinds of newly constructed
  // metadata. These are stored as BuilderErrorOr because it's not a totally
  // fatal error if we fail to look these up. If one of these symbols can't be
  // found, then we're unable to build metadata that needs it, but we can still
  // build other metadata.
  BuilderErrorOr<Buffer<const char>> pod_copy;
  BuilderErrorOr<Buffer<const char>> pod_destroy;
  BuilderErrorOr<Buffer<const char>>
      pod_direct_initializeBufferWithCopyOfBuffer;
  BuilderErrorOr<Buffer<const char>>
      pod_indirect_initializeBufferWithCopyOfBuffer;
  BuilderErrorOr<Buffer<const ValueWitnessTable>> VWT_Bi8_;
  BuilderErrorOr<Buffer<const ValueWitnessTable>> VWT_Bi16_;
  BuilderErrorOr<Buffer<const ValueWitnessTable>> VWT_Bi32_;
  BuilderErrorOr<Buffer<const ValueWitnessTable>> VWT_Bi64_;
  BuilderErrorOr<Buffer<const ValueWitnessTable>> VWT_Bi128_;
  BuilderErrorOr<Buffer<const ValueWitnessTable>> VWT_Bi256_;
  BuilderErrorOr<Buffer<const ValueWitnessTable>> VWT_Bi512_;

  /// Read the name from a given type descriptor.
  template <typename DescriptorType>
  BuilderErrorOr<const char *>
  getDescriptorName(Buffer<DescriptorType> descriptionBuffer) {
    auto name = ERROR_CHECK(
        descriptionBuffer.resolvePointer(&descriptionBuffer.ptr->Name));
    return name.ptr ? name.ptr : "<unknown>";
  }

  /// Utility function for getting the location of an offset into a given
  /// pointer, when the offset is given in terms of a number of integer words.
  /// This is used with various metadata values which are given in terms of
  /// 32-bit or pointer-sized words from the start of the metadata.
  template <typename Word>
  static Word *wordsOffset(void *from, size_t offset) {
    auto asWords = reinterpret_cast<Word *>(from);
    return asWords + offset;
  }

  /// Const version of wordsOffset.
  template <typename Word>
  static const Word *wordsOffset(const void *from, size_t offset) {
    auto asWords = reinterpret_cast<const Word *>(from);
    return asWords + offset;
  }

public:
  /// A fully constructed metadata, which consists of the data buffer and the
  /// offset to the metadata's address point within it.
  struct ConstructedMetadata {
    WritableData<FullMetadata<Metadata>> data;
    Size offset;
  };

  GenericMetadataBuilder(ReaderWriter &readerWriter)
      : readerWriter(readerWriter),
        pod_copy(readerWriter.getSymbolPointer("_swift_pod_copy")),
        pod_destroy(readerWriter.getSymbolPointer("_swift_pod_destroy")),
        pod_direct_initializeBufferWithCopyOfBuffer(
            readerWriter.getSymbolPointer(
                "_swift_pod_direct_initializeBufferWithCopyOfBuffer")),
        pod_indirect_initializeBufferWithCopyOfBuffer(
            readerWriter.getSymbolPointer(
                "_swift_pod_indirect_initializeBufferWithCopyOfBuffer")),
        VWT_Bi8_(readerWriter.template getSymbolPointer<ValueWitnessTable>(
            MANGLE_AS_STRING(VALUE_WITNESS_SYM(Bi8_)))),
        VWT_Bi16_(readerWriter.template getSymbolPointer<ValueWitnessTable>(
            MANGLE_AS_STRING(VALUE_WITNESS_SYM(Bi16_)))),
        VWT_Bi32_(readerWriter.template getSymbolPointer<ValueWitnessTable>(
            MANGLE_AS_STRING(VALUE_WITNESS_SYM(Bi32_)))),
        VWT_Bi64_(readerWriter.template getSymbolPointer<ValueWitnessTable>(
            MANGLE_AS_STRING(VALUE_WITNESS_SYM(Bi64_)))),
        VWT_Bi128_(readerWriter.template getSymbolPointer<ValueWitnessTable>(
            MANGLE_AS_STRING(VALUE_WITNESS_SYM(Bi128_)))),
        VWT_Bi256_(readerWriter.template getSymbolPointer<ValueWitnessTable>(
            MANGLE_AS_STRING(VALUE_WITNESS_SYM(Bi256_)))),
        VWT_Bi512_(readerWriter.template getSymbolPointer<ValueWitnessTable>(
            MANGLE_AS_STRING(VALUE_WITNESS_SYM(Bi512_)))) {}

  /// Initialize an already-allocated generic value metadata.
  BuilderErrorOr<std::monostate> initializeValueMetadataFromPattern(
      WritableData<FullMetadata<Metadata>> data, Size metadataOffset,
      Buffer<const ValueTypeDescriptor> descriptionBuffer,
      Buffer<const GenericValueMetadataPattern> patternBuffer) {
    const auto *pattern = patternBuffer.ptr;

    char *metadataBase = reinterpret_cast<char *>(data.ptr);
    auto metadata =
        reinterpret_cast<ValueMetadata *>(metadataBase + metadataOffset);
    char *rawMetadata = reinterpret_cast<char *>(metadata);
    auto fullMetadata = asFullMetadata(metadata);

    if (pattern->hasExtraDataPattern()) {
      StoredPointer *metadataExtraData = reinterpret_cast<StoredPointer *>(
          rawMetadata + sizeof(ValueMetadata));
      auto extraDataPattern = pattern->getExtraDataPattern();

      // Zero memory up to the offset.
      // [pre-5.3-extra-data-zeroing] Before Swift 5.3, the runtime did not
      // correctly zero the zero-prefix of the extra-data pattern.
      memset(metadataExtraData, 0,
             size_t(extraDataPattern->OffsetInWords) * sizeof(StoredPointer));

      // Copy the pattern into the rest of the extra data.
      METADATA_BUILDER_LOG(
          "Writing %" PRIu16 " words of extra data from offset %" PRIu16,
          extraDataPattern->SizeInWords, extraDataPattern->OffsetInWords);
      auto patternPointers =
          ERROR_CHECK(patternBuffer.resolvePointer(&extraDataPattern->Pattern));
      for (unsigned i = 0; i < extraDataPattern->SizeInWords; i++) {
        auto patternPointer = ERROR_CHECK(
            patternPointers.resolvePointer(&patternPointers.ptr[i]));
        data.writePointer(
            &metadataExtraData[i + extraDataPattern->OffsetInWords],
            patternPointer.template cast<const StoredPointer>());
      }
    }

    // Put the VWT pattern in place as if it was the real VWT.
    // The various initialization functions will instantiate this as
    // necessary.
    auto valueWitnesses =
        ERROR_CHECK(patternBuffer.resolvePointer(&pattern->ValueWitnesses));
    METADATA_BUILDER_LOG("Setting initial value witnesses");
    data.writePointer(&fullMetadata->ValueWitnesses, valueWitnesses);

    // Set the metadata kind.
    METADATA_BUILDER_LOG("Setting metadata kind %#x",
                         (unsigned)pattern->getMetadataKind());
    metadata->setKind(pattern->getMetadataKind());

    // Set the type descriptor.
    METADATA_BUILDER_LOG("Setting descriptor");
    data.writePointer(&metadata->Description, descriptionBuffer);

    return {{}};
  }

  /// Install the generic arguments in a metadata structure.
  BuilderErrorOr<std::monostate>
  installGenericArguments(WritableData<FullMetadata<Metadata>> data,
                          Size metadataOffset,
                          Buffer<const ValueTypeDescriptor> descriptionBuffer,
                          const GenericArgument *arguments) {
    METADATA_BUILDER_LOG("Building %s",
                         ERROR_CHECK(getDescriptorName(descriptionBuffer)));
    char *metadataBase = reinterpret_cast<char *>(data.ptr);
    auto metadata =
        reinterpret_cast<ValueMetadata *>(metadataBase + metadataOffset);
    const auto &genericContext = *descriptionBuffer.ptr->getGenericContext();
    const auto &header = genericContext.getGenericContextHeader();
    auto dst =
        (reinterpret_cast<Pointer<const Metadata> *>(metadata) +
         getGenericArgumentOffset(
             descriptionBuffer.template cast<const TypeContextDescriptor>()));
    METADATA_BUILDER_LOG(
        "Installing %" PRIu16 " generic arguments at offset %" PRId32,
        header.NumKeyArguments,
        getGenericArgumentOffset(
            descriptionBuffer.template cast<const TypeContextDescriptor>()));
    for (unsigned i = 0; i < header.NumKeyArguments; i++)
      data.writePointer(&dst[i], arguments[i]);

    // TODO: parameter pack support.

    return {{}};
  }

  int32_t getGenericArgumentOffset(
      Buffer<const TypeContextDescriptor> descriptionBuffer) {
    auto description = descriptionBuffer.ptr;
    if (auto enumDescription = llvm::dyn_cast<EnumDescriptor>(description))
      return enumDescription->getGenericArgumentOffset();
    if (auto structDescription = llvm::dyn_cast<StructDescriptor>(description))
      return structDescription->getGenericArgumentOffset();
    if (auto classDescription = llvm::dyn_cast<ClassDescriptor>(description))
      swift_unreachable("Classes not yet supported.");
    swift_unreachable("Not a type context descriptor.");
  }

  /// Allocate and build a metadata structure.
  BuilderErrorOr<ConstructedMetadata>
  buildGenericMetadata(Buffer<const TypeContextDescriptor> descriptionBuffer,
                       const GenericArgument *arguments,
                       Buffer<const GenericMetadataPattern> patternBuffer,
                       size_t extraDataSize) {
    auto description = descriptionBuffer.ptr;

    if (description->hasLayoutString())
      return BuilderError("Types with layout strings are not yet supported");

    if (auto *valueDescription =
            llvm::dyn_cast<ValueTypeDescriptor>(description)) {
      return buildGenericValueMetadata(
          descriptionBuffer.template cast<const ValueTypeDescriptor>(),
          arguments,
          patternBuffer.template cast<const GenericValueMetadataPattern>(),
          extraDataSize);
    }

    return BuilderError(
        "Don't know how to build metadata from descriptor kind %#" PRIx32,
        static_cast<uint32_t>(description->getKind()));
  }

  BuilderErrorOr<ConstructedMetadata> buildGenericValueMetadata(
      Buffer<const ValueTypeDescriptor> descriptionBuffer,
      const GenericArgument *arguments,
      Buffer<const GenericValueMetadataPattern> patternBuffer,
      size_t extraDataSize) {
    auto *pattern = patternBuffer.ptr;
    assert(!pattern->hasExtraDataPattern() ||
           (extraDataSize == (pattern->getExtraDataPattern()->OffsetInWords +
                              pattern->getExtraDataPattern()->SizeInWords) *
                                 sizeof(void *)));

    size_t totalSize = sizeof(FullMetadata<ValueMetadata>) + extraDataSize;
    METADATA_BUILDER_LOG("Extra data size is %zu, allocating %zu bytes total",
                         extraDataSize, totalSize);
    auto metadataBuffer =
        readerWriter.template allocate<FullMetadata<Metadata>>(totalSize);
    auto metadataOffset = sizeof(typename ValueMetadata::HeaderType);

    ERROR_CHECK(initializeValueMetadataFromPattern(
        metadataBuffer, metadataOffset, descriptionBuffer, patternBuffer));

    // Copy the generic arguments into place.
    ERROR_CHECK(installGenericArguments(metadataBuffer, metadataOffset,
                                        descriptionBuffer, arguments));

    return ConstructedMetadata{metadataBuffer,
                               static_cast<Size>(metadataOffset)};
  }

  /// Initialize a generic value metadata structure.
  BuilderErrorOr<std::monostate>
  initializeGenericMetadata(WritableData<FullMetadata<Metadata>> metadataBuffer,
                            swift::Demangle::NodePointer metadataMangleNode) {
    auto *metadata = static_cast<Metadata *>(metadataBuffer.ptr);
    auto *valueMetadata = llvm::dyn_cast<ValueMetadata>(metadata);
    if (!valueMetadata)
      return BuilderError(
          "Don't know how to initialize metadata kind %#" PRIx32,
          static_cast<uint32_t>(metadataBuffer.ptr->getKind()));

    auto descriptionBuffer =
        ERROR_CHECK(metadataBuffer.resolvePointer(&valueMetadata->Description));
    auto patternBuffer = ERROR_CHECK(descriptionBuffer.resolvePointer(
        &descriptionBuffer.ptr->getFullGenericContextHeader()
             .DefaultInstantiationPattern));
    auto completionFunction = ERROR_CHECK(patternBuffer.resolveFunctionPointer(
        &patternBuffer.ptr->CompletionFunction));

    if (completionFunction.isNull()) {
      METADATA_BUILDER_LOG(
          "Type has no completion function, skipping initialization");
      return {{}};
    }

    if (auto structmd = llvm::dyn_cast<StructMetadata>(metadata))
      ERROR_CHECK(initializeStructMetadata(metadataBuffer, structmd,
                                           metadataMangleNode));
    else if (auto enummd = llvm::dyn_cast<EnumMetadata>(metadata))
      ERROR_CHECK(
          initializeEnumMetadata(metadataBuffer, enummd, metadataMangleNode));
    else
      return BuilderError(
          "Don't know how to initialize metadata kind %#" PRIx32,
          static_cast<uint32_t>(metadataBuffer.ptr->getKind()));
    return {{}};
  }

  static constexpr swift::TargetTypeLayout<Runtime>
  getInitialLayoutForValueType() {
    swift::TargetValueWitnessFlags<typename Runtime::StoredSize> flags{};
    flags = flags.withAlignment(1).withPOD(true);
    return {0, 0, flags, 0};
  }

  /// Copy the contents of a given value witness table into a new one.
  BuilderErrorOr<std::monostate>
  copyVWT(WritableData<ValueWitnessTable> vwtBuffer,
          Buffer<const ValueWitnessTable> from) {
#define WANT_ONLY_REQUIRED_VALUE_WITNESSES
#define VALUE_WITNESS(LOWER_ID, UPPER_ID)                                      \
  auto LOWER_ID##_Buffer =                                                     \
      ERROR_CHECK(from.resolveFunctionPointer(&from.ptr->LOWER_ID));           \
  vwtBuffer.writeFunctionPointer(&vwtBuffer.ptr->LOWER_ID, LOWER_ID##_Buffer);
#define DATA_VALUE_WITNESS(LOWER_ID, UPPER_ID, TYPE)
#include "swift/ABI/ValueWitness.def"

    return {{}}; // success
  }

  /// Install common value witness functions for POD and bitwise-takable
  /// metadata.
  BuilderErrorOr<std::monostate>
  installCommonValueWitnesses(const swift::TargetTypeLayout<Runtime> &layout,
                              WritableData<ValueWitnessTable> vwtBuffer) {
    auto flags = layout.flags;
    if (flags.isPOD()) {
      // Use POD value witnesses.
      // If the value has a common size and alignment, use specialized value
      // witnesses we already have lying around for the builtin types.
      bool hasExtraInhabitants = layout.hasExtraInhabitants();
      METADATA_BUILDER_LOG("type isPOD, hasExtraInhabitants=%s layout.size=%zu "
                           "flags.getAlignmentMask=%zu",
                           hasExtraInhabitants ? "true" : "false",
                           (size_t)layout.size,
                           (size_t)flags.getAlignmentMask());
      switch (sizeWithAlignmentMask(layout.size, flags.getAlignmentMask(),
                                    hasExtraInhabitants)) {
      default:
        // For uncommon layouts, use value witnesses that work with an arbitrary
        // size and alignment.
        METADATA_BUILDER_LOG("Uncommon layout case, flags.isInlineStorage=%s",
                             flags.isInlineStorage() ? "true" : "false");
        if (flags.isInlineStorage()) {
          vwtBuffer.writeFunctionPointer(
              &vwtBuffer.ptr->initializeBufferWithCopyOfBuffer,
              ERROR_CHECK(pod_direct_initializeBufferWithCopyOfBuffer));
        } else {
          vwtBuffer.writeFunctionPointer(
              &vwtBuffer.ptr->initializeBufferWithCopyOfBuffer,
              ERROR_CHECK(pod_indirect_initializeBufferWithCopyOfBuffer));
        }
        vwtBuffer.writeFunctionPointer(&vwtBuffer.ptr->destroy,
                                       ERROR_CHECK(pod_destroy));
        vwtBuffer.writeFunctionPointer(&vwtBuffer.ptr->initializeWithCopy,
                                       ERROR_CHECK(pod_copy));
        vwtBuffer.writeFunctionPointer(&vwtBuffer.ptr->initializeWithTake,
                                       ERROR_CHECK(pod_copy));
        vwtBuffer.writeFunctionPointer(&vwtBuffer.ptr->assignWithCopy,
                                       ERROR_CHECK(pod_copy));
        vwtBuffer.writeFunctionPointer(&vwtBuffer.ptr->assignWithTake,
                                       ERROR_CHECK(pod_copy));
        // getEnumTagSinglePayload and storeEnumTagSinglePayload are not
        // interestingly optimizable based on POD-ness.
        return {{}};

      case sizeWithAlignmentMask(1, 0, 0):
        METADATA_BUILDER_LOG("case sizeWithAlignmentMask(1, 0, 0)");
        ERROR_CHECK(copyVWT(vwtBuffer, ERROR_CHECK(VWT_Bi8_)));
        break;
      case sizeWithAlignmentMask(2, 1, 0):
        METADATA_BUILDER_LOG("case sizeWithAlignmentMask(2, 1, 0)");
        ERROR_CHECK(copyVWT(vwtBuffer, ERROR_CHECK(VWT_Bi16_)));
        break;
      case sizeWithAlignmentMask(4, 3, 0):
        METADATA_BUILDER_LOG("case sizeWithAlignmentMask(4, 3, 0)");
        ERROR_CHECK(copyVWT(vwtBuffer, ERROR_CHECK(VWT_Bi32_)));
        break;
      case sizeWithAlignmentMask(8, 7, 0):
        METADATA_BUILDER_LOG("case sizeWithAlignmentMask(8, 7, 0)");
        ERROR_CHECK(copyVWT(vwtBuffer, ERROR_CHECK(VWT_Bi64_)));
        break;
      case sizeWithAlignmentMask(16, 15, 0):
        METADATA_BUILDER_LOG("case sizeWithAlignmentMask(16, 15, 0)");
        ERROR_CHECK(copyVWT(vwtBuffer, ERROR_CHECK(VWT_Bi128_)));
        break;
      case sizeWithAlignmentMask(32, 31, 0):
        METADATA_BUILDER_LOG("case sizeWithAlignmentMask(32, 31, 0)");
        ERROR_CHECK(copyVWT(vwtBuffer, ERROR_CHECK(VWT_Bi256_)));
        break;
      case sizeWithAlignmentMask(64, 63, 0):
        METADATA_BUILDER_LOG("case sizeWithAlignmentMask(64, 63, 0)");
        ERROR_CHECK(copyVWT(vwtBuffer, ERROR_CHECK(VWT_Bi512_)));
        break;
      }

      return {{}};
    }

    if (flags.isBitwiseTakable()) {
      METADATA_BUILDER_LOG(
          "Is bitwise takable, setting pod_copy as initializeWithTake");
      // Use POD value witnesses for operations that do an initializeWithTake.
      vwtBuffer.writeFunctionPointer(&vwtBuffer.ptr->initializeWithTake,
                                     ERROR_CHECK(pod_copy));
    }
    return {{}};
  }

  /// Initialize generic struct metadata.
  BuilderErrorOr<std::monostate>
  initializeStructMetadata(WritableData<FullMetadata<Metadata>> metadataBuffer,
                           StructMetadata *metadata,
                           swift::Demangle::NodePointer metadataMangleNode) {
    METADATA_BUILDER_LOG("Initializing struct");

    auto descriptionBuffer =
        ERROR_CHECK(metadataBuffer.resolvePointer(&metadata->Description));
    auto description =
        reinterpret_cast<const StructDescriptor *>(descriptionBuffer.ptr);

    auto fieldDescriptorBuffer =
        ERROR_CHECK(descriptionBuffer.resolvePointer(&description->Fields));
    auto fieldDescriptor = fieldDescriptorBuffer.ptr;
    auto fields = fieldDescriptor->getFields();
    METADATA_BUILDER_LOG("%zu fields", fields.size());

    auto layout = getInitialLayoutForValueType();
    size_t size = layout.size;
    size_t alignMask = layout.flags.getAlignmentMask();
    bool isPOD = layout.flags.isPOD();
    bool isBitwiseTakable = layout.flags.isBitwiseTakable();

    auto *fieldOffsetsStart = wordsOffset<StoredPointer>(
        metadata, description->FieldOffsetVectorOffset);
    auto *fieldOffsets = reinterpret_cast<uint32_t *>(fieldOffsetsStart);

    // We have extra inhabitants if any element does. Use the field with the
    // most.
    unsigned extraInhabitantCount = 0;

    for (unsigned i = 0; i != fields.size(); ++i) {
      auto &field = fields[i];
      auto nameBuffer =
          ERROR_CHECK(fieldDescriptorBuffer.resolvePointer(&field.FieldName));
      auto mangledTypeNameBuffer = ERROR_CHECK(
          fieldDescriptorBuffer.resolvePointer(&field.MangledTypeName));
      auto mangledTypeName = swift::Demangle::makeSymbolicMangledNameStringRef(
          mangledTypeNameBuffer.ptr);
      METADATA_BUILDER_LOG(
          "Examining field %u '%s' type '%.*s' (mangled name is %zu bytes)", i,
          nameBuffer.ptr, (int)mangledTypeName.size(), mangledTypeName.data(),
          mangledTypeName.size());

      auto fieldTypeBuffer = ERROR_CHECK(readerWriter.getTypeByMangledName(
          metadataBuffer, metadataMangleNode, mangledTypeName));
      auto *fieldType = fieldTypeBuffer.ptr;
      METADATA_BUILDER_LOG("Looked up field type metadata %p", fieldType);

      auto fieldWitnessTableBuffer = ERROR_CHECK(fieldTypeBuffer.resolvePointer(
          &asFullMetadata(fieldType)->ValueWitnesses));
      auto *fieldWitnessTable = fieldWitnessTableBuffer.ptr;
      auto *fieldLayout = fieldWitnessTable->getTypeLayout();
      size = roundUpToAlignMask(size, fieldLayout->flags.getAlignmentMask());

      fieldOffsets[i] = size;

      size += fieldLayout->size;
      alignMask = std::max(alignMask, fieldLayout->flags.getAlignmentMask());
      if (!fieldLayout->flags.isPOD())
        isPOD = false;
      if (!fieldLayout->flags.isBitwiseTakable())
        isBitwiseTakable = false;

      unsigned fieldExtraInhabitantCount =
          fieldWitnessTable->getNumExtraInhabitants();
      if (fieldExtraInhabitantCount > extraInhabitantCount) {
        extraInhabitantCount = fieldExtraInhabitantCount;
      }
    }

    bool isInline =
        ValueWitnessTable::isValueInline(isBitwiseTakable, size, alignMask + 1);

    layout.size = size;
    layout.flags = ValueWitnessFlags()
                       .withAlignmentMask(alignMask)
                       .withPOD(isPOD)
                       .withBitwiseTakable(isBitwiseTakable)
                       .withInlineStorage(isInline);
    layout.extraInhabitantCount = extraInhabitantCount;
    layout.stride = std::max(size_t(1), roundUpToAlignMask(size, alignMask));

    auto oldVWTBuffer = ERROR_CHECK(metadataBuffer.resolvePointer(
        &asFullMetadata(metadata)->ValueWitnesses));
    auto *oldVWT = oldVWTBuffer.ptr;

    if (readerWriter.isLoggingEnabled()) {
      auto info = readerWriter.getSymbolInfo(oldVWTBuffer);
      METADATA_BUILDER_LOG("Initializing new VWT from old VWT %#" PRIx64
                           " - %s (%s + %" PRIu64 ")",
                           oldVWTBuffer.getAddress(), info.symbolName.c_str(),
                           info.libraryName.c_str(), info.pointerOffset);
    }

    auto newVWTData = readerWriter.template allocate<ValueWitnessTable>(
        sizeof(ValueWitnessTable));
    auto *newVWT = newVWTData.ptr;

    // Initialize the new table with the raw contents of the old table first.
    // This will set the data fields.
    new (newVWT) ValueWitnessTable(*oldVWT);

    // Set all the functions separately so they get the right fixups.
#define WANT_ONLY_REQUIRED_VALUE_WITNESSES
#define DATA_VALUE_WITNESS(LOWER_ID, UPPER_ID, TYPE)                           \
  // This macro intentionally left blank.
#define FUNCTION_VALUE_WITNESS(LOWER_ID, UPPER_ID, RETURN_TYPE, PARAM_TYPES)   \
  newVWTData.writeFunctionPointer(                                             \
      &newVWT->LOWER_ID,                                                       \
      ERROR_CHECK(oldVWTBuffer.resolveFunctionPointer(&oldVWT->LOWER_ID)));
#include "swift/ABI/ValueWitness.def"

    ERROR_CHECK(installCommonValueWitnesses(layout, newVWTData));

    newVWT->size = layout.size;
    newVWT->stride = layout.stride;
    newVWT->extraInhabitantCount = layout.extraInhabitantCount;
    newVWT->flags = layout.flags;

    metadataBuffer.writePointer(
        &metadataBuffer.ptr->ValueWitnesses,
        newVWTData.template cast<const ValueWitnessTable>());
    return {{}}; // success
  }

  /// Initialize generic enum metadata.
  BuilderErrorOr<std::monostate>
  initializeEnumMetadata(WritableData<FullMetadata<Metadata>> metadataBuffer,
                         EnumMetadata *metadata,
                         swift::Demangle::NodePointer metadataMangleNode) {
    METADATA_BUILDER_LOG("Initializing enum");
    return BuilderError("Don't know how to initialize enum metadata yet");
  }

  /// Get the extra data size required to allocate a new metadata structure for
  /// the given description and pattern.
  BuilderErrorOr<size_t>
  extraDataSize(Buffer<const TypeContextDescriptor> descriptionBuffer,
                Buffer<const GenericMetadataPattern> patternBuffer) {
    METADATA_BUILDER_LOG("Getting extra data size for %s",
                         ERROR_CHECK(getDescriptorName(descriptionBuffer)));

    auto *pattern = patternBuffer.ptr;

    auto *description = descriptionBuffer.ptr;

    if (auto *valueDescription =
            llvm::dyn_cast<ValueTypeDescriptor>(description)) {
      auto *valuePattern =
          reinterpret_cast<const GenericValueMetadataPattern *>(pattern);
      if (valuePattern->hasExtraDataPattern()) {
        auto extraDataPattern = valuePattern->getExtraDataPattern();
        auto result =
            (extraDataPattern->OffsetInWords + extraDataPattern->SizeInWords) *
            sizeof(void *);
        METADATA_BUILDER_LOG(
            "Value type descriptor has extra data pattern, extra data size: "
            "%zu",
            result);
        return result;
      }

      if (auto structDescription =
              llvm::dyn_cast<StructDescriptor>(description)) {
        if (structDescription->hasFieldOffsetVector()) {
          auto fieldsStart =
              structDescription->FieldOffsetVectorOffset * sizeof(void *);
          auto fieldsEnd =
              fieldsStart + structDescription->NumFields * sizeof(uint32_t);
          auto size = fieldsEnd - sizeof(StructMetadata);
          auto result = roundUpToAlignment(size, sizeof(StoredPointer));
          METADATA_BUILDER_LOG(
              "Struct descriptor has field offset vector, computed extra data "
              "size: %zu",
              result);
          return result;
        } else if (structDescription->isGeneric()) {
          const auto &genericContext = *structDescription->getGenericContext();
          const auto &header = genericContext.getGenericContextHeader();
          auto result = header.NumKeyArguments * sizeof(void *);
          METADATA_BUILDER_LOG(
              "Struct descriptor has no field offset vector, computed extra "
              "data size from generic arguments, extra data size: %zu",
              result);
          return result;
        }
      }

      if (auto enumDescription = llvm::dyn_cast<EnumDescriptor>(description)) {
        if (enumDescription->hasPayloadSizeOffset()) {
          auto offset = enumDescription->getPayloadSizeOffset();
          auto result = offset * sizeof(StoredPointer) - sizeof(EnumMetadata);
          METADATA_BUILDER_LOG(
              "Enum descriptor has payload size offset, computed extra data "
              "size: %zu",
              result);
          return result;
        } else if (enumDescription->isGeneric()) {
          const auto &genericContext = *enumDescription->getGenericContext();
          const auto &header = genericContext.getGenericContextHeader();
          auto result = header.NumKeyArguments * sizeof(void *);
          METADATA_BUILDER_LOG(
              "Enum descriptor has no payload size offset, computed extra data "
              "size from generic arguments, extra data size: %zu",
              result);
          return result;
        }
      }
    }

    return BuilderError(
        "Unable to compute extra data size of descriptor with kind %u",
        static_cast<unsigned>(description->getKind()));
  }

  /// A class that can dump generic metadata structures.
  template <typename Printer>
  class Dumper {
    Printer print;
    ReaderWriter readerWriter;

    template <typename T>
    void printPointer(Buffer<T> buffer) {
      auto info = readerWriter.getSymbolInfo(buffer);
      print("%#" PRIx64 " - %s (%s + %" PRIu64 ")", buffer.getAddress(),
            info.symbolName.c_str(), info.libraryName.c_str(),
            info.pointerOffset);
    }

    template <typename T>
    void printPointer(const char *prefix, Buffer<T> buffer,
                      const char *suffix = "\n") {
      print("%s", prefix);
      printPointer(buffer);
      print("%s", suffix);
    }

  public:
    Dumper(Printer print) : print(print) {}

    BuilderErrorOr<std::monostate>
    dumpMetadata(Buffer<const Metadata> metadataBuffer) {
      printPointer("Metadata ", metadataBuffer);

      auto fullMetadata = asFullMetadata(metadataBuffer.ptr);

      auto valueWitnesses = ERROR_CHECK(
          metadataBuffer.resolvePointer(&fullMetadata->ValueWitnesses));
      printPointer("  value witnesses: ", valueWitnesses);
      ERROR_CHECK(dumpVWT(valueWitnesses));

      auto kind = fullMetadata->getKind();
      auto kindString = getStringForMetadataKind(kind);
      print("  kind: %#" PRIx32 " (%s)\n", static_cast<uint32_t>(kind),
            kindString.str().c_str());

      if (auto classmd = llvm::dyn_cast<ClassMetadataType>(metadataBuffer.ptr))
        ERROR_CHECK(dumpClassMetadata(metadataBuffer, classmd));
      else if (auto valuemd = llvm::dyn_cast<ValueMetadata>(metadataBuffer.ptr))
        ERROR_CHECK(dumpValueMetadata(metadataBuffer, valuemd));

      return {{}};
    }

    BuilderErrorOr<std::monostate>
    dumpClassMetadata(Buffer<const Metadata> metadataBuffer,
                      const ClassMetadataType *metadata) {
      return BuilderError("Class dumping is not yet implemented");
    }

    BuilderErrorOr<std::monostate>
    dumpValueMetadata(Buffer<const Metadata> metadataBuffer,
                      const ValueMetadata *metadata) {
      auto descriptionBuffer =
          ERROR_CHECK(metadataBuffer.resolvePointer(&metadata->Description));
      auto description = descriptionBuffer.ptr;
      printPointer("  description: ", descriptionBuffer);

      if (description->hasLayoutString()) {
        auto layoutStringBuffer = ERROR_CHECK(metadataBuffer.resolvePointer(
            &asFullMetadata(metadata)->layoutString));
        printPointer("  layout string: ", layoutStringBuffer);
      }

      auto name =
          ERROR_CHECK(descriptionBuffer.resolvePointer(&description->Name));
      printPointer("  name: ", name);
      print("        \"%s\"\n", name.ptr);

      if (auto structmd = llvm::dyn_cast<StructMetadata>(metadataBuffer.ptr))
        ERROR_CHECK(dumpStructMetadata(metadataBuffer, structmd));
      else if (auto enummd = llvm::dyn_cast<EnumMetadata>(metadataBuffer.ptr))
        ERROR_CHECK(dumpEnumMetadata(metadataBuffer, enummd));

      if (description->isGeneric()) {
        auto numGenericParams =
            description->getGenericContextHeader().NumParams;
        auto genericArguments = wordsOffset<
            ConstTargetMetadataPointer<Runtime, swift::TargetMetadata>>(
            metadata, description->getGenericArgumentOffset());
        for (unsigned i = 0; i < numGenericParams; i++) {
          auto arg =
              ERROR_CHECK(metadataBuffer.resolvePointer(&genericArguments[i]));
          print("  genericArg[%u]: ", i);
          printPointer(arg);
          print("\n");
        }
      }

      return {{}};
    }

    BuilderErrorOr<std::monostate>
    dumpStructMetadata(Buffer<const Metadata> metadataBuffer,
                       const StructMetadata *metadata) {
      auto descriptionBuffer =
          ERROR_CHECK(metadataBuffer.resolvePointer(&metadata->Description));
      auto structDescription =
          reinterpret_cast<const StructDescriptor *>(descriptionBuffer.ptr);
      if (structDescription->hasFieldOffsetVector()) {
        auto *offsetsStart = wordsOffset<StoredPointer>(
            metadata, structDescription->FieldOffsetVectorOffset);
        auto *offsets = reinterpret_cast<const uint32_t *>(offsetsStart);
        for (unsigned i = 0; i < structDescription->NumFields; i++)
          print("  fieldOffset[%u]: %" PRIu32 "\n", i, offsets[i]);
      }
      return {{}};
    }

    BuilderErrorOr<std::monostate>
    dumpEnumMetadata(Buffer<const Metadata> metadataBuffer,
                     const EnumMetadata *metadata) {
      auto descriptionBuffer =
          ERROR_CHECK(metadataBuffer.resolvePointer(&metadata->Description));
      auto description =
          reinterpret_cast<const EnumDescriptor *>(descriptionBuffer.ptr);

      if (description->hasPayloadSizeOffset()) {
        auto payloadSizeOffset = description->getPayloadSizeOffset();
        print("  offset: %u\n", payloadSizeOffset);
        auto *payloadSizePtr =
            wordsOffset<StoredSize *>(metadata, payloadSizeOffset);
        print("  payload size: %" PRIu64 "\n", (uint64_t)*payloadSizePtr);
      }

      return {{}};
    }

    BuilderErrorOr<std::monostate>
    dumpVWT(Buffer<const ValueWitnessTable> vwtBuffer) {
      auto *vwt = vwtBuffer.ptr;

#define WANT_ONLY_REQUIRED_VALUE_WITNESSES
#define DATA_VALUE_WITNESS(LOWER_ID, UPPER_ID, TYPE)                           \
  ERROR_CHECK(dumpVWTDataField(#LOWER_ID, vwt->LOWER_ID));
#define FUNCTION_VALUE_WITNESS(LOWER_ID, UPPER_ID, RETURN_TYPE, PARAM_TYPES)   \
  ERROR_CHECK(dumpVWTFunctionField(vwtBuffer, #LOWER_ID, &vwt->LOWER_ID));
#include "swift/ABI/ValueWitness.def"

      if (auto *enumVWT = llvm::dyn_cast<EnumValueWitnessTable>(vwt)) {
#define WANT_ONLY_ENUM_VALUE_WITNESSES
#define DATA_VALUE_WITNESS(LOWER_ID, UPPER_ID, TYPE)                           \
  ERROR_CHECK(dumpVWTDataField(#LOWER_ID, enumVWT->LOWER_ID));
#define FUNCTION_VALUE_WITNESS(LOWER_ID, UPPER_ID, RETURN_TYPE, PARAM_TYPES)   \
  ERROR_CHECK(dumpVWTFunctionField(vwtBuffer, #LOWER_ID, &enumVWT->LOWER_ID));
#include "swift/ABI/ValueWitness.def"
      }

      return {{}};
    }

    BuilderErrorOr<std::monostate> dumpVWTDataField(const char *name,
                                                    uint64_t value) {
      print("    %s: %#" PRIx64 " (%" PRIu64 ")\n", name, value, value);
      return {{}};
    }

    BuilderErrorOr<std::monostate> dumpVWTDataField(const char *name,
                                                    ValueWitnessFlags value) {
      return dumpVWTDataField(name, value.getOpaqueValue());
    }

    template <typename T>
    BuilderErrorOr<std::monostate>
    dumpVWTFunctionField(Buffer<const ValueWitnessTable> vwtBuffer,
                         const char *name, T *ptr) {
      auto function = ERROR_CHECK(vwtBuffer.resolveFunctionPointer(ptr));
      print("    %s: ", name);
      printPointer(function);
      print("\n");

      return {{}};
    }
  };

  template <typename Printer>
  Dumper(Printer) -> Dumper<Printer>;
};

template <typename ReaderWriter>
GenericMetadataBuilder(ReaderWriter) -> GenericMetadataBuilder<ReaderWriter>;

} // namespace swift

#endif // SWIFT_RUNTIME_GENERIC_METADATA_BUILDER_H
