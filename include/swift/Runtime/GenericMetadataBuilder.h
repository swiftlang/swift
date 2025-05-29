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

// Use __FILE_NAME__ for logs when it's available, __FILE__ as a fallback.
#ifdef __FILE_NAME__
#define METADATA_BUILDER_LOG_FILE_NAME __FILE_NAME__
#else
#define METADATA_BUILDER_LOG_FILE_NAME __FILE__
#endif

#define METADATA_BUILDER_LOG(...)                                              \
  readerWriter.log(METADATA_BUILDER_LOG_FILE_NAME, __LINE__, __func__,         \
                   __VA_ARGS__)

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
  SWIFT_FORMAT(2, 3) BuilderError(const char *fmt, ...) {
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

  /// Allow manipulating the value with ->. `this` must not contain an error.
  T *operator->() {
    T *ptr = getValue();
    assert(ptr);
    return ptr;
  }

  /// Get the value using *. `this` must not contain an error.
  T &operator*() {
    T *ptr = getValue();
    assert(ptr);
    return *ptr;
  }

  /// Objects are truthy if they contain a value, falsy for errors.
  operator bool() { return getValue() != nullptr; }
};

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
    auto name = descriptionBuffer.resolvePointer(&descriptionBuffer.ptr->Name);
    if (!name)
      return *name.getError();
    return name->ptr ? name->ptr : "<unknown>";
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
          patternBuffer.resolvePointer(&extraDataPattern->Pattern);
      if (!patternPointers)
        return *patternPointers.getError();
      for (unsigned i = 0; i < extraDataPattern->SizeInWords; i++) {
        auto patternPointer =
            patternPointers->resolvePointer(&patternPointers->ptr[i]);
        if (!patternPointer)
          return *patternPointer.getError();
        auto writeResult = data.writePointer(
            &metadataExtraData[i + extraDataPattern->OffsetInWords],
            patternPointer->template cast<const StoredPointer>());
        if (!writeResult)
          return *writeResult.getError();
      }
    }

    // Put the VWT pattern in place as if it was the real VWT.
    // The various initialization functions will instantiate this as
    // necessary.
    auto valueWitnesses =
        patternBuffer.resolvePointer(&pattern->ValueWitnesses);
    if (!valueWitnesses)
      return *valueWitnesses.getError();
    METADATA_BUILDER_LOG("Setting initial value witnesses");
    auto writeResult =
        data.writePointer(&fullMetadata->ValueWitnesses, *valueWitnesses);
    if (!writeResult)
      return *writeResult.getError();

    // Set the metadata kind.
    METADATA_BUILDER_LOG("Setting metadata kind %#x",
                         (unsigned)pattern->getMetadataKind());
    metadata->setKind(pattern->getMetadataKind());

    // Set the type descriptor.
    METADATA_BUILDER_LOG("Setting descriptor");
    writeResult = data.writePointer(&metadata->Description, descriptionBuffer);
    if (!writeResult)
      return *writeResult.getError();

    return {{}};
  }

  /// Install the generic arguments in a metadata structure.
  BuilderErrorOr<std::monostate>
  installGenericArguments(WritableData<FullMetadata<Metadata>> data,
                          Size metadataOffset,
                          Buffer<const ValueTypeDescriptor> descriptionBuffer,
                          llvm::ArrayRef<GenericArgument> arguments) {
    auto name = getDescriptorName(descriptionBuffer);
    if (!name)
      return *name.getError();
    METADATA_BUILDER_LOG("Building %s", *name);
    char *metadataBase = reinterpret_cast<char *>(data.ptr);
    auto metadata =
        reinterpret_cast<ValueMetadata *>(metadataBase + metadataOffset);
    const auto &genericContext = *descriptionBuffer.ptr->getGenericContext();
    const auto &header = genericContext.getGenericContextHeader();
    auto dst =
        (reinterpret_cast<Pointer<const Metadata> *>(metadata) +
         getGenericArgumentOffset(
             descriptionBuffer.template cast<const TypeContextDescriptor>()));

    if (arguments.size() < header.NumKeyArguments) {
      return BuilderError("Not enough generic arguments, %zu provided, %" PRId32 " required", arguments.size(), header.NumKeyArguments);
    }

    METADATA_BUILDER_LOG(
        "Installing %" PRIu16 " generic arguments at offset %" PRId32,
        header.NumKeyArguments,
        getGenericArgumentOffset(
            descriptionBuffer.template cast<const TypeContextDescriptor>()));
    for (unsigned i = 0; i < header.NumKeyArguments; i++) {
      auto writeResult = data.writePointer(&dst[i], arguments[i]);
      if (!writeResult)
        return *writeResult.getError();
    }

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
                       llvm::ArrayRef<GenericArgument> arguments,
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
      llvm::ArrayRef<GenericArgument> arguments,
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

    auto initializeResult = initializeValueMetadataFromPattern(
        metadataBuffer, metadataOffset, descriptionBuffer, patternBuffer);
    if (!initializeResult)
      return *initializeResult.getError();

    // Copy the generic arguments into place.
    auto installResult = installGenericArguments(metadataBuffer, metadataOffset,
                                                 descriptionBuffer, arguments);
    if (!installResult)
      return *installResult.getError();

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
        metadataBuffer.resolvePointer(&valueMetadata->Description);
    if (!descriptionBuffer)
      return *descriptionBuffer.getError();
    auto patternBuffer = descriptionBuffer->resolvePointer(
        &descriptionBuffer->ptr->getFullGenericContextHeader()
             .DefaultInstantiationPattern);
    if (!patternBuffer)
      return *patternBuffer.getError();
    auto completionFunction = patternBuffer->resolveFunctionPointer(
        &patternBuffer->ptr->CompletionFunction);
    if (!completionFunction)
      return *completionFunction.getError();

    if (completionFunction->isNull()) {
      METADATA_BUILDER_LOG(
          "Type has no completion function, skipping initialization");
      return {{}};
    }

    if (auto structmd = llvm::dyn_cast<StructMetadata>(metadata)) {
      auto result = initializeStructMetadata(metadataBuffer, structmd,
                                             metadataMangleNode);
      if (!result)
        return *result.getError();
    } else if (auto enummd = llvm::dyn_cast<EnumMetadata>(metadata)) {
      auto result =
          initializeEnumMetadata(metadataBuffer, enummd, metadataMangleNode);
      if (!result)
        return *result.getError();
    } else
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
  auto LOWER_ID##_Buffer = from.resolveFunctionPointer(&from.ptr->LOWER_ID);   \
  if (!LOWER_ID##_Buffer)                                                      \
    return *LOWER_ID##_Buffer.getError();                                      \
  if (auto *error = vwtBuffer                                                  \
                        .writeFunctionPointer(&vwtBuffer.ptr->LOWER_ID,        \
                                              *LOWER_ID##_Buffer)              \
                        .getError())                                           \
    return *error;
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
      default: {
        // For uncommon layouts, use value witnesses that work with an arbitrary
        // size and alignment.
        METADATA_BUILDER_LOG("Uncommon layout case, flags.isInlineStorage=%s",
                             flags.isInlineStorage() ? "true" : "false");
        if (flags.isInlineStorage()) {
          if (!pod_direct_initializeBufferWithCopyOfBuffer)
            return *pod_direct_initializeBufferWithCopyOfBuffer.getError();
          auto writeResult = vwtBuffer.writeFunctionPointer(
              &vwtBuffer.ptr->initializeBufferWithCopyOfBuffer,
              *pod_direct_initializeBufferWithCopyOfBuffer);
          if (!writeResult)
            return *writeResult.getError();
        } else {
          if (!pod_indirect_initializeBufferWithCopyOfBuffer)
            return *pod_indirect_initializeBufferWithCopyOfBuffer.getError();
          auto writeResult = vwtBuffer.writeFunctionPointer(
              &vwtBuffer.ptr->initializeBufferWithCopyOfBuffer,
              *pod_indirect_initializeBufferWithCopyOfBuffer);
          if (!writeResult)
            return *writeResult.getError();
        }
        if (!pod_destroy)
          return *pod_destroy.getError();
        if (!pod_copy)
          return *pod_copy.getError();
        auto writeResult = vwtBuffer.writeFunctionPointer(
            &vwtBuffer.ptr->destroy, *pod_destroy);
        if (!writeResult)
          return *writeResult.getError();
        writeResult = vwtBuffer.writeFunctionPointer(
            &vwtBuffer.ptr->initializeWithCopy, *pod_copy);
        if (!writeResult)
          return *writeResult.getError();
        writeResult = vwtBuffer.writeFunctionPointer(
            &vwtBuffer.ptr->initializeWithTake, *pod_copy);
        if (!writeResult)
          return *writeResult.getError();
        writeResult = vwtBuffer.writeFunctionPointer(
            &vwtBuffer.ptr->assignWithCopy, *pod_copy);
        if (!writeResult)
          return *writeResult.getError();
        writeResult = vwtBuffer.writeFunctionPointer(
            &vwtBuffer.ptr->assignWithTake, *pod_copy);
        if (!writeResult)
          return *writeResult.getError();
        // getEnumTagSinglePayload and storeEnumTagSinglePayload are not
        // interestingly optimizable based on POD-ness.
        return {{}};
      }

      case sizeWithAlignmentMask(1, 0, 0): {
        METADATA_BUILDER_LOG("case sizeWithAlignmentMask(1, 0, 0)");
        if (!VWT_Bi8_)
          return *VWT_Bi8_.getError();
        auto result = copyVWT(vwtBuffer, *VWT_Bi8_);
        if (!result)
          return *result.getError();
        break;
      }
      case sizeWithAlignmentMask(2, 1, 0): {
        if (!VWT_Bi16_)
          return *VWT_Bi16_.getError();
        METADATA_BUILDER_LOG("case sizeWithAlignmentMask(2, 1, 0)");
        auto result = copyVWT(vwtBuffer, *VWT_Bi16_);
        if (!result)
          return *result.getError();
        break;
      }
      case sizeWithAlignmentMask(4, 3, 0): {
        if (!VWT_Bi32_)
          return *VWT_Bi32_.getError();
        METADATA_BUILDER_LOG("case sizeWithAlignmentMask(4, 3, 0)");
        auto result = copyVWT(vwtBuffer, *VWT_Bi32_);
        if (!result)
          return *result.getError();
        break;
      }
      case sizeWithAlignmentMask(8, 7, 0): {
        if (!VWT_Bi64_)
          return *VWT_Bi64_.getError();
        METADATA_BUILDER_LOG("case sizeWithAlignmentMask(8, 7, 0)");
        auto result = copyVWT(vwtBuffer, *VWT_Bi64_);
        if (!result)
          return *result.getError();
        break;
      }
      case sizeWithAlignmentMask(16, 15, 0): {
        if (!VWT_Bi128_)
          return *VWT_Bi128_.getError();
        METADATA_BUILDER_LOG("case sizeWithAlignmentMask(16, 15, 0)");
        auto result = copyVWT(vwtBuffer, *VWT_Bi128_);
        if (!result)
          return *result.getError();
        break;
      }
      case sizeWithAlignmentMask(32, 31, 0): {
        if (!VWT_Bi256_)
          return *VWT_Bi256_.getError();
        METADATA_BUILDER_LOG("case sizeWithAlignmentMask(32, 31, 0)");
        auto result = copyVWT(vwtBuffer, *VWT_Bi256_);
        if (!result)
          return *result.getError();
        break;
      }
      case sizeWithAlignmentMask(64, 63, 0): {
        if (!VWT_Bi512_)
          return *VWT_Bi512_.getError();
        METADATA_BUILDER_LOG("case sizeWithAlignmentMask(64, 63, 0)");
        auto result = copyVWT(vwtBuffer, *VWT_Bi512_);
        if (!result)
          return *result.getError();
        break;
      }
      }

      return {{}};
    }

    if (flags.isBitwiseTakable()) {
      METADATA_BUILDER_LOG(
          "Is bitwise takable, setting pod_copy as initializeWithTake");
      // Use POD value witnesses for operations that do an initializeWithTake.
      if (!pod_copy)
        return *pod_copy.getError();
      auto writeResult = vwtBuffer.writeFunctionPointer(
          &vwtBuffer.ptr->initializeWithTake, *pod_copy);
      if (!writeResult)
        return *writeResult.getError();
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
        metadataBuffer.resolvePointer(&metadata->Description);
    if (!descriptionBuffer)
      return *descriptionBuffer.getError();
    auto description =
        reinterpret_cast<const StructDescriptor *>(descriptionBuffer->ptr);

    auto fieldDescriptorBuffer =
        descriptionBuffer->resolvePointer(&description->Fields);
    if (!fieldDescriptorBuffer)
      return *fieldDescriptorBuffer.getError();
    auto fieldDescriptor = fieldDescriptorBuffer->ptr;
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
      auto nameBuffer = fieldDescriptorBuffer->resolvePointer(&field.FieldName);
      if (!nameBuffer)
        return *nameBuffer.getError();
      auto mangledTypeNameBuffer =
          fieldDescriptorBuffer->resolvePointer(&field.MangledTypeName);
      if (!mangledTypeNameBuffer)
        return *mangledTypeNameBuffer.getError();
      auto mangledTypeName = swift::Demangle::makeSymbolicMangledNameStringRef(
          mangledTypeNameBuffer->ptr);
      METADATA_BUILDER_LOG(
          "Examining field %u '%s' type '%.*s' (mangled name is %zu bytes)", i,
          nameBuffer->ptr, (int)mangledTypeName.size(), mangledTypeName.data(),
          mangledTypeName.size());

      auto fieldTypeBuffer = readerWriter.getTypeByMangledName(
          metadataBuffer, metadataMangleNode, mangledTypeName);
      if (!fieldTypeBuffer)
        return *fieldTypeBuffer.getError();
      auto *fieldType = fieldTypeBuffer->ptr;
      METADATA_BUILDER_LOG("Looked up field type metadata %p", fieldType);

      auto fieldWitnessTableBuffer = fieldTypeBuffer->resolvePointer(
          &asFullMetadata(fieldType)->ValueWitnesses);
      if (!fieldWitnessTableBuffer)
        return *fieldWitnessTableBuffer.getError();
      auto *fieldWitnessTable = fieldWitnessTableBuffer->ptr;
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

    auto oldVWTBuffer = metadataBuffer.resolvePointer(
        &asFullMetadata(metadata)->ValueWitnesses);
    if (!oldVWTBuffer)
      return *oldVWTBuffer.getError();
    auto *oldVWT = oldVWTBuffer->ptr;

    if (readerWriter.isLoggingEnabled()) {
      auto info = readerWriter.getSymbolInfo(*oldVWTBuffer);
      METADATA_BUILDER_LOG("Initializing new VWT from old VWT %#" PRIx64
                           " - %s (%s + %" PRIu64 ")",
                           oldVWTBuffer->getAddress(), info.symbolName.c_str(),
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
  {                                                                            \
    auto fptr = oldVWTBuffer->resolveFunctionPointer(&oldVWT->LOWER_ID);       \
    if (!fptr)                                                                 \
      return *fptr.getError();                                                 \
    if (auto *error =                                                          \
            newVWTData.writeFunctionPointer(&newVWT->LOWER_ID, *fptr)          \
                .getError())                                                   \
      return *error;                                                           \
  }
#include "swift/ABI/ValueWitness.def"

    auto installResult = installCommonValueWitnesses(layout, newVWTData);
    if (!installResult)
      return *installResult.getError();

    newVWT->size = layout.size;
    newVWT->stride = layout.stride;
    newVWT->extraInhabitantCount = layout.extraInhabitantCount;
    newVWT->flags = layout.flags;

    auto writeResult = metadataBuffer.writePointer(
        &metadataBuffer.ptr->ValueWitnesses,
        newVWTData.template cast<const ValueWitnessTable>());
    if (!writeResult)
      return *writeResult.getError();
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
    auto name = getDescriptorName(descriptionBuffer);
    METADATA_BUILDER_LOG("Getting extra data size for %s", *name);

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

      auto valueWitnesses =
          metadataBuffer.resolvePointer(&fullMetadata->ValueWitnesses);
      if (!valueWitnesses)
        return *valueWitnesses.getError();
      printPointer("  value witnesses: ", *valueWitnesses);
      auto dumpResult = dumpVWT(*valueWitnesses);
      if (!dumpResult)
        return *dumpResult.getError();

      auto kind = fullMetadata->getKind();
      auto kindString = getStringForMetadataKind(kind);
      print("  kind: %#" PRIx32 " (%s)\n", static_cast<uint32_t>(kind),
            kindString.str().c_str());

      if (auto classmd =
              llvm::dyn_cast<ClassMetadataType>(metadataBuffer.ptr)) {
        auto dumpResult = dumpClassMetadata(metadataBuffer, classmd);
        if (!dumpResult)
          return *dumpResult.getError();
      } else if (auto valuemd =
                     llvm::dyn_cast<ValueMetadata>(metadataBuffer.ptr)) {
        auto dumpResult = dumpValueMetadata(metadataBuffer, valuemd);
        if (!dumpResult)
          return *dumpResult.getError();
      }

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
          metadataBuffer.resolvePointer(&metadata->Description);
      if (!descriptionBuffer)
        return *descriptionBuffer.getError();
      auto description = descriptionBuffer->ptr;
      printPointer("  description: ", *descriptionBuffer);

      if (description->hasLayoutString()) {
        auto layoutStringBuffer = metadataBuffer.resolvePointer(
            &asFullMetadata(metadata)->layoutString);
        if (!layoutStringBuffer)
          return *layoutStringBuffer.getError();
        printPointer("  layout string: ", *layoutStringBuffer);
      }

      auto name = descriptionBuffer->resolvePointer(&description->Name);
      if (!name)
        return *name.getError();
      printPointer("  name: ", *name);
      print("        \"%s\"\n", name->ptr);

      if (auto structmd = llvm::dyn_cast<StructMetadata>(metadataBuffer.ptr)) {
        auto dumpResult = dumpStructMetadata(metadataBuffer, structmd);
        if (!dumpResult)
          return *dumpResult.getError();
      } else if (auto enummd =
                     llvm::dyn_cast<EnumMetadata>(metadataBuffer.ptr)) {
        auto dumpResult = dumpEnumMetadata(metadataBuffer, enummd);
        if (!dumpResult)
          return *dumpResult.getError();
      }

      if (description->isGeneric()) {
        auto numGenericParams =
            description->getGenericContextHeader().NumParams;
        auto genericArguments = wordsOffset<
            ConstTargetMetadataPointer<Runtime, swift::TargetMetadata>>(
            metadata, description->getGenericArgumentOffset());
        for (unsigned i = 0; i < numGenericParams; i++) {
          auto arg = metadataBuffer.resolvePointer(&genericArguments[i]);
          if (!arg)
            return *arg.getError();
          print("  genericArg[%u]: ", i);
          printPointer(*arg);
          print("\n");
        }
      }

      return {{}};
    }

    BuilderErrorOr<std::monostate>
    dumpStructMetadata(Buffer<const Metadata> metadataBuffer,
                       const StructMetadata *metadata) {
      auto descriptionBuffer =
          metadataBuffer.resolvePointer(&metadata->Description);
      if (!descriptionBuffer)
        return *descriptionBuffer.getError();
      auto structDescription =
          reinterpret_cast<const StructDescriptor *>(descriptionBuffer->ptr);
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
          metadataBuffer.resolvePointer(&metadata->Description);
      if (!descriptionBuffer)
        return *descriptionBuffer.getError();
      auto description =
          reinterpret_cast<const EnumDescriptor *>(descriptionBuffer->ptr);

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
  {                                                                            \
    auto dumpResult = dumpVWTDataField(#LOWER_ID, vwt->LOWER_ID);              \
    if (!dumpResult)                                                           \
      return *dumpResult.getError();                                           \
  }

#define FUNCTION_VALUE_WITNESS(LOWER_ID, UPPER_ID, RETURN_TYPE, PARAM_TYPES)   \
  {                                                                            \
    auto dumpResult =                                                          \
        dumpVWTFunctionField(vwtBuffer, #LOWER_ID, &vwt->LOWER_ID);            \
    if (!dumpResult)                                                           \
      return *dumpResult.getError();                                           \
  }
#include "swift/ABI/ValueWitness.def"

      if (auto *enumVWT = llvm::dyn_cast<EnumValueWitnessTable>(vwt)) {
#define WANT_ONLY_ENUM_VALUE_WITNESSES
#define DATA_VALUE_WITNESS(LOWER_ID, UPPER_ID, TYPE)                           \
  {                                                                            \
    auto dumpResult = dumpVWTDataField(#LOWER_ID, enumVWT->LOWER_ID);          \
    if (!dumpResult)                                                           \
      return *dumpResult.getError();                                           \
  }
#define FUNCTION_VALUE_WITNESS(LOWER_ID, UPPER_ID, RETURN_TYPE, PARAM_TYPES)   \
  {                                                                            \
    auto dumpResult =                                                          \
        dumpVWTFunctionField(vwtBuffer, #LOWER_ID, &enumVWT->LOWER_ID);        \
    if (!dumpResult)                                                           \
      return *dumpResult.getError();                                           \
  }
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
      auto function = vwtBuffer.resolveFunctionPointer(ptr);
      if (!function)
        return *function.getError();
      print("    %s: ", name);
      printPointer(*function);
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
