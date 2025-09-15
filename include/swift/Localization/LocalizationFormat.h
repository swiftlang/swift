//===--- LocalizationFormat.h - Format for Diagnostic Messages --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the format for localized diagnostic messages.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_LOCALIZATIONFORMAT_H
#define SWIFT_LOCALIZATIONFORMAT_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Bitstream/BitstreamReader.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/OnDiskHashTable.h"
#include "llvm/Support/StringSaver.h"
#include "llvm/Support/raw_ostream.h"
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

namespace swift {
enum class DiagID : uint32_t;

namespace diag {

enum LocalizationProducerState : uint8_t {
  NotInitialized,
  Initialized,
  FailedInitialization
};

class DefToStringsConverter {
  llvm::ArrayRef<const char *> IDs;
  llvm::ArrayRef<const char *> Messages;

public:
  DefToStringsConverter(llvm::ArrayRef<const char *> ids,
                        llvm::ArrayRef<const char *> messages)
    : IDs(ids), Messages(messages) {
    assert(IDs.size() == Messages.size());
  }

  void convert(llvm::raw_ostream &out);
};

class LocalizationWriterInfo {
public:
  using key_type = uint32_t;
  using key_type_ref = const uint32_t &;
  using data_type = std::string;
  using data_type_ref = llvm::StringRef;
  using hash_value_type = uint32_t;
  using offset_type = uint32_t;

  hash_value_type ComputeHash(key_type_ref key) { return key; }

  std::pair<offset_type, offset_type> EmitKeyDataLength(llvm::raw_ostream &out,
                                                        key_type_ref key,
                                                        data_type_ref data) {
    offset_type dataLength = static_cast<offset_type>(data.size());
    llvm::support::endian::write<offset_type>(out, dataLength,
                                              llvm::endianness::little);
    // No need to write the key length; it's constant.
    return {sizeof(key_type), dataLength};
  }

  void EmitKey(llvm::raw_ostream &out, key_type_ref key, unsigned len) {
    assert(len == sizeof(key_type));
    llvm::support::endian::write<key_type>(out, key, llvm::endianness::little);
  }

  void EmitData(llvm::raw_ostream &out, key_type_ref key, data_type_ref data,
                unsigned len) {
    out << data;
  }
};

class LocalizationReaderInfo {
public:
  using internal_key_type = uint32_t;
  using external_key_type = swift::DiagID;
  using data_type = llvm::StringRef;
  using hash_value_type = uint32_t;
  using offset_type = uint32_t;

  internal_key_type GetInternalKey(external_key_type key) {
    return static_cast<internal_key_type>(key);
  }

  external_key_type GetExternalKey(internal_key_type key) {
    return static_cast<external_key_type>(key);
  }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  hash_value_type ComputeHash(internal_key_type key) { return key; }

  static std::pair<offset_type, offset_type>
  ReadKeyDataLength(const unsigned char *&data) {
    offset_type dataLength =
        llvm::support::endian::readNext<offset_type, llvm::endianness::little,
                                        llvm::support::unaligned>(data);
    return {sizeof(uint32_t), dataLength};
  }

  internal_key_type ReadKey(const unsigned char *data, offset_type length) {
    return llvm::support::endian::readNext<
        internal_key_type, llvm::endianness::little, llvm::support::unaligned>(
        data);
  }

  data_type ReadData(internal_key_type Key, const unsigned char *data,
                     offset_type length) {
    return data_type((const char *)data, length);
  }
};

class SerializedLocalizationWriter {
  using offset_type = LocalizationWriterInfo::offset_type;
  llvm::OnDiskChainedHashTableGenerator<LocalizationWriterInfo> generator;

public:
  /// Enqueue the given diagnostic to be included in a serialized translations
  /// file.
  ///
  /// \param id The identifier associated with the given diagnostic message e.g.
  ///           'cannot_convert_argument'.
  /// \param translation The localized diagnostic message for the given
  ///                    identifier.
  void insert(swift::DiagID id, llvm::StringRef translation);

  /// Write out previously inserted diagnostic translations into the given
  /// location.
  ///
  /// \param filePath The location of the serialized diagnostics file. It's
  /// supposed to be a file with '.db' postfix.
  /// \returns true if all diagnostic
  /// messages have been successfully serialized, false otherwise.
  bool emit(llvm::StringRef filePath);
};

class LocalizationProducer {
  LocalizationProducerState state = NotInitialized;

public:
  /// If the  message isn't available/localized in current context
  /// return the fallback default message.
  virtual llvm::StringRef getMessageOr(swift::DiagID id,
                                       llvm::StringRef defaultMessage);

  /// \returns a `SerializedLocalizationProducer` pointer if the serialized
  /// diagnostics file available, otherwise returns a
  /// `StringsLocalizationProducer` if the `.strings` file is available. If both
  /// files aren't available returns a `nullptr`.
  static std::unique_ptr<LocalizationProducer>
  producerFor(llvm::StringRef locale, llvm::StringRef path);

  virtual ~LocalizationProducer() {}

protected:
  LocalizationProducerState getState() const;

  /// Used to lazily initialize `LocalizationProducer`s.
  /// \returns true if the producer is successfully initialized, false
  /// otherwise.
  virtual bool initializeImpl() = 0;
  virtual void initializeIfNeeded() final;

  /// Retrieve a message for the given diagnostic id.
  /// \returns empty string if message couldn't be found.
  virtual llvm::StringRef getMessage(swift::DiagID id) const = 0;
};

class StringsLocalizationProducer final : public LocalizationProducer {
  std::string filePath;

  std::vector<std::string> diagnostics;

public:
  explicit StringsLocalizationProducer(llvm::StringRef filePath)
      : LocalizationProducer(), filePath(filePath) {}

  /// Iterate over all of the available (non-empty) translations
  /// maintained by this producer, callback gets each translation
  /// with its unique identifier.
  void forEachAvailable(
      llvm::function_ref<void(swift::DiagID, llvm::StringRef)> callback);

protected:
  bool initializeImpl() override;
  llvm::StringRef getMessage(swift::DiagID id) const override;

private:
  static void readStringsFile(llvm::MemoryBuffer *in,
                              std::vector<std::string> &diagnostics);
};

class SerializedLocalizationProducer final : public LocalizationProducer {
  using SerializedLocalizationTable =
      llvm::OnDiskIterableChainedHashTable<LocalizationReaderInfo>;
  using offset_type = LocalizationReaderInfo::offset_type;
  std::unique_ptr<llvm::MemoryBuffer> Buffer;
  std::unique_ptr<SerializedLocalizationTable> SerializedTable;

public:
  explicit SerializedLocalizationProducer(
      std::unique_ptr<llvm::MemoryBuffer> buffer);

protected:
  bool initializeImpl() override;
  llvm::StringRef getMessage(swift::DiagID id) const override;
};

} // namespace diag
} // namespace swift

#endif
