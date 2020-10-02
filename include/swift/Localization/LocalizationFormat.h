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
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Bitstream/BitstreamReader.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/OnDiskHashTable.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Support/raw_ostream.h"
#include <cstdint>
#include <memory>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

namespace swift {
enum class DiagID : uint32_t;

namespace diag {

using namespace llvm::support;

class DefToYAMLConverter {
  llvm::ArrayRef<const char *> IDs;
  llvm::ArrayRef<const char *> Messages;

public:
  DefToYAMLConverter(llvm::ArrayRef<const char *> ids,
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

  hash_value_type ComputeHash(key_type_ref key) { return llvm::hash_code(key); }

  std::pair<offset_type, offset_type> EmitKeyDataLength(llvm::raw_ostream &out,
                                                        key_type_ref key,
                                                        data_type_ref data) {
    offset_type dataLength = static_cast<offset_type>(data.size());
    endian::write<offset_type>(out, dataLength, little);
    // No need to write the key length; it's constant.
    return {sizeof(key_type), dataLength};
  }

  void EmitKey(llvm::raw_ostream &out, key_type_ref key, unsigned len) {
    assert(len == sizeof(key_type));
    endian::write<key_type>(out, key, little);
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

  hash_value_type ComputeHash(internal_key_type key) {
    return llvm::hash_code(key);
  }

  static std::pair<offset_type, offset_type>
  ReadKeyDataLength(const unsigned char *&data) {
    offset_type dataLength =
        endian::readNext<offset_type, little, unaligned>(data);
    return {sizeof(uint32_t), dataLength};
  }

  internal_key_type ReadKey(const unsigned char *data, offset_type length) {
    return endian::readNext<internal_key_type, little, unaligned>(data);
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
public:
  /// If the  message isn't available/localized in current context
  /// return the fallback default message.
  virtual llvm::StringRef getMessageOr(swift::DiagID id,
                                       llvm::StringRef defaultMessage) const {
    auto message = getMessage(id);
    return message.empty() ? defaultMessage : message;
  }

  virtual ~LocalizationProducer() {}

protected:
  /// Retrieve a message for the given diagnostic id.
  /// \returns empty string if message couldn't be found.
  virtual llvm::StringRef getMessage(swift::DiagID id) const = 0;
};

class YAMLLocalizationProducer final : public LocalizationProducer {
  std::vector<std::string> diagnostics;

public:
  /// The diagnostics IDs that are no longer available in `.def`
  std::vector<std::string> unknownIDs;
  explicit YAMLLocalizationProducer(llvm::StringRef filePath);

  /// Iterate over all of the available (non-empty) translations
  /// maintained by this producer, callback gets each translation
  /// with its unique identifier.
  void forEachAvailable(
      llvm::function_ref<void(swift::DiagID, llvm::StringRef)> callback) const;

protected:
  llvm::StringRef getMessage(swift::DiagID id) const override;
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
  llvm::StringRef getMessage(swift::DiagID id) const override;
};

class LocalizationInput : public llvm::yaml::Input {
  using Input::Input;

  /// Read diagnostics in the YAML file iteratively
  template <typename T, typename Context>
  friend typename std::enable_if<llvm::yaml::has_SequenceTraits<T>::value,
                                 void>::type
  readYAML(llvm::yaml::IO &io, T &Seq, T &unknownIDs, bool, Context &Ctx);

  template <typename T>
  friend typename std::enable_if<llvm::yaml::has_SequenceTraits<T>::value,
                                 LocalizationInput &>::type
  operator>>(LocalizationInput &yin, T &diagnostics);

public:
  /// A vector that keeps track of the diagnostics IDs that are available in
  /// YAML and not available in `.def` files.
  std::vector<std::string> unknownIDs;
  
  /// A diagnostic ID might be present in YAML and not in `.def` file, if that's
  /// the case the `id` won't have a `DiagID` value.
  /// If the `id` is available in `.def` file this method will return the `id`'s
  /// value, otherwise this method won't return a value.
  static llvm::Optional<uint32_t> readID(llvm::yaml::IO &io);
};

} // namespace diag
} // namespace swift

#endif
