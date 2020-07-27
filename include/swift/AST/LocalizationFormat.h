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

#include "llvm/ADT/StringRef.h"
#include "llvm/Bitstream/BitstreamReader.h"
#include "llvm/Support/DJB.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/OnDiskHashTable.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/Support/YAMLTraits.h"
#include <string>
#include <type_traits>

namespace swift {
enum class DiagID : uint32_t;

namespace diag {
using namespace llvm::support;

class LocalizationWriterInfo {
public:
  using key_type = llvm::StringRef;
  using key_type_ref = key_type;
  using data_type = llvm::StringRef;
  using data_type_ref = data_type;
  using hash_value_type = uint32_t;
  using offset_type = uint32_t;

  hash_value_type ComputeHash(key_type_ref key) { return llvm::djbHash(key); }

  std::pair<unsigned, unsigned> EmitKeyDataLength(llvm::raw_ostream &out,
                                                  key_type_ref key,
                                                  data_type_ref data) {
    offset_type keyLength = static_cast<offset_type>(key.size());
    offset_type dataLength = static_cast<offset_type>(data.size());
    endian::write<offset_type>(out, keyLength, little);
    endian::write<offset_type>(out, dataLength, little);
    return {keyLength, dataLength};
  }

  void EmitKey(llvm::raw_ostream &out, key_type_ref key, unsigned len) {
    out << key;
  }

  void EmitData(llvm::raw_ostream &out, key_type_ref key, data_type_ref data,
                unsigned len) {
    out << data;
  }
};

class LocalizationReaderInfo {
public:
  using internal_key_type = llvm::StringRef;
  using external_key_type = internal_key_type;
  using data_type = llvm::StringRef;
  using hash_value_type = uint32_t;
  using offset_type = uint32_t;

  internal_key_type GetInternalKey(external_key_type key) { return key; }

  external_key_type GetExternalKey(internal_key_type key) { return key; }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  hash_value_type ComputeHash(internal_key_type key) {
    return llvm::djbHash(key);
  }

  static std::pair<unsigned, unsigned> ReadKeyDataLength(const uint8_t *&data) {
    offset_type keyLength =
        endian::readNext<offset_type, little, unaligned>(data);
    offset_type dataLength =
        endian::readNext<offset_type, little, unaligned>(data);
    return {keyLength, dataLength};
  }

  internal_key_type ReadKey(const uint8_t *data, offset_type length) {
    return internal_key_type((const char *)data, length);
  }

  data_type ReadData(llvm::StringRef Key, const uint8_t *data,
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
  /// 'cannot_convert_argument'.
  /// \param translation The localized diagnostic
  /// message for the given identifier.
  void insert(llvm::StringRef id, llvm::StringRef translation);

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
  /// If the  message isn't available/localized in the current `yaml` file,
  /// return the fallback default message.
  virtual llvm::StringRef getMessageOr(swift::DiagID id,
                                       llvm::StringRef defaultMessage) const {
    return defaultMessage;
  }

  virtual ~LocalizationProducer() {}
};

class YAMLLocalizationProducer final : public LocalizationProducer {
  // Type of the `diagnostics` vector.
  using T = std::vector<std::string>;
  struct Node {
    uint32_t id;
    typename T::value_type &msg;
  };
  typedef Node value_type;

  class iterator {
    typename T::iterator it;
    uint32_t counter;

  public:
    iterator(T::iterator _it, uint32_t counter = 0)
        : it(_it), counter(counter) {}

    iterator operator++() { return iterator(++it, ++counter); }

    bool operator!=(iterator other) { return it != other.it; }

    typename T::iterator::value_type node() { return *it; }

    value_type operator*() { return value_type{counter, *it}; }

    uint32_t index() { return counter; }
  };

public:
  std::vector<std::string> diagnostics;
  explicit YAMLLocalizationProducer(llvm::StringRef filePath);
  llvm::StringRef getMessageOr(swift::DiagID id,
                               llvm::StringRef defaultMessage) const override;

  iterator begin() { return iterator(diagnostics.begin()); }

  iterator end() { return iterator(diagnostics.end()); }
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

  llvm::StringRef getMessageOr(swift::DiagID id,
                               llvm::StringRef defaultMessage) const override;
};

class LocalizationInput : public llvm::yaml::Input {
  using Input::Input;

  /// Read diagnostics in the YAML file iteratively
  template <typename T, typename Context>
  friend typename std::enable_if<llvm::yaml::has_SequenceTraits<T>::value,
                                 void>::type
  readYAML(llvm::yaml::IO &io, T &Seq, bool, Context &Ctx);

  template <typename T>
  friend typename std::enable_if<llvm::yaml::has_SequenceTraits<T>::value,
                                 LocalizationInput &>::type
  operator>>(LocalizationInput &yin, T &diagnostics);
};

} // namespace diag
} // namespace swift

#endif
