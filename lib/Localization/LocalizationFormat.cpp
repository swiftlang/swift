//===-- LocalizationFormat.cpp - Format for Diagnostic Messages -*- C++ -*-===//
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
// This file implements the format for localized diagnostic messages.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/Range.h"
#include "swift/Localization/LocalizationFormat.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Bitstream/BitstreamReader.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/Support/YAMLTraits.h"
#include <cstdint>
#include <string>
#include <system_error>
#include <type_traits>

namespace {

enum LocalDiagID : uint32_t {
#define DIAG(KIND, ID, Options, Text, Signature) ID,
#include "swift/AST/DiagnosticsAll.def"
  NumDiags
};

} // namespace

namespace llvm {
namespace yaml {

template <> struct ScalarEnumerationTraits<LocalDiagID> {
  static void enumeration(IO &io, LocalDiagID &value) {
#define DIAG(KIND, ID, Options, Text, Signature)                               \
  io.enumCase(value, #ID, LocalDiagID::ID);
#include "swift/AST/DiagnosticsAll.def"
    // Ignore diagnostic IDs that are available in the YAML file and not
    // available in the `.def` file.
    if (io.matchEnumFallback())
      value = LocalDiagID::NumDiags;
  }
};

} // namespace yaml
} // namespace llvm

namespace swift {
namespace diag {

void SerializedLocalizationWriter::insert(swift::DiagID id,
                                          llvm::StringRef translation) {
  generator.insert(static_cast<uint32_t>(id), translation);
}

bool SerializedLocalizationWriter::emit(llvm::StringRef filePath) {
  assert(llvm::sys::path::extension(filePath) == ".db");
  std::error_code error;
  llvm::raw_fd_ostream OS(filePath, error, llvm::sys::fs::F_None);
  if (OS.has_error()) {
    return true;
  }

  offset_type offset;
  {
    llvm::support::endian::write<offset_type>(OS, 0, llvm::support::little);
    offset = generator.Emit(OS);
  }
  OS.seek(0);
  llvm::support::endian::write(OS, offset, llvm::support::little);
  OS.close();

  return OS.has_error();
}

SerializedLocalizationProducer::SerializedLocalizationProducer(
    std::unique_ptr<llvm::MemoryBuffer> buffer)
    : Buffer(std::move(buffer)) {
  auto base =
      reinterpret_cast<const unsigned char *>(Buffer.get()->getBufferStart());
  auto tableOffset = endian::read<offset_type>(base, little);
  SerializedTable.reset(SerializedLocalizationTable::Create(
      base + tableOffset, base + sizeof(offset_type), base));
}

llvm::StringRef
SerializedLocalizationProducer::getMessage(swift::DiagID id) const {
  auto value = SerializedTable.get()->find(id);
  if (value.getDataLen() == 0)
    return llvm::StringRef();
  return {(const char *)value.getDataPtr(), value.getDataLen()};
}

YAMLLocalizationProducer::YAMLLocalizationProducer(llvm::StringRef filePath) {
  auto FileBufOrErr = llvm::MemoryBuffer::getFileOrSTDIN(filePath);
  llvm::MemoryBuffer *document = FileBufOrErr->get();
  diag::LocalizationInput yin(document->getBuffer());
  yin >> diagnostics;
  unknownIDs = std::move(yin.unknownIDs);
}

llvm::StringRef YAMLLocalizationProducer::getMessage(swift::DiagID id) const {
  return diagnostics[(unsigned)id];
}

void YAMLLocalizationProducer::forEachAvailable(
    llvm::function_ref<void(swift::DiagID, llvm::StringRef)> callback) const {
  for (uint32_t i = 0, n = diagnostics.size(); i != n; ++i) {
    auto translation = diagnostics[i];
    if (!translation.empty())
      callback(static_cast<swift::DiagID>(i), translation);
  }
}

llvm::Optional<uint32_t> LocalizationInput::readID(llvm::yaml::IO &io) {
  LocalDiagID diagID;
  io.mapRequired("id", diagID);
  if (diagID == LocalDiagID::NumDiags)
    return llvm::None;
  return static_cast<uint32_t>(diagID);
}

template <typename T, typename Context>
typename std::enable_if<llvm::yaml::has_SequenceTraits<T>::value, void>::type
readYAML(llvm::yaml::IO &io, T &Seq, T &unknownIDs, bool, Context &Ctx) {
  unsigned count = io.beginSequence();
  if (count) {
    Seq.resize(LocalDiagID::NumDiags);
  }

  for (unsigned i = 0; i < count; ++i) {
    void *SaveInfo;
    if (io.preflightElement(i, SaveInfo)) {
      io.beginMapping();

      // If the current diagnostic ID is available in YAML and in `.def`, add it
      // to the diagnostics array. Otherwise, re-parse the current diagnnostic
      // id as a string and store it in `unknownIDs` array.
      if (auto id = LocalizationInput::readID(io)) {
        // YAML file isn't guaranteed to have diagnostics in order of their
        // declaration in `.def` files, to accommodate that we need to leave
        // holes in diagnostic array for diagnostics which haven't yet been
        // localized and for the ones that have `id` indicates their position.
        io.mapRequired("msg", Seq[*id]);
      } else {
        std::string unknownID, message;
        // Read "raw" id since it doesn't exist in `.def` file.
        io.mapRequired("id", unknownID);
        io.mapRequired("msg", message);
        unknownIDs.push_back(unknownID);
      }
      io.endMapping();
      io.postflightElement(SaveInfo);
    }
  }
  io.endSequence();
}

template <typename T>
typename std::enable_if<llvm::yaml::has_SequenceTraits<T>::value,
                        LocalizationInput &>::type
operator>>(LocalizationInput &yin, T &diagnostics) {
  llvm::yaml::EmptyContext Ctx;
  if (yin.setCurrentDocument()) {
    // If YAML file's format doesn't match the current format in
    // DiagnosticMessageFormat, will throw an error.
    readYAML(yin, diagnostics, yin.unknownIDs, true, Ctx);
  }
  return yin;
}

void DefToYAMLConverter::convert(llvm::raw_ostream &out) {
  for (auto i : swift::indices(IDs)) {
    out << "- id: " << IDs[i] << "\n";

    const std::string &msg = Messages[i];

    out << "  msg: \"";
    // Add an escape character before a double quote `"` or a backslash `\`.
    for (unsigned j = 0; j < msg.length(); ++j) {
      if (msg[j] == '"') {
        out << '\\';
        out << '"';
      } else if (msg[j] == '\\') {
        out << '\\';
        out << '\\';
      } else {
        out << msg[j];
      }
    }
    out << "\"\r\n";
  }
}

} // namespace diag
} // namespace swift
