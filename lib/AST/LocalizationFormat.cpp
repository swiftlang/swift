//===--- LocalizationFormat.cpp - YAML format for Diagnostic Messages ---*-
// C++ -*-===//
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

#include "swift/AST/LocalizationFormat.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/Support/YAMLTraits.h"

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
  }
};

template <> struct MappingTraits<swift::diag::DiagnosticNode> {
  static void mapping(IO &io, swift::diag::DiagnosticNode &node) {
    io.mapRequired("id", node.id);
    io.mapRequired("msg", node.msg);
  }
};

} // namespace yaml
} // namespace llvm

LLVM_YAML_IS_SEQUENCE_VECTOR(swift::diag::DiagnosticNode)

namespace swift {
namespace diag {

YAMLLocalizationProducer::YAMLLocalizationProducer(std::string locale,
                                                   std::string path) {
  llvm::SmallString<128> DiagnosticsFilePath(path);
  llvm::sys::path::append(DiagnosticsFilePath, locale);
  llvm::sys::path::replace_extension(DiagnosticsFilePath, ".yaml");
  auto FileBufOrErr = llvm::MemoryBuffer::getFileOrSTDIN(DiagnosticsFilePath);
  // Absence of localizations shouldn't crash the compiler.
  if (!FileBufOrErr)
    return;
  llvm::MemoryBuffer *document = FileBufOrErr->get();
  diag::LocalizationInput yin(document->getBuffer());
  yin >> diagnostics;
}

llvm::StringRef
YAMLLocalizationProducer::getMessageOr(DiagID id,
                                       llvm::StringRef defaultMessage) const {
  if (diagnostics.empty())
    return defaultMessage;
  const std::string &diagnosticMessage = diagnostics[(unsigned)id];
  if (diagnosticMessage.empty())
    return defaultMessage;
  return diagnosticMessage;
}

template <typename T, typename Context>
typename std::enable_if<llvm::yaml::has_SequenceTraits<T>::value, void>::type
readYAML(llvm::yaml::IO &io, T &Seq, bool, Context &Ctx) {
  // Resize Diags from YAML file to be the same size
  // as diagnosticStrings from def files.
  Seq.resize((unsigned)DiagID::NumDiags);
  unsigned count = io.beginSequence();
  for (unsigned i = 0; i < count; ++i) {
    void *SaveInfo;
    if (io.preflightElement(i, SaveInfo)) {
      DiagnosticNode current;
      yamlize(io, current, true, Ctx);
      io.postflightElement(SaveInfo);
      // YAML file isn't guaranteed to have diagnostics in order of their
      // declaration in `.def` files, to accommodate that we need to leave
      // holes in diagnostic array for diagnostics which haven't yet been
      // localized and for the ones that have `DiagnosticNode::id`
      // indicates their position.
      Seq[static_cast<unsigned>(current.id)] = std::move(current.msg);
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
    readYAML(yin, diagnostics, true, Ctx);
  }
  return yin;
}

} // namespace diag
} // namespace swift
