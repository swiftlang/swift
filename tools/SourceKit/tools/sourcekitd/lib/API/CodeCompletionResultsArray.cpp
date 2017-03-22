//===--- CodeCompletionResultsArray.cpp -----------------------------------===//
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

#include "sourcekitd/CodeCompletionResultsArray.h"
#include "sourcekitd/CompactArray.h"
#include "SourceKit/Core/LLVM.h"
#include "SourceKit/Support/UIdent.h"
#include "DictionaryKeys.h"

#include "llvm/Support/MemoryBuffer.h"

using namespace SourceKit;
using namespace sourcekitd;

struct CodeCompletionResultsArrayBuilder::Implementation {
  CompactArrayBuilder<UIdent,
                      StringRef,
                      StringRef,
                      StringRef,
                      StringRef,
                      Optional<StringRef>,
                      Optional<StringRef>,
                      Optional<StringRef>,
                      UIdent,
                      uint8_t> Builder;
};

CodeCompletionResultsArrayBuilder::CodeCompletionResultsArrayBuilder()
  : Impl(*new Implementation()) {

}

CodeCompletionResultsArrayBuilder::~CodeCompletionResultsArrayBuilder() {
  delete &Impl;
}

void CodeCompletionResultsArrayBuilder::add(
    UIdent Kind,
    StringRef Name,
    StringRef Description,
    StringRef SourceText,
    StringRef TypeName,
    Optional<StringRef> ModuleName,
    Optional<StringRef> DocBrief,
    Optional<StringRef> AssocUSRs,
    UIdent SemanticContext,
    bool NotRecommended,
    unsigned NumBytesToErase) {

  assert(NumBytesToErase <= (uint8_t(-1) >> 1));
  uint8_t BytesAndNotRecommended = (NumBytesToErase << 1) | NotRecommended;
  Impl.Builder.addEntry(Kind,
                        Name,
                        Description,
                        SourceText,
                        TypeName,
                        ModuleName,
                        DocBrief,
                        AssocUSRs,
                        SemanticContext,
                        BytesAndNotRecommended);
}

std::unique_ptr<llvm::MemoryBuffer>
CodeCompletionResultsArrayBuilder::createBuffer() {
  return Impl.Builder.createBuffer();
}

namespace {

class CodeCompletionResultsArray {
public:
  typedef CompactArrayReader<sourcekitd_uid_t,
                             const char *,
                             const char *,
                             const char *,
                             const char *,
                             const char *,
                             const char *,
                             const char *,
                             sourcekitd_uid_t,
                             uint8_t> CompactArrayReaderTy;

  static bool
  dictionary_apply(void *Buf, size_t Index,
                   llvm::function_ref<bool(sourcekitd_uid_t,
                                           sourcekitd_variant_t)> applier) {
    CompactArrayReaderTy Reader(Buf);

    sourcekitd_uid_t Kind;
    const char *Name;
    const char *Description;
    const char *SourceText;
    const char *TypeName;
    const char *ModuleName;
    const char *DocBrief;
    const char *AssocUSRs;
    sourcekitd_uid_t SemanticContext;
    uint8_t BytesAndNotRecommended;

    Reader.readEntries(Index,
                  Kind,
                  Name,
                  Description,
                  SourceText,
                  TypeName,
                  ModuleName,
                  DocBrief,
                  AssocUSRs,
                  SemanticContext,
                  BytesAndNotRecommended);

    unsigned NumBytesToErase = BytesAndNotRecommended >> 1;
    bool NotRecommended = BytesAndNotRecommended & 0x1;

#define APPLY(K, Ty, Field)                              \
  do {                                                   \
    sourcekitd_uid_t key = SKDUIDFromUIdent(K);          \
    sourcekitd_variant_t var = make##Ty##Variant(Field); \
    if (!applier(key, var)) return false;                \
  } while (0)

    APPLY(KeyKind, UID, Kind);
    APPLY(KeyName, String, Name);
    APPLY(KeyDescription, String, Description);
    APPLY(KeySourceText, String, SourceText);
    APPLY(KeyTypeName, String, TypeName);
    if (ModuleName) {
      APPLY(KeyModuleName, String, ModuleName);
    }
    if (DocBrief) {
      APPLY(KeyDocBrief, String, DocBrief);
    }
    if (AssocUSRs) {
      APPLY(KeyAssociatedUSRs, String, AssocUSRs);
    }
    APPLY(KeyContext, UID, SemanticContext);
    APPLY(KeyNumBytesToErase, Int, NumBytesToErase);
    if (NotRecommended) {
      APPLY(KeyNotRecommended, Bool, NotRecommended);
    }

    return true;
  }
};

}

VariantFunctions *
sourcekitd::getVariantFunctionsForCodeCompletionResultsArray() {
  return &CompactArrayFuncs<CodeCompletionResultsArray>::Funcs;
}
