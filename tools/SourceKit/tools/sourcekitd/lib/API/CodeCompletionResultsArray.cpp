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
#include "sourcekitd/DictionaryKeys.h"
#include "SourceKit/Core/LLVM.h"
#include "SourceKit/Support/UIdent.h"

#include "llvm/Support/MemoryBuffer.h"

using namespace SourceKit;
using namespace sourcekitd;

struct CodeCompletionResultsArrayBuilder::Implementation {
  CompactArrayBuilder<UIdent, StringRef, StringRef, StringRef, StringRef,
                      std::optional<StringRef>, std::optional<StringRef>,
                      std::optional<StringRef>, UIdent, UIdent, uint8_t,
                      uint8_t>
      Builder;
};

CodeCompletionResultsArrayBuilder::CodeCompletionResultsArrayBuilder()
  : Impl(*new Implementation()) {

}

CodeCompletionResultsArrayBuilder::~CodeCompletionResultsArrayBuilder() {
  delete &Impl;
}

void CodeCompletionResultsArrayBuilder::add(
    UIdent Kind, StringRef Name, StringRef Description, StringRef SourceText,
    StringRef TypeName, std::optional<StringRef> ModuleName,
    std::optional<StringRef> DocBrief, std::optional<StringRef> AssocUSRs,
    UIdent SemanticContext, UIdent TypeRelation, bool NotRecommended,
    bool IsSystem, unsigned NumBytesToErase) {

  uint8_t Flags = 0;
  Flags |= NotRecommended << 1;
  Flags |= IsSystem << 0;

  assert(NumBytesToErase <= uint8_t(-1));

  Impl.Builder.addEntry(Kind,
                        Name,
                        Description,
                        SourceText,
                        TypeName,
                        ModuleName,
                        DocBrief,
                        AssocUSRs,
                        SemanticContext,
                        TypeRelation,
                        Flags,
                        uint8_t(NumBytesToErase));
}

std::unique_ptr<llvm::MemoryBuffer>
CodeCompletionResultsArrayBuilder::createBuffer() {
  return Impl.Builder.createBuffer(
      CustomBufferKind::CodeCompletionResultsArray);
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
                             sourcekitd_uid_t,
                             uint8_t,
                             uint8_t> CompactArrayReaderTy;

  static bool
  dictionary_apply(void *Buf, size_t Index,
                   sourcekitd_variant_dictionary_applier_f_t applier,
                   void *context) {
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
    sourcekitd_uid_t TypeRelation;
    uint8_t Flags;
    uint8_t NumBytesToErase;

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
                  TypeRelation,
                  Flags,
                  NumBytesToErase);

    bool NotRecommended = Flags & 0x2;
    bool IsSystem = Flags & 0x1;

#define APPLY(K, Ty, Field)                                                    \
  do {                                                                         \
    sourcekitd_uid_t key = SKDUIDFromUIdent(K);                                \
    sourcekitd_variant_t var = make##Ty##Variant(Field);                       \
    if (!applier(key, var, context))                                           \
      return false;                                                            \
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
    APPLY(KeyTypeRelation, UID, TypeRelation);
    APPLY(KeyNumBytesToErase, Int, NumBytesToErase);
    if (NotRecommended) {
      APPLY(KeyNotRecommended, Bool, NotRecommended);
    }
    if (IsSystem) {
      APPLY(KeyIsSystem, Bool, IsSystem);
    }

    return true;
  }
};

} // end anonymous namespace

VariantFunctions *
sourcekitd::getVariantFunctionsForCodeCompletionResultsArray() {
  return &CompactArrayFuncs<CodeCompletionResultsArray>::Funcs;
}
