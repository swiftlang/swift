//===--- TokenAnnotationsArray.cpp ----------------------------------------===//
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

#include "sourcekitd/TokenAnnotationsArray.h"
#include "sourcekitd/CompactArray.h"
#include "SourceKit/Core/LLVM.h"
#include "SourceKit/Support/UIdent.h"
#include "DictionaryKeys.h"

#include "llvm/Support/MemoryBuffer.h"

using namespace SourceKit;
using namespace sourcekitd;

struct TokenAnnotationsArrayBuilder::Implementation {
  CompactArrayBuilder<UIdent,
                      unsigned,
                      unsigned> Builder;
};

TokenAnnotationsArrayBuilder::TokenAnnotationsArrayBuilder()
  : Impl(*new Implementation()) {

}

TokenAnnotationsArrayBuilder::~TokenAnnotationsArrayBuilder() {
  delete &Impl;
}

void TokenAnnotationsArrayBuilder::add(
    UIdent Kind,
    unsigned Offset,
    unsigned Length,
    bool IsSystem) {

  assert(Length <= (unsigned(-1) >> 1));
  unsigned LengthAndIsSystem = (Length << 1) | IsSystem;
  Impl.Builder.addEntry(Kind,
                        Offset,
                        LengthAndIsSystem);
}

bool TokenAnnotationsArrayBuilder::empty() const {
  return Impl.Builder.empty();
}

std::unique_ptr<llvm::MemoryBuffer>
TokenAnnotationsArrayBuilder::createBuffer() {
  return Impl.Builder.createBuffer();
}

namespace {

class TokenAnnotationsArray {
public:
  typedef CompactArrayReader<sourcekitd_uid_t,
                             unsigned,
                             unsigned> CompactArrayReaderTy;

  static void readElements(void *Buf, size_t Index,
                           sourcekitd_uid_t &Kind,
                           unsigned &Offset,
                           unsigned &Length,
                           bool &IsSystem) {
    CompactArrayReaderTy Reader(Buf);
    unsigned LengthAndIsSystem;
    Reader.readEntries(Index,
                   Kind,
                   Offset,
                   LengthAndIsSystem);
    Length = LengthAndIsSystem >> 1;
    IsSystem = LengthAndIsSystem & 0x1;
  }

  static bool apply(sourcekitd_uid_t Kind,
                    unsigned Offset,
                    unsigned Length,
                    bool IsSystem,
                    llvm::function_ref<bool(sourcekitd_uid_t,
                                            sourcekitd_variant_t)> applier) {

#define APPLY(K, Ty, Field)                              \
  do {                                                   \
    sourcekitd_uid_t key = SKDUIDFromUIdent(K);          \
    sourcekitd_variant_t var = make##Ty##Variant(Field); \
    if (!applier(key, var)) return false;                \
  } while (0)

    APPLY(KeyKind, UID, Kind);
    APPLY(KeyOffset, Int, Offset);
    APPLY(KeyLength, Int, Length);
    if (IsSystem)
      APPLY(KeyIsSystem, Bool, IsSystem);

    return true;
  }

  static bool getBool(sourcekitd_uid_t Key,
                      sourcekitd_uid_t Kind,
                      unsigned Offset,
                      unsigned Length,
                      bool IsSystem) {
    if (Key == SKDUIDFromUIdent(KeyIsSystem))
      return IsSystem;
    return false;
  }

  static int64_t getInt(sourcekitd_uid_t Key,
                        sourcekitd_uid_t Kind,
                        unsigned Offset,
                        unsigned Length,
                        bool IsSystem) {
    if (Key == SKDUIDFromUIdent(KeyOffset))
      return Offset;
    if (Key == SKDUIDFromUIdent(KeyLength))
      return Length;
    return 0;
  }

  static sourcekitd_uid_t getUID(sourcekitd_uid_t Key,
                                 sourcekitd_uid_t Kind,
                                 unsigned Offset,
                                 unsigned Length,
                                 bool IsSystem) {
    if (Key == SKDUIDFromUIdent(KeyKind))
      return Kind;
    return nullptr;
  }
};

}

namespace sourcekitd {

template <>
struct CompactVariantFuncs<TokenAnnotationsArray> {
  static sourcekitd_variant_type_t get_type(sourcekitd_variant_t var) {
    return SOURCEKITD_VARIANT_TYPE_DICTIONARY;
  }

  template <typename FnTy>
  static auto getElement(sourcekitd_variant_t dict, sourcekitd_uid_t key,
                const FnTy &Fn) -> decltype(Fn(nullptr, nullptr, 0, 0, false)) {
    void *Buf = (void*)dict.data[1];
    size_t Index = dict.data[2];

    sourcekitd_uid_t Kind;
    unsigned Offset;
    unsigned Length;
    bool IsSystem;
    TokenAnnotationsArray::readElements(Buf, Index,
                                        Kind, Offset, Length, IsSystem);

    return Fn(key, Kind, Offset, Length, IsSystem);
  }
  
  static bool
  dictionary_apply(sourcekitd_variant_t dict,
                   llvm::function_ref<bool(sourcekitd_uid_t,
                                           sourcekitd_variant_t)> applier) {
    void *Buf = (void*)dict.data[1];
    size_t Index = dict.data[2];

    sourcekitd_uid_t Kind;
    unsigned Offset;
    unsigned Length;
    bool IsSystem;
    TokenAnnotationsArray::readElements(Buf, Index,
                                        Kind, Offset, Length, IsSystem);
    return TokenAnnotationsArray::apply(Kind, Offset, Length, IsSystem, applier);
  }

  static bool dictionary_get_bool(sourcekitd_variant_t dict,
                                  sourcekitd_uid_t key) {
    return getElement(dict, key, TokenAnnotationsArray::getBool);
  }

  static int64_t dictionary_get_int64(sourcekitd_variant_t dict,
                                      sourcekitd_uid_t key) {

    return getElement(dict, key, TokenAnnotationsArray::getInt);
  }

  static sourcekitd_uid_t dictionary_get_uid(sourcekitd_variant_t dict,
                                             sourcekitd_uid_t key) {

    return getElement(dict, key, TokenAnnotationsArray::getUID);
  }

  static VariantFunctions Funcs;
};

VariantFunctions CompactVariantFuncs<TokenAnnotationsArray>::Funcs = {
  get_type,
  nullptr/*Annot_array_apply*/,
  nullptr/*Annot_array_get_bool*/,
  nullptr/*Annot_array_get_count*/,
  nullptr/*Annot_array_get_int64*/,
  nullptr/*Annot_array_get_string*/,
  nullptr/*Annot_array_get_uid*/,
  nullptr/*Annot_array_get_value*/,
  nullptr/*Annot_bool_get_value*/,
  dictionary_apply,
  dictionary_get_bool,
  dictionary_get_int64,
  nullptr/*Annot_dictionary_get_string*/,
  nullptr/*Annot_dictionary_get_value*/,
  dictionary_get_uid,
  nullptr/*Annot_string_get_length*/,
  nullptr/*Annot_string_get_ptr*/,
  nullptr/*Annot_int64_get_value*/,
  nullptr/*Annot_uid_get_value*/
};

}

VariantFunctions *
sourcekitd::getVariantFunctionsForTokenAnnotationsArray() {
  return &CompactArrayFuncs<TokenAnnotationsArray>::Funcs;
}
