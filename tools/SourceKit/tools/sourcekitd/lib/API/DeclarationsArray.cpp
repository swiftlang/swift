//===--- DeclarationsArray.cpp --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "sourcekitd/DeclarationsArray.h"
#include "SourceKit/Core/LLVM.h"
#include "SourceKit/Support/UIdent.h"
#include "sourcekitd/CompactArray.h"
#include "sourcekitd/DictionaryKeys.h"

#include "llvm/Support/MemoryBuffer.h"

using namespace SourceKit;
using namespace sourcekitd;

struct DeclarationsArrayBuilder::Implementation {
  CompactArrayBuilder<UIdent, unsigned, unsigned, StringRef> Builder;
};

DeclarationsArrayBuilder::DeclarationsArrayBuilder()
    : Impl(*new Implementation()) {}

DeclarationsArrayBuilder::~DeclarationsArrayBuilder() { delete &Impl; }

void DeclarationsArrayBuilder::add(UIdent Kind, unsigned Offset,
                                   unsigned Length, StringRef USR) {

  Impl.Builder.addEntry(Kind, Offset, Length, USR);
}

bool DeclarationsArrayBuilder::empty() const { return Impl.Builder.empty(); }

std::unique_ptr<llvm::MemoryBuffer> DeclarationsArrayBuilder::createBuffer() {
  return Impl.Builder.createBuffer(CustomBufferKind::DeclarationsArray);
}

namespace {

class DeclarationsArray {
public:
  typedef CompactArrayReader<sourcekitd_uid_t, unsigned, unsigned, const char *>
      CompactArrayReaderTy;

  static void readElements(void *Buf, size_t Index, sourcekitd_uid_t &Kind,
                           unsigned &Offset, unsigned &Length,
                           const char *&USR) {
    CompactArrayReaderTy Reader(Buf);
    Reader.readEntries(Index, Kind, Offset, Length, USR);
  }

  static bool apply(sourcekitd_uid_t Kind, unsigned Offset, unsigned Length,
                    const char *USR,
                    sourcekitd_variant_dictionary_applier_f_t applier,
                    void *context) {

#define APPLY(K, Ty, Field)                                                    \
  do {                                                                         \
    sourcekitd_uid_t key = SKDUIDFromUIdent(K);                                \
    sourcekitd_variant_t var = make##Ty##Variant(Field);                       \
    if (!applier(key, var, context))                                           \
      return false;                                                            \
  } while (0)

    APPLY(KeyKind, UID, Kind);
    APPLY(KeyOffset, Int, Offset);
    APPLY(KeyLength, Int, Length);
    if (USR != nullptr && std::strcmp(USR, "") != 0)
      APPLY(KeyUSR, String, USR);

    return true;
  }

  static int64_t getInt(sourcekitd_uid_t Key, sourcekitd_uid_t Kind,
                        unsigned Offset, unsigned Length, const char *USR) {
    if (Key == SKDUIDFromUIdent(KeyOffset))
      return Offset;
    if (Key == SKDUIDFromUIdent(KeyLength))
      return Length;
    return 0;
  }

  static sourcekitd_uid_t getUID(sourcekitd_uid_t Key, sourcekitd_uid_t Kind,
                                 unsigned Offset, unsigned Length,
                                 const char *USR) {
    if (Key == SKDUIDFromUIdent(KeyKind))
      return Kind;
    return nullptr;
  }

  static const char *getString(sourcekitd_uid_t Key, sourcekitd_uid_t Kind,
                               unsigned Offset, unsigned Length,
                               const char *USR) {
    if (Key == SKDUIDFromUIdent(KeyUSR))
      return USR;
    return nullptr;
  }
};

} // end anonymous namespace

namespace sourcekitd {

template <>
struct CompactVariantFuncs<DeclarationsArray> {
  static sourcekitd_variant_type_t get_type(sourcekitd_variant_t var) {
    return SOURCEKITD_VARIANT_TYPE_DICTIONARY;
  }

  template <typename FnTy>
  static auto getElement(sourcekitd_variant_t dict, sourcekitd_uid_t key,
                         const FnTy &Fn) -> decltype(Fn(nullptr, nullptr, 0, 0,
                                                        nullptr)) {
    void *Buf = (void *)dict.data[1];
    size_t Index = dict.data[2];

    sourcekitd_uid_t Kind;
    unsigned Offset;
    unsigned Length;
    const char *USR;
    DeclarationsArray::readElements(Buf, Index, Kind, Offset, Length, USR);

    return Fn(key, Kind, Offset, Length, USR);
  }

  static bool
  dictionary_apply(sourcekitd_variant_t dict,
                   sourcekitd_variant_dictionary_applier_f_t applier,
                   void *context) {
    void *Buf = (void *)dict.data[1];
    size_t Index = dict.data[2];

    sourcekitd_uid_t Kind;
    unsigned Offset;
    unsigned Length;
    const char *USR;
    DeclarationsArray::readElements(Buf, Index, Kind, Offset, Length, USR);
    return DeclarationsArray::apply(Kind, Offset, Length, USR, applier,
                                    context);
  }

  static int64_t dictionary_get_int64(sourcekitd_variant_t dict,
                                      sourcekitd_uid_t key) {

    return getElement(dict, key, DeclarationsArray::getInt);
  }

  static sourcekitd_uid_t dictionary_get_uid(sourcekitd_variant_t dict,
                                             sourcekitd_uid_t key) {

    return getElement(dict, key, DeclarationsArray::getUID);
  }

  static const char *dictionary_get_string(sourcekitd_variant_t dict,
                                           sourcekitd_uid_t key) {

    return getElement(dict, key, DeclarationsArray::getString);
  }

  static VariantFunctions Funcs;
};

VariantFunctions CompactVariantFuncs<DeclarationsArray>::Funcs = {
    get_type,
    nullptr /*Annot_array_apply*/,
    nullptr /*Annot_array_get_bool*/,
    nullptr /*Annot_array_get_double*/,
    nullptr /*Annot_array_get_count*/,
    nullptr /*Annot_array_get_int64*/,
    nullptr /*Annot_array_get_string*/,
    nullptr /*Annot_array_get_uid*/,
    nullptr /*Annot_array_get_value*/,
    nullptr /*Annot_bool_get_value*/,
    nullptr /*Annot_double_get_value*/,
    dictionary_apply,
    nullptr /*Annot_dictionary_get_bool*/,
    nullptr /*Annot_dictionary_get_double*/,
    dictionary_get_int64,
    dictionary_get_string,
    nullptr /*Annot_dictionary_get_value*/,
    dictionary_get_uid,
    nullptr /*Annot_string_get_length*/,
    nullptr /*Annot_string_get_ptr*/,
    nullptr /*Annot_int64_get_value*/,
    nullptr /*Annot_uid_get_value*/,
    nullptr /*Annot_data_get_size*/,
    nullptr /*Annot_data_get_ptr*/,
};

} // namespace sourcekitd

VariantFunctions *sourcekitd::getVariantFunctionsForDeclarationsArray() {
  return &CompactArrayFuncs<DeclarationsArray>::Funcs;
}
