//===---------- VariableTypeArray.cpp -------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "sourcekitd/VariableTypeArray.h"
#include "sourcekitd/CompactArray.h"
#include "sourcekitd/DictionaryKeys.h"
#include "SourceKit/Core/LLVM.h"
#include "SourceKit/Core/LangSupport.h"
#include "SourceKit/Support/UIdent.h"

#include "llvm/Support/MemoryBuffer.h"

using namespace SourceKit;
using namespace sourcekitd;

namespace {
class VariableTypeReader {
  /// A string that contains the types of the variables that are reported by
  /// this \c VariableTypeReader. Each type is null-terminated and \c
  /// EntryReader references the types using offsets into this string.
  const char *PrintedTypes;
  // Four unsigned integers for:
  //  - Variable offset in the source buffer
  //  - Variable length in the source buffer
  //  - Offset of printed Variable type inside `PrintedTypes`
  //  - Whether the variable has an explicit type annotation
  CompactArrayReader<unsigned, unsigned, unsigned, unsigned> EntryReader;

  static uint64_t getHeaderValue(char *Buffer, unsigned Index) {
    uint64_t HeaderField;
    memcpy(&HeaderField, (uint64_t *)Buffer + Index, sizeof(HeaderField));
    return HeaderField;
  }

public:
  VariableTypeReader(char *Buffer)
      : EntryReader(Buffer + getHeaderValue(Buffer, 0)) {
    // Read the printed type string buffer here.
    CompactArrayReader<const char *> Reader(Buffer + getHeaderValue(Buffer, 1));
    Reader.readEntries(0, PrintedTypes);
  }

  uint64_t count() const { return EntryReader.getCount(); }

  VariableType getVariable(uint64_t Idx) {
    VariableType Result;
    unsigned HasExplicitType;
    EntryReader.readEntries(Idx, Result.VarOffset, Result.VarLength,
                            Result.TypeOffset, HasExplicitType);
    Result.HasExplicitType = static_cast<bool>(HasExplicitType);
    return Result;
  }

  const char *readPrintedType(unsigned Offset) { return PrintedTypes + Offset; }

  static bool
  dictionary_apply(void *Buffer, size_t Index,
                   sourcekitd_variant_dictionary_applier_f_t Applier,
                   void *Context) {
    VariableTypeReader Reader((char *)Buffer);
    auto Result = Reader.getVariable(Index);
#define APPLY(K, Ty, Field)                                                    \
  do {                                                                         \
    sourcekitd_uid_t Key = SKDUIDFromUIdent(K);                                \
    sourcekitd_variant_t Var = make##Ty##Variant(Field);                       \
    if (!Applier(Key, Var, Context))                                           \
      return false;                                                            \
  } while (0)

    APPLY(KeyVariableOffset, Int, Result.VarOffset);
    APPLY(KeyVariableLength, Int, Result.VarLength);
    APPLY(KeyVariableType, String, Reader.readPrintedType(Result.TypeOffset));
    APPLY(KeyVariableTypeExplicit, Bool, Result.HasExplicitType);
    return true;
  }
};
} // end of anonymous namespace

struct VariableTypeArrayBuilder::Implementation {
  /// A builder that builds values read by \c EntryReader in \c
  /// VariableTypeReader. See \c VariableTypeReader::EntryReader for more info.
  CompactArrayBuilder<unsigned, unsigned, unsigned, unsigned> Builder;
  /// A builder that builds the \c PrintedTypes string used by \c
  /// VariableTypeReader. See \c VariableTypeReader::PrintedTypes for more info.
  CompactArrayBuilder<StringRef> StrBuilder;

  Implementation(StringRef PrintedTypes) { StrBuilder.addEntry(PrintedTypes); }
  static sourcekitd_variant_type_t get_type(sourcekitd_variant_t var) {
    return SOURCEKITD_VARIANT_TYPE_ARRAY;
  }

  static sourcekitd_variant_t array_get_value(sourcekitd_variant_t Array,
                                              size_t Index) {
    return {{(uintptr_t)&CompactVariantFuncs<VariableTypeReader>::Funcs,
             (uintptr_t)Array.data[1], Index}};
  }

  // data[0] = VariableTypeArrayBuilder::funcs
  // data[1] = custom buffer
  static size_t array_get_count(sourcekitd_variant_t Array) {
    VariableTypeReader Reader((char *)Array.data[1]);
    return Reader.count();
  }

  std::unique_ptr<llvm::MemoryBuffer> createBuffer(CustomBufferKind Kind) {
    std::array<CompactArrayBuilderImpl *, 2> Builders = {&Builder, &StrBuilder};
    auto KindSize = sizeof(uint64_t);
    size_t HeaderSize = sizeof(uint64_t) * Builders.size();
    auto AllSize = KindSize + HeaderSize;
    for (auto *B : Builders)
      AllSize += B->sizeInBytes();
    auto Result = llvm::WritableMemoryBuffer::getNewUninitMemBuffer(AllSize);
    *reinterpret_cast<uint64_t *>(Result->getBufferStart()) = (uint64_t)Kind;

    char *Start = Result->getBufferStart() + KindSize;
    char *HeaderPtr = Start;
    char *Ptr = Start + HeaderSize;
    auto addBuilder = [&](CompactArrayBuilderImpl &Buffer) {
      uint64_t Offset = Ptr - Start;
      memcpy(HeaderPtr, &Offset, sizeof(Offset));
      HeaderPtr += sizeof(Offset);
      Ptr += Buffer.copyInto(Ptr);
    };
    for (auto *B : Builders) {
      addBuilder(*B);
    }
    assert(Ptr == Result->getBufferEnd());
    return std::move(Result);
  }
};

VariableTypeArrayBuilder::VariableTypeArrayBuilder(StringRef PrintedTypes)
    : Impl(*new Implementation(PrintedTypes)) {}

VariableTypeArrayBuilder::~VariableTypeArrayBuilder() { delete &Impl; }

void VariableTypeArrayBuilder::add(const VariableType &VarType) {
  Impl.Builder.addEntry(VarType.VarOffset, VarType.VarLength,
                        VarType.TypeOffset /*Printed type is null-terminated*/,
                        VarType.HasExplicitType);
}

std::unique_ptr<llvm::MemoryBuffer> VariableTypeArrayBuilder::createBuffer() {
  return Impl.createBuffer(CustomBufferKind::VariableTypeArray);
}

VariantFunctions VariableTypeArrayBuilder::Funcs = {
    Implementation::get_type,
    nullptr /*AnnotArray_array_apply*/,
    nullptr /*AnnotArray_array_get_bool*/,
    nullptr /*AnnotArray_array_get_double*/,
    Implementation::array_get_count,
    nullptr /*AnnotArray_array_get_int64*/,
    nullptr /*AnnotArray_array_get_string*/,
    nullptr /*AnnotArray_array_get_uid*/,
    Implementation::array_get_value,
    nullptr /*AnnotArray_bool_get_value*/,
    nullptr /*AnnotArray_double_get_value*/,
    nullptr /*AnnotArray_dictionary_apply*/,
    nullptr /*AnnotArray_dictionary_get_bool*/,
    nullptr /*AnnotArray_dictionary_get_double*/,
    nullptr /*AnnotArray_dictionary_get_int64*/,
    nullptr /*AnnotArray_dictionary_get_string*/,
    nullptr /*AnnotArray_dictionary_get_value*/,
    nullptr /*AnnotArray_dictionary_get_uid*/,
    nullptr /*AnnotArray_string_get_length*/,
    nullptr /*AnnotArray_string_get_ptr*/,
    nullptr /*AnnotArray_int64_get_value*/,
    nullptr /*AnnotArray_uid_get_value*/,
    nullptr /*Annot_data_get_size*/,
    nullptr /*Annot_data_get_ptr*/,
};

VariantFunctions *sourcekitd::getVariantFunctionsForVariableTypeArray() {
  return &VariableTypeArrayBuilder::Funcs;
}
