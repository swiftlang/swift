//===---------- ExpressionTypeArray.cpp -----------------------------------===//
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

#include "sourcekitd/ExpressionTypeArray.h"
#include "sourcekitd/CompactArray.h"
#include "SourceKit/Core/LLVM.h"
#include "SourceKit/Core/LangSupport.h"
#include "SourceKit/Support/UIdent.h"
#include "DictionaryKeys.h"

#include "llvm/Support/MemoryBuffer.h"

using namespace SourceKit;
using namespace sourcekitd;

class ExpressionTypeReader {
  const char *printedType;
  CompactArrayReader<unsigned, unsigned, unsigned> entryReader;

  static uint64_t getHeaderValue(char *buffer, unsigned index) {
    uint64_t headerField;
    memcpy(&headerField, (uint64_t*)buffer + index, sizeof(headerField));
    return headerField;
  }

public:
  ExpressionTypeReader(char *buffer):
      entryReader(buffer + getHeaderValue(buffer, 0)) {
    // Read the printed type string buffer here.
    CompactArrayReader<const char*> reader(buffer + getHeaderValue(buffer, 1));
    reader.readEntries(0, printedType);
  }

  uint64_t count() const { return entryReader.getCount(); }

  std::pair<std::pair<unsigned, unsigned>, const char*> getExpression(uint64_t idx) {
    ExpressionType result;
    entryReader.readEntries(idx, result.ExprOffset, result.ExprLength,
                            result.TypeOffset);
    return {std::make_pair(result.ExprOffset, result.ExprLength),
      printedType + result.TypeOffset};
  }

  static bool
  dictionary_apply(void *buffer, size_t index,
                   llvm::function_ref<bool(sourcekitd_uid_t,
                                           sourcekitd_variant_t)> applier) {
    ExpressionTypeReader reader((char*)buffer);
    auto result = reader.getExpression(index);
#define APPLY(K, Ty, Field)                              \
  do {                                                   \
    sourcekitd_uid_t key = SKDUIDFromUIdent(K);          \
    sourcekitd_variant_t var = make##Ty##Variant(Field); \
    if (!applier(key, var)) return false;                \
  } while (0)
    APPLY(KeyExpressionOffset, Int, result.first.first);
    APPLY(KeyExpressionLength, Int, result.first.second);
    APPLY(KeyExpressionType, String, result.second);
    return true;
  }
};

struct ExpressionTypeArrayBuilder::Implementation {
  StringRef printedType;
  SmallVector<char, 256> buffer;
  CompactArrayBuilder<unsigned, unsigned, unsigned> builder;
  CompactArrayBuilder<StringRef> strBuilder;

  Implementation(StringRef PrintedType) { strBuilder.addEntry(PrintedType); }
  static sourcekitd_variant_type_t get_type(sourcekitd_variant_t var) {
    return SOURCEKITD_VARIANT_TYPE_ARRAY;
  }

  static sourcekitd_variant_t array_get_value(sourcekitd_variant_t array,
                                              size_t index) {
    return {{(uintptr_t)&CompactVariantFuncs<ExpressionTypeReader>::Funcs,
      (uintptr_t)array.data[1], index}};
  }

  // data[0] = ExpressionTypeArrayBuilder::funcs
  // data[1] = custum buffer
  static size_t array_get_count(sourcekitd_variant_t array) {
    ExpressionTypeReader reader((char*)array.data[1]);
    return reader.count();
  }

  std::unique_ptr<llvm::MemoryBuffer> createBuffer() {
    size_t headerSize = sizeof(uint64_t) * 2;
    auto result = llvm::WritableMemoryBuffer::getNewUninitMemBuffer(
      headerSize + builder.sizeInBytes() + strBuilder.sizeInBytes());
    char *start = result->getBufferStart();
    char *headerPtr = start;
    char *ptr = start + headerSize;
    auto addBuilder = [&](CompactArrayBuilderImpl& buffer) {
      uint64_t offset = ptr - start;
      memcpy(headerPtr, &offset, sizeof(offset));
      headerPtr += sizeof(offset);
      ptr += buffer.copyInto(ptr);
    };

    addBuilder(builder);
    addBuilder(strBuilder);
    assert(ptr == result->getBufferEnd());
    return std::move(result);
  }
};

ExpressionTypeArrayBuilder::ExpressionTypeArrayBuilder(StringRef printedType)
  : Impl(*new Implementation(printedType)) {}

ExpressionTypeArrayBuilder::~ExpressionTypeArrayBuilder() {
  delete &Impl;
}

void ExpressionTypeArrayBuilder::add(const ExpressionType &expType) {
  Impl.builder.addEntry(expType.ExprOffset, expType.ExprLength,
                        expType.TypeOffset/*Printed type is null ended*/);
}

std::unique_ptr<llvm::MemoryBuffer>
ExpressionTypeArrayBuilder::createBuffer() {
  return Impl.createBuffer();
}

VariantFunctions ExpressionTypeArrayBuilder::Funcs = {
  Implementation::get_type,
  nullptr /*AnnotArray_array_apply*/,
  nullptr /*AnnotArray_array_get_bool*/,
  Implementation::array_get_count,
  nullptr /*AnnotArray_array_get_int64*/,
  nullptr /*AnnotArray_array_get_string*/,
  nullptr /*AnnotArray_array_get_uid*/,
  Implementation::array_get_value,
  nullptr /*AnnotArray_bool_get_value*/,
  nullptr /*AnnotArray_dictionary_apply*/,
  nullptr /*AnnotArray_dictionary_get_bool*/,
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

VariantFunctions *
sourcekitd::getVariantFunctionsForExpressionTypeArray() {
 return &ExpressionTypeArrayBuilder::Funcs;
}
