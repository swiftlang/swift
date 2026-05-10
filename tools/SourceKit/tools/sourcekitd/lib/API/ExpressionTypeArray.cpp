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
#include "sourcekitd/DictionaryKeys.h"
#include "SourceKit/Core/LLVM.h"
#include "SourceKit/Core/LangSupport.h"
#include "SourceKit/Support/UIdent.h"

#include "llvm/Support/MemoryBuffer.h"

using namespace SourceKit;
using namespace sourcekitd;

namespace {
class ExpressionTypeReader {
  const char *printedType;
  // Five unsigned integers for:
  //  - Expression offset in the source buffer
  //  - Expression length in the source buffer
  //  - Offset of printed expression type inside printedType
  //  - Offset of the first conforming protocol in the protocol buffer
  //  - Number of conforming protocols
  CompactArrayReader<unsigned, unsigned, unsigned, unsigned, unsigned> entryReader;

  // Offsets inside printedType where a protocol name starts.
  CompactArrayReader<unsigned> protoReader;

  static uint64_t getHeaderValue(char *buffer, unsigned index) {
    uint64_t headerField;
    memcpy(&headerField, (uint64_t*)buffer + index, sizeof(headerField));
    return headerField;
  }

public:
  ExpressionTypeReader(char *buffer):
      entryReader(buffer + getHeaderValue(buffer, 0)),
      protoReader(buffer + getHeaderValue(buffer, 2)) {
    // Read the printed type string buffer here.
    CompactArrayReader<const char*> reader(buffer + getHeaderValue(buffer, 1));
    reader.readEntries(0, printedType);
  }

  uint64_t count() const { return entryReader.getCount(); }

  ExpressionType getExpression(uint64_t idx) {
    ExpressionType result;
    unsigned protoStart, protoCount;
    entryReader.readEntries(idx, result.ExprOffset, result.ExprLength,
                            result.TypeOffset, protoStart, protoCount);
    for (unsigned i = protoStart, n = protoStart + protoCount; i < n; i ++) {
      result.ProtocolOffsets.emplace_back();
      protoReader.readEntries(i, result.ProtocolOffsets.back());
    }
    return result;
  }

  const char* readPrintedType(unsigned offset) {
    return printedType + offset;
  }

  static bool
  dictionary_apply(void *buffer, size_t index,
                   sourcekitd_variant_dictionary_applier_f_t applier,
                   void *context) {
    ExpressionTypeReader reader((char*)buffer);
    auto result = reader.getExpression(index);
#define APPLY(K, Ty, Field)                                                    \
  do {                                                                         \
    sourcekitd_uid_t key = SKDUIDFromUIdent(K);                                \
    sourcekitd_variant_t var = make##Ty##Variant(Field);                       \
    if (!applier(key, var, context))                                           \
      return false;                                                            \
  } while (0)

#define APPLY_ARRAY(Kind, Key)                                                 \
  do {                                                                         \
    sourcekitd_uid_t key = SKDUIDFromUIdent(Key);                              \
    sourcekitd_variant_t var = {                                               \
        {(uintptr_t)getVariantFunctionsFor##Kind##Array(), (uintptr_t)buffer,  \
         index}};                                                              \
    if (!applier(key, var, context))                                           \
      return false;                                                            \
  } while (0)

    APPLY(KeyExpressionOffset, Int, result.ExprOffset);
    APPLY(KeyExpressionLength, Int, result.ExprLength);
    APPLY(KeyExpressionType, String, reader.readPrintedType(result.TypeOffset));
    APPLY_ARRAY(ProtocolName, KeyExpectedTypes);
    return true;
  }
};

// data[0] = ProtocolListFuncs::funcs
// data[1] = custom buffer
// data[2] = offset for the element in ExpressionTypeArray
struct ProtocolListFuncs {
  static sourcekitd_variant_type_t get_type(sourcekitd_variant_t var) {
    return SOURCEKITD_VARIANT_TYPE_ARRAY;
  }

  static size_t array_get_count(sourcekitd_variant_t array) {
    char *buffer = (char*)array.data[1];
    size_t offset = array.data[2];
    return ExpressionTypeReader(buffer).getExpression(offset).
      ProtocolOffsets.size();
  }

  static sourcekitd_variant_t array_get_value(sourcekitd_variant_t array,
                                              size_t index) {
    char *buffer = (char*)array.data[1];
    size_t offset = array.data[2];
    ExpressionTypeReader reader(buffer);
    return makeStringVariant(reader.readPrintedType((reader.getExpression(offset).
      ProtocolOffsets[index])));
  }

  static VariantFunctions Funcs;
};
}// end of anonymous namespace

VariantFunctions ProtocolListFuncs::Funcs = {
    get_type,
    nullptr /*AnnotArray_array_apply*/,
    nullptr /*AnnotArray_array_get_bool*/,
    nullptr /*AnnotArray_array_get_double*/,
    array_get_count,
    nullptr /*AnnotArray_array_get_int64*/,
    nullptr /*AnnotArray_array_get_string*/,
    nullptr /*AnnotArray_array_get_uid*/,
    array_get_value,
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

struct ExpressionTypeArrayBuilder::Implementation {
  StringRef printedType;
  SmallVector<char, 256> buffer;
  CompactArrayBuilder<unsigned, unsigned, unsigned, unsigned, unsigned> builder;
  CompactArrayBuilder<StringRef> strBuilder;
  CompactArrayBuilder<unsigned> protoBuilder;

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
  // data[1] = custom buffer
  static size_t array_get_count(sourcekitd_variant_t array) {
    ExpressionTypeReader reader((char*)array.data[1]);
    return reader.count();
  }

  std::unique_ptr<llvm::MemoryBuffer> createBuffer(CustomBufferKind Kind) {
    std::array<CompactArrayBuilderImpl*, 3> builders =
      {&builder, &strBuilder, &protoBuilder};
    auto kindSize = sizeof(uint64_t);
    size_t headerSize = sizeof(uint64_t) * builders.size();
    auto allSize = kindSize + headerSize;
    for (auto *b: builders)
      allSize += b->sizeInBytes();
    auto result = llvm::WritableMemoryBuffer::getNewUninitMemBuffer(allSize);
    *reinterpret_cast<uint64_t*>(result->getBufferStart()) = (uint64_t)Kind;

    char *start = result->getBufferStart() + kindSize;
    char *headerPtr = start;
    char *ptr = start + headerSize;
    auto addBuilder = [&](CompactArrayBuilderImpl& buffer) {
      uint64_t offset = ptr - start;
      memcpy(headerPtr, &offset, sizeof(offset));
      headerPtr += sizeof(offset);
      ptr += buffer.copyInto(ptr);
    };
    for (auto *b: builders) {
      addBuilder(*b);
    }
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
  auto protoStart = Impl.protoBuilder.size();
  // Add protocol name starts and length to the protocol buffer.
  for (auto off: expType.ProtocolOffsets) {
    Impl.protoBuilder.addEntry(off);
  }
  auto protoCount = Impl.protoBuilder.size() - protoStart;
  Impl.builder.addEntry(expType.ExprOffset, expType.ExprLength,
                        expType.TypeOffset/*Printed type is null ended*/,
                        protoStart/*Index of first protocol in the protocol buffer*/,
                        protoCount/*Number of conforming protocols*/);
}

std::unique_ptr<llvm::MemoryBuffer>
ExpressionTypeArrayBuilder::createBuffer() {
  return Impl.createBuffer(CustomBufferKind::ExpressionTypeArray);
}

VariantFunctions ExpressionTypeArrayBuilder::Funcs = {
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

VariantFunctions *
sourcekitd::getVariantFunctionsForExpressionTypeArray() {
 return &ExpressionTypeArrayBuilder::Funcs;
}

VariantFunctions *
sourcekitd::getVariantFunctionsForProtocolNameArray() {
  return &ProtocolListFuncs::Funcs;
}
