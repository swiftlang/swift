//===--- RawData.cpp ------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "sourcekitd/RawData.h"

using namespace sourcekitd;

struct RawDataFuncs {
  static sourcekitd_variant_type_t get_type(sourcekitd_variant_t var) {
    return SOURCEKITD_VARIANT_TYPE_DATA;
  }

  static size_t data_get_size(sourcekitd_variant_t variant) {
    return variant.data[2];
  }

  static const void *data_get_ptr(sourcekitd_variant_t variant) {
    return reinterpret_cast<void *>(variant.data[1]);
  }

  static VariantFunctions Funcs;
};

VariantFunctions RawDataFuncs::Funcs = {
  get_type,
  nullptr /*AnnotArray_array_apply*/,
  nullptr /*AnnotArray_array_get_bool*/,
  nullptr /*Annot_array_array_get_count*/,
  nullptr /*AnnotArray_array_get_int64*/,
  nullptr /*AnnotArray_array_get_string*/,
  nullptr /*AnnotArray_array_get_uid*/,
  nullptr /*AnnotArray_array_get_value*/,
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
  data_get_size,
  data_get_ptr,
};

VariantFunctions *sourcekitd::getVariantFunctionsForRawData() {
  return &RawDataFuncs::Funcs;
}
