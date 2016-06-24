//===--- sourcekitdAPI-InProc.cpp -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "sourcekitd/sourcekitd.h"
#include "sourcekitd/Internal.h"
#include "SourceKit/Support/UIdent.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/ErrorHandling.h"

using namespace SourceKit;
using namespace sourcekitd;
using llvm::ArrayRef;
using llvm::StringRef;
using llvm::raw_ostream;

// The following functions are declared in sourcekitd/sourcekitd.h.
// These are their "in-process" implementations.

sourcekitd_uid_t
sourcekitd_uid_get_from_buf(const char *buf, size_t length) {
  llvm::report_fatal_error("not yet implemented");
}

size_t
sourcekitd_uid_get_length(sourcekitd_uid_t uid) {
  llvm::report_fatal_error("not yet implemented");
}

const char *
sourcekitd_uid_get_string_ptr(sourcekitd_uid_t uid) {
  llvm::report_fatal_error("not yet implemented");
}

sourcekitd_object_t
sourcekitd_request_retain(sourcekitd_object_t object) {
  llvm::report_fatal_error("not yet implemented");
}

void sourcekitd_request_release(sourcekitd_object_t object) {
  llvm::report_fatal_error("not yet implemented");
}

sourcekitd_object_t
sourcekitd_request_dictionary_create(const sourcekitd_uid_t *keys,
                                     const sourcekitd_object_t *values,
                                     size_t count) {
  llvm::report_fatal_error("not yet implemented");
}

void
sourcekitd_request_dictionary_set_value(sourcekitd_object_t dict,
                                        sourcekitd_uid_t key,
                                        sourcekitd_object_t value) {
  llvm::report_fatal_error("not yet implemented");
}

sourcekitd_object_t
sourcekitd_request_array_create(const sourcekitd_object_t *objects,
                                size_t count) {
  llvm::report_fatal_error("not yet implemented");
}

void
sourcekitd_request_array_set_value(sourcekitd_object_t array, size_t index,
                                   sourcekitd_object_t value) {
  llvm::report_fatal_error("not yet implemented");
}

sourcekitd_object_t
sourcekitd_request_int64_create(int64_t val) {
  llvm::report_fatal_error("not yet implemented");
}

sourcekitd_object_t
sourcekitd_request_string_create(const char *string) {
  llvm::report_fatal_error("not yet implemented");
}

sourcekitd_object_t
sourcekitd_request_uid_create(sourcekitd_uid_t uid) {
  llvm::report_fatal_error("not yet implemented");
}

void
sourcekitd_response_dispose(sourcekitd_response_t obj) {
  llvm::report_fatal_error("not yet implemented");
}

bool
sourcekitd_response_is_error(sourcekitd_response_t obj) {
  llvm::report_fatal_error("not yet implemented");
}

sourcekitd_error_t
sourcekitd_response_error_get_kind(sourcekitd_response_t obj) {
  llvm::report_fatal_error("not yet implemented");
}

const char *
sourcekitd_response_error_get_description(sourcekitd_response_t obj) {
  llvm::report_fatal_error("not yet implemented");
}

sourcekitd_variant_t
sourcekitd_response_get_value(sourcekitd_response_t resp) {
  llvm::report_fatal_error("not yet implemented");
}

// The following functions are declared in sourcekitd/Internal.h.
// These are their "in-process" implementations.

void sourcekitd::printRequestObject(sourcekitd_object_t Obj, raw_ostream &OS) {
  llvm::report_fatal_error("not yet implemented");
}

ResponseBuilder::ResponseBuilder() {
  llvm::report_fatal_error("not yet implemented");
}

ResponseBuilder::~ResponseBuilder() {
  llvm::report_fatal_error("not yet implemented");
}

ResponseBuilder::Dictionary ResponseBuilder::getDictionary() {
  llvm::report_fatal_error("not yet implemented");
}

sourcekitd_response_t ResponseBuilder::createResponse() {
  llvm::report_fatal_error("not yet implemented");
}

void ResponseBuilder::Dictionary::set(UIdent Key, SourceKit::UIdent UID) {
  llvm::report_fatal_error("not yet implemented");
}

void ResponseBuilder::Dictionary::set(UIdent Key, sourcekitd_uid_t UID) {
  llvm::report_fatal_error("not yet implemented");
}

void ResponseBuilder::Dictionary::set(UIdent Key, const char *Str) {
  llvm::report_fatal_error("not yet implemented");
}

void ResponseBuilder::Dictionary::set(UIdent Key, llvm::StringRef Str) {
  llvm::report_fatal_error("not yet implemented");
}

void ResponseBuilder::Dictionary::set(UIdent Key, int64_t val) {
  llvm::report_fatal_error("not yet implemented");
}

void ResponseBuilder::Dictionary::set(SourceKit::UIdent Key,
                                      ArrayRef<StringRef> Strs) {
  llvm::report_fatal_error("not yet implemented");
}

void ResponseBuilder::Dictionary::setBool(UIdent Key, bool val) {
  llvm::report_fatal_error("not yet implemented");
}

ResponseBuilder::Dictionary
ResponseBuilder::Dictionary::setDictionary(UIdent Key) {
  llvm::report_fatal_error("not yet implemented");
}

void ResponseBuilder::Dictionary::setCustomBuffer(
      SourceKit::UIdent Key,
      CustomBufferKind Kind, std::unique_ptr<llvm::MemoryBuffer> MemBuf) {
  llvm::report_fatal_error("not yet implemented");
}

ResponseBuilder::Array
ResponseBuilder::Dictionary::setArray(UIdent Key) {
  llvm::report_fatal_error("not yet implemented");
}

ResponseBuilder::Dictionary ResponseBuilder::Array::appendDictionary() {
  llvm::report_fatal_error("not yet implemented");
}

sourcekitd_uid_t RequestDict::getUID(UIdent Key) {
  llvm::report_fatal_error("not yet implemented");
}


Optional<StringRef> RequestDict::getString(UIdent Key) {
  llvm::report_fatal_error("not yet implemented");
}

Optional<RequestDict> RequestDict::getDictionary(SourceKit::UIdent Key) {
  llvm::report_fatal_error("not yet implemented");
}

bool RequestDict::getStringArray(SourceKit::UIdent Key,
                                 llvm::SmallVectorImpl<const char *> &Arr,
                                 bool isOptional) {
  llvm::report_fatal_error("not yet implemented");
}

bool RequestDict::getUIDArray(SourceKit::UIdent Key,
                              llvm::SmallVectorImpl<sourcekitd_uid_t> &Arr,
                              bool isOptional) {
  llvm::report_fatal_error("not yet implemented");
}

bool RequestDict::dictionaryArrayApply(
    SourceKit::UIdent key, llvm::function_ref<bool(RequestDict)> applier) {
  llvm::report_fatal_error("not yet implemented");
}

bool RequestDict::getInt64(SourceKit::UIdent Key, int64_t &Val,
                           bool isOptional) {
  llvm::report_fatal_error("not yet implemented");
}

sourcekitd_response_t
sourcekitd::createErrorRequestInvalid(const char *Description) {
  llvm::report_fatal_error("not yet implemented");
}

sourcekitd_response_t
sourcekitd::createErrorRequestFailed(const char *Description) {
  llvm::report_fatal_error("not yet implemented");
}

sourcekitd_response_t
sourcekitd::createErrorRequestCancelled() {
  llvm::report_fatal_error("not yet implemented");
}
