//===--- sourcekitdAPI-XPC.cpp --------------------------------------------===//
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

#include "SourceKit/Support/UIdent.h"
#include "sourcekitd/CodeCompletionResultsArray.h"
#include "sourcekitd/DeclarationsArray.h"
#include "sourcekitd/DictionaryKeys.h"
#include "sourcekitd/DocStructureArray.h"
#include "sourcekitd/DocSupportAnnotationArray.h"
#include "sourcekitd/ExpressionTypeArray.h"
#include "sourcekitd/RawData.h"
#include "sourcekitd/RequestResponsePrinterBase.h"
#include "sourcekitd/TokenAnnotationsArray.h"
#include "sourcekitd/VariableTypeArray.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MemoryBuffer.h"
#include <vector>
#include <xpc/xpc.h>

using namespace SourceKit;
using namespace sourcekitd;
using llvm::ArrayRef;
using llvm::StringRef;
using llvm::raw_ostream;

namespace {

class CustomXPCData {
public:
  enum class Kind : char {
    ErrorRequestInvalid,
    ErrorRequestFailed,
    ErrorRequestInterrupted,
    ErrorRequestCancelled
  };

  explicit CustomXPCData(xpc_object_t xobj) : XObj(xobj) {
    assert(xpc_get_type(XObj) == XPC_TYPE_DATA);
    assert(getLength() > 0);
  }

  xpc_object_t getXObj() const { return XObj; }

  Kind getKind() const { return Kind(getPtr()[0]); }

  const char *getPtr() const {
    return (const char *)xpc_data_get_bytes_ptr(XObj);
  }
  size_t getLength() const {
    return xpc_data_get_length(XObj);
  }

  bool isError() const {
    return true; // No other kind for now.
  }

  const char *getErrorDescription() const {
    assert(isError());
    assert(getLength() > 1);
    return getPtr()+1;
  }

  static CustomXPCData createErrorRequestInvalid(StringRef Description) {
    return createKindAndString(Kind::ErrorRequestInvalid, Description);
  }
  static CustomXPCData createErrorRequestFailed(StringRef Description) {
    return createKindAndString(Kind::ErrorRequestFailed, Description);
  }
  static CustomXPCData createErrorRequestInterrupted(StringRef Description) {
    return createKindAndString(Kind::ErrorRequestInterrupted, Description);
  }
  static CustomXPCData createErrorRequestCancelled(StringRef Description) {
    return createKindAndString(Kind::ErrorRequestCancelled, Description);
  }

  static CustomXPCData createKindAndString(Kind K, StringRef Str) {
    llvm::SmallVector<char, 128> Buf;
    Buf.push_back((char)K);
    Buf.append(Str.begin(), Str.end());
    Buf.push_back('\0');
    return CustomXPCData(xpc_data_create(Buf.begin(), Buf.size()));
  }

  xpc_object_t XObj;
};

template <typename ImplClass, typename RetTy = void>
class SKDObjectVisitor {
public:
  typedef std::vector<std::pair<UIdent, sourcekitd_object_t>> DictMap;

  static bool compKeys(const std::pair<UIdent, sourcekitd_object_t> &LHS,
                       const std::pair<UIdent, sourcekitd_object_t> &RHS) {
    return sourcekitd::compareDictKeys(LHS.first, RHS.first);
  }

  RetTy visit(sourcekitd_object_t Obj) {
    xpc_type_t XType = xpc_get_type(Obj);
    if (XType == XPC_TYPE_DICTIONARY) {
      DictMap Dict;
      DictMap &DictRef = Dict;
      xpc_dictionary_apply(Obj, ^(const char *Key, xpc_object_t Value) {
        UIdent UID = UIdent(Key);
        DictRef.push_back({ UID, Value });
        return true;
      });
      std::sort(Dict.begin(), Dict.end(), compKeys);
      return static_cast<ImplClass*>(this)->visitDictionary(Dict);
    }
    if (XType == XPC_TYPE_ARRAY) {
      std::vector<sourcekitd_object_t> Vec;
      for (size_t i = 0, e = xpc_array_get_count(Obj); i != e; ++i)
        Vec.push_back(xpc_array_get_value(Obj, i));
      return static_cast<ImplClass*>(this)->visitArray(Vec);
    }
    if (XType == XPC_TYPE_INT64)
      return static_cast<ImplClass*>(this)->visitInt64(xpc_int64_get_value(Obj));
    if (XType == XPC_TYPE_STRING) {
      size_t Len = xpc_string_get_length(Obj);
      const char *Ptr = xpc_string_get_string_ptr(Obj);
      return static_cast<ImplClass*>(this)->visitString(StringRef(Ptr, Len));
    }
    if (XType == XPC_TYPE_UINT64) {
      sourcekitd_uid_t SKDUID = sourcekitd_uid_t(xpc_uint64_get_value(Obj));
      UIdent UID = UIdentFromSKDUID(SKDUID);
      return static_cast<ImplClass*>(this)->visitUID(UID.getName());
    }

    llvm_unreachable("unknown sourcekitd_object_t");
  }
};

class SKDObjectPrinter : public SKDObjectVisitor<SKDObjectPrinter>,
                         public RequestResponsePrinterBase<SKDObjectPrinter,
                                                           sourcekitd_object_t> {
public:
  SKDObjectPrinter(raw_ostream &OS, unsigned Indent = 0)
    : RequestResponsePrinterBase(OS, Indent) { }
};

} // anonymous namespace

void sourcekitd::printRequestObject(sourcekitd_object_t Obj, raw_ostream &OS) {
  if (!Obj) {
    OS << "<<NULL>>";
    return;
  }

  SKDObjectPrinter(OS).visit(Obj);
}

//===----------------------------------------------------------------------===//
// Internal API
//===----------------------------------------------------------------------===//

ResponseBuilder::ResponseBuilder() {
  Impl = xpc_dictionary_create_empty();
}

ResponseBuilder::~ResponseBuilder() {
  xpc_release(Impl);
}

ResponseBuilder::ResponseBuilder(const ResponseBuilder &Other) {
  Impl = xpc_retain(Other.Impl);
}

ResponseBuilder &ResponseBuilder::operator =(const ResponseBuilder &Other) {
  xpc_object_t Tmp = xpc_retain(Other.Impl);
  xpc_release(Impl);
  Impl = Tmp;
  return *this;
}

ResponseBuilder::Dictionary ResponseBuilder::getDictionary() {
  return Dictionary(Impl);
}

sourcekitd_response_t ResponseBuilder::createResponse() {
  return xpc_retain(Impl);
}

void ResponseBuilder::Dictionary::set(UIdent Key, SourceKit::UIdent UID) {
  set(Key, SKDUIDFromUIdent(UID));
}

void ResponseBuilder::Dictionary::set(UIdent Key, sourcekitd_uid_t UID) {
  xpc_dictionary_set_uint64(Impl, Key.c_str(), uintptr_t(UID));
}

void ResponseBuilder::Dictionary::set(UIdent Key, const char *Str) {
  xpc_dictionary_set_string(Impl, Key.c_str(), Str);
}

void ResponseBuilder::Dictionary::set(UIdent Key, llvm::StringRef Str) {
  llvm::SmallString<512> Buf(Str);
  xpc_dictionary_set_string(Impl, Key.c_str(), Buf.c_str());
}

void ResponseBuilder::Dictionary::set(UIdent Key, const std::string &Str) {
  xpc_dictionary_set_string(Impl, Key.c_str(), Str.c_str());
}

void ResponseBuilder::Dictionary::set(UIdent Key, int64_t val) {
  xpc_dictionary_set_int64(Impl, Key.c_str(), val);
}

void ResponseBuilder::Dictionary::set(SourceKit::UIdent Key,
                                      ArrayRef<StringRef> Strs) {
  llvm::SmallString<128> Buf;
  xpc_object_t arr = xpc_array_create_empty();
  for (auto Str : Strs) {
    Buf = Str;
    xpc_array_set_string(arr, XPC_ARRAY_APPEND, Buf.c_str());
  }
  xpc_dictionary_set_value(Impl, Key.c_str(), arr);
  xpc_release(arr);
}

void ResponseBuilder::Dictionary::set(SourceKit::UIdent Key,
                                      ArrayRef<std::string> Strs) {
  xpc_object_t arr = xpc_array_create_empty();
  for (auto Str : Strs) {
    xpc_array_set_string(arr, XPC_ARRAY_APPEND, Str.c_str());
  }
  xpc_dictionary_set_value(Impl, Key.c_str(), arr);
  xpc_release(arr);
}

void ResponseBuilder::Dictionary::set(SourceKit::UIdent Key,
                                      ArrayRef<SourceKit::UIdent> UIDs) {
  xpc_object_t arr = xpc_array_create_empty();
  for (auto UID : UIDs) {
    xpc_array_set_uint64(arr, XPC_ARRAY_APPEND, uintptr_t(SKDUIDFromUIdent(UID)));
  }
  xpc_dictionary_set_value(Impl, Key.c_str(), arr);
  xpc_release(arr);
}

void ResponseBuilder::Dictionary::setBool(UIdent Key, bool val) {
  xpc_dictionary_set_bool(Impl, Key.c_str(), val);
}

ResponseBuilder::Array
ResponseBuilder::Dictionary::setArray(UIdent Key) {
  xpc_object_t arr = xpc_array_create_empty();
  xpc_dictionary_set_value(Impl, Key.c_str(), arr);
  xpc_release(arr);
  return Array(arr);
}

ResponseBuilder::Dictionary
ResponseBuilder::Dictionary::setDictionary(UIdent Key) {
  xpc_object_t dict = xpc_dictionary_create_empty();
  xpc_dictionary_set_value(Impl, Key.c_str(), dict);
  xpc_release(dict);
  return Dictionary(dict);
}

void ResponseBuilder::Dictionary::setCustomBuffer(
    SourceKit::UIdent Key, std::unique_ptr<llvm::MemoryBuffer> Buf) {
  xpc_object_t xdata = xpc_data_create(Buf->getBufferStart(),
                                       Buf->getBufferSize());
  xpc_dictionary_set_value(Impl, Key.c_str(), xdata);
  xpc_release(xdata);
}

ResponseBuilder::Dictionary ResponseBuilder::Array::appendDictionary() {
  xpc_object_t dict = xpc_dictionary_create_empty();
  xpc_array_append_value(Impl, dict);
  xpc_release(dict);
  return Dictionary(dict);
}

sourcekitd_uid_t RequestDict::getUID(UIdent Key) const {
  return sourcekitd_uid_t(xpc_dictionary_get_uint64(Dict, Key.c_str()));
}

std::optional<StringRef> RequestDict::getString(UIdent Key) const {
  xpc_object_t xobj = xpc_dictionary_get_value(Dict, Key.c_str());
  if (!xobj)
    return std::nullopt;
  if (xpc_get_type(xobj) != XPC_TYPE_STRING)
    return std::nullopt;
  return StringRef(xpc_string_get_string_ptr(xobj),
                   xpc_string_get_length(xobj));
}

std::optional<RequestDict>
RequestDict::getDictionary(SourceKit::UIdent Key) const {
  xpc_object_t xobj = xpc_dictionary_get_value(Dict, Key.c_str());
  if (!xobj)
    return std::nullopt;
  if (xpc_get_type(xobj) != XPC_TYPE_DICTIONARY)
    return std::nullopt;
  return RequestDict(xobj);
}

bool RequestDict::getStringArray(SourceKit::UIdent Key,
                                 llvm::SmallVectorImpl<const char *> &Arr,
                                 bool isOptional) const {
  xpc_object_t xarr = xpc_dictionary_get_value(Dict, Key.c_str());
  if (!xarr)
    return !isOptional;
  if (xpc_get_type(xarr) != XPC_TYPE_ARRAY)
    return true;
  size_t count = xpc_array_get_count(xarr);
  Arr.reserve(count);
  for (size_t i = 0; i != count; ++i) {
    const char *Str = xpc_array_get_string(xarr, i);
    if (!Str)
      return true;
    Arr.push_back(Str);
  }
  return false;
}

bool RequestDict::getUIDArray(SourceKit::UIdent Key,
                              llvm::SmallVectorImpl<sourcekitd_uid_t> &Arr,
                              bool isOptional) const {
  xpc_object_t xarr = xpc_dictionary_get_value(Dict, Key.c_str());
  if (!xarr)
    return !isOptional;
  if (xpc_get_type(xarr) != XPC_TYPE_ARRAY)
    return true;
  size_t count = xpc_array_get_count(xarr);
  Arr.reserve(count);
  for (size_t i = 0; i != count; ++i) {
    auto UID = sourcekitd_uid_t(xpc_array_get_uint64(xarr, i));
    if (!UID)
      return true;
    Arr.push_back(UID);
  }
  return false;
}

bool RequestDict::dictionaryArrayApply(
    SourceKit::UIdent key,
    llvm::function_ref<bool(RequestDict)> applier) const {
  xpc_object_t xarr = xpc_dictionary_get_value(Dict, key.c_str());
  if (!xarr || xpc_get_type(xarr) != XPC_TYPE_ARRAY)
    return true;
  size_t count = xpc_array_get_count(xarr);
  for (size_t i = 0; i != count; ++i) {
    auto xdict = xpc_array_get_value(xarr, i);
    if (!xdict || xpc_get_type(xdict) != XPC_TYPE_DICTIONARY)
      return true;
    if (applier(RequestDict(xdict)))
      return true;
  }
  return false;
}

bool RequestDict::getInt64(SourceKit::UIdent Key, int64_t &Val,
                           bool isOptional) const {
  xpc_object_t xobj = xpc_dictionary_get_value(Dict, Key.c_str());
  if (!xobj)
    return !isOptional;
  Val = xpc_int64_get_value(xobj);
  return false;
}

std::optional<int64_t>
RequestDict::getOptionalInt64(SourceKit::UIdent Key) const {
  xpc_object_t xobj = xpc_dictionary_get_value(Dict, Key.c_str());
  if (!xobj)
    return std::nullopt;
  return xpc_int64_get_value(xobj);
}

sourcekitd_response_t
sourcekitd::createErrorRequestInvalid(StringRef Description) {
  return CustomXPCData::createErrorRequestInvalid(Description).getXObj();
}
sourcekitd_response_t
sourcekitd::createErrorRequestFailed(StringRef Description) {
  return CustomXPCData::createErrorRequestFailed(Description).getXObj();
}
sourcekitd_response_t
sourcekitd::createErrorRequestInterrupted(StringRef Description) {
  return CustomXPCData::createErrorRequestInterrupted(Description).getXObj();
}
sourcekitd_response_t
sourcekitd::createErrorRequestCancelled() {
  return CustomXPCData::createErrorRequestCancelled(StringRef("")).getXObj();
}

//===----------------------------------------------------------------------===//
// Public Request API
//===----------------------------------------------------------------------===//

static inline const char *strFromUID(sourcekitd_uid_t uid) {
  return UIdentFromSKDUID(uid).c_str();
}

sourcekitd_object_t
sourcekitd_request_retain(sourcekitd_object_t object) {
  return xpc_retain(object);
}

void sourcekitd_request_release(sourcekitd_object_t object) {
  xpc_release(object);
}

sourcekitd_object_t
sourcekitd_request_dictionary_create(const sourcekitd_uid_t *keys,
                                     const sourcekitd_object_t *values,
                                     size_t count) {
  llvm::SmallVector<const char *, 8> Keys;
  Keys.reserve(count);
  for (size_t i = 0; i < count; ++i)
    Keys.push_back(strFromUID(keys[i]));
  return xpc_dictionary_create(Keys.data(), values, count);
}

void
sourcekitd_request_dictionary_set_value(sourcekitd_object_t dict,
                                        sourcekitd_uid_t key,
                                        sourcekitd_object_t value) {
  xpc_dictionary_set_value(dict, strFromUID(key), value);
}

void sourcekitd_request_dictionary_set_string(sourcekitd_object_t dict,
                                              sourcekitd_uid_t key,
                                              const char *string) {
  xpc_dictionary_set_string(dict, strFromUID(key), string);
}

void
sourcekitd_request_dictionary_set_stringbuf(sourcekitd_object_t dict,
                                            sourcekitd_uid_t key,
                                            const char *buf, size_t length) {
  llvm::SmallString<512> SS;
  SS += StringRef(buf, length);
  sourcekitd_request_dictionary_set_string(dict, key, SS.c_str());
}

void sourcekitd_request_dictionary_set_int64(sourcekitd_object_t dict,
                                             sourcekitd_uid_t key,
                                             int64_t val) {
  xpc_dictionary_set_int64(dict, strFromUID(key), val);
}

void
sourcekitd_request_dictionary_set_uid(sourcekitd_object_t dict,
                                      sourcekitd_uid_t key,
                                      sourcekitd_uid_t uid) {
  xpc_dictionary_set_uint64(dict, strFromUID(key), uintptr_t(uid));
}

sourcekitd_object_t
sourcekitd_request_dictionary_get_value(sourcekitd_object_t dict,
                                        sourcekitd_uid_t key) {
  return xpc_dictionary_get_value(dict, strFromUID(key));
}

const char *sourcekitd_request_dictionary_get_string(sourcekitd_object_t dict,
                                                     sourcekitd_uid_t key) {
  return xpc_dictionary_get_string(dict, strFromUID(key));
}

int64_t sourcekitd_request_dictionary_get_int64(sourcekitd_object_t dict,
                                                sourcekitd_uid_t key) {
  return xpc_dictionary_get_int64(dict, strFromUID(key));
}

bool sourcekitd_request_dictionary_get_bool(sourcekitd_object_t dict,
                                            sourcekitd_uid_t key) {
  return xpc_dictionary_get_bool(dict, strFromUID(key));
}

sourcekitd_uid_t sourcekitd_request_dictionary_get_uid(sourcekitd_object_t dict,
                                                       sourcekitd_uid_t key) {
  return (sourcekitd_uid_t)xpc_dictionary_get_uint64(dict, strFromUID(key));
}

size_t sourcekitd_request_array_get_count(sourcekitd_object_t array) {
  return xpc_array_get_count(array);
}

sourcekitd_object_t
sourcekitd_request_array_get_value(sourcekitd_object_t array, size_t index) {
  return xpc_array_get_value(array, index);
}

const char *sourcekitd_request_array_get_string(sourcekitd_object_t array,
                                                size_t index) {
  return xpc_array_get_string(array, index);
}

int64_t sourcekitd_request_array_get_int64(sourcekitd_object_t array,
                                           size_t index) {
  return xpc_array_get_int64(array, index);
}

bool sourcekitd_request_array_get_bool(sourcekitd_object_t array,
                                       size_t index) {
  return xpc_array_get_bool(array, index);
}

sourcekitd_uid_t sourcekitd_request_array_get_uid(sourcekitd_object_t array,
                                                  size_t index) {
  return (sourcekitd_uid_t)xpc_array_get_uint64(array, index);
}

int64_t sourcekitd_request_int64_get_value(sourcekitd_object_t obj) {
  return xpc_int64_get_value(obj);
}

bool sourcekitd_request_bool_get_value(sourcekitd_object_t obj) {
  return xpc_bool_get_value(obj);
}

size_t sourcekitd_request_string_get_length(sourcekitd_object_t obj) {
  return xpc_string_get_length(obj);
}

const char *sourcekitd_request_string_get_ptr(sourcekitd_object_t obj) {
  return xpc_string_get_string_ptr(obj);
}

sourcekitd_uid_t sourcekitd_request_uid_get_value(sourcekitd_object_t obj) {
  return (sourcekitd_uid_t)xpc_uint64_get_value(obj);
}

sourcekitd_object_t
sourcekitd_request_array_create(const sourcekitd_object_t *objects,
                                size_t count) {
  return xpc_array_create(objects, count);
}

void
sourcekitd_request_array_set_value(sourcekitd_object_t array, size_t index,
                                   sourcekitd_object_t value) {
  xpc_array_set_value(array, index, value);
}

void
sourcekitd_request_array_set_string(sourcekitd_object_t array, size_t index,
                                    const char *string) {
  xpc_array_set_string(array, index, string);
}

void
sourcekitd_request_array_set_stringbuf(sourcekitd_object_t array, size_t index,
                                       const char *buf, size_t length) {
  llvm::SmallString<512> SS;
  SS += StringRef(buf, length);
  sourcekitd_request_array_set_string(array, index, SS.c_str());
}

void
sourcekitd_request_array_set_int64(sourcekitd_object_t array, size_t index,
                                   int64_t val) {
  xpc_array_set_int64(array, index, val);
}

void
sourcekitd_request_array_set_uid(sourcekitd_object_t array, size_t index,
                                 sourcekitd_uid_t uid) {
  xpc_array_set_uint64(array, index, uintptr_t(uid));
}

sourcekitd_object_t
sourcekitd_request_int64_create(int64_t val) {
  return xpc_int64_create(val);
}

sourcekitd_object_t
sourcekitd_request_string_create(const char *string) {
  return xpc_string_create(string);
}

sourcekitd_object_t
sourcekitd_request_uid_create(sourcekitd_uid_t uid) {
  return xpc_uint64_create(uintptr_t(uid));
}

//===----------------------------------------------------------------------===//
// Public Response API
//===----------------------------------------------------------------------===//

void
sourcekitd_response_dispose(sourcekitd_response_t obj) {
  xpc_release(obj);
}

bool
sourcekitd_response_is_error(sourcekitd_response_t obj) {
  xpc_type_t type = xpc_get_type(obj);
  if (type == XPC_TYPE_ERROR)
    return true;
  if (type == XPC_TYPE_DATA) {
    CustomXPCData Dat(obj);
    return Dat.isError();
  }
  return false;
}

sourcekitd_error_t
sourcekitd_response_error_get_kind(sourcekitd_response_t obj) {
  xpc_type_t type = xpc_get_type(obj);
  if (type == XPC_TYPE_ERROR) {
    if (obj == XPC_ERROR_CONNECTION_INTERRUPTED)
      return SOURCEKITD_ERROR_CONNECTION_INTERRUPTED;
    // This can originate if the client initiated shutdown.
    if (obj == XPC_ERROR_CONNECTION_INVALID)
      return SOURCEKITD_ERROR_REQUEST_CANCELLED;
  }
  if (type == XPC_TYPE_DATA) {
    CustomXPCData Dat(obj);
    switch (Dat.getKind()) {
      case CustomXPCData::Kind::ErrorRequestInvalid:
        return SOURCEKITD_ERROR_REQUEST_INVALID;
      case CustomXPCData::Kind::ErrorRequestFailed:
        return SOURCEKITD_ERROR_REQUEST_FAILED;
      case CustomXPCData::Kind::ErrorRequestInterrupted:
        return SOURCEKITD_ERROR_CONNECTION_INTERRUPTED;
      case CustomXPCData::Kind::ErrorRequestCancelled:
        return SOURCEKITD_ERROR_REQUEST_CANCELLED;
    }
  }
  
  llvm::report_fatal_error("sourcekitd error did not resolve to a known kind");
}

const char *
sourcekitd_response_error_get_description(sourcekitd_response_t obj) {
  xpc_type_t type = xpc_get_type(obj);
  if (type == XPC_TYPE_ERROR)
    return xpc_dictionary_get_string(obj, XPC_ERROR_KEY_DESCRIPTION);
  if (type == XPC_TYPE_DATA) {
    CustomXPCData Dat(obj);
    if (Dat.isError())
      return Dat.getErrorDescription();
  }
  
  llvm::report_fatal_error("invalid sourcekitd error object");
}

static sourcekitd_variant_t variantFromXPCObject(xpc_object_t obj);

sourcekitd_variant_t
sourcekitd_response_get_value(sourcekitd_response_t resp) {
  if (sourcekitd_response_is_error(resp))
    return makeNullVariant();
  return variantFromXPCObject(resp);
}

//===----------------------------------------------------------------------===//
// Variant functions
//===----------------------------------------------------------------------===//

#define XPC_OBJ(var) ((xpc_object_t)(var).data[1])

#define CUSTOM_BUF_KIND(xobj) \
  ((CustomBufferKind)*(const uint64_t*)xpc_data_get_bytes_ptr(xobj))
#define CUSTOM_BUF_START(xobj) \
  ((const void*)(((const uint64_t*)xpc_data_get_bytes_ptr(xobj))+1))
#define CUSTOM_BUF_SIZE(xobj) (xpc_data_get_length(xobj) - sizeof(uint64_t))

static sourcekitd_variant_type_t XPCVar_get_type(sourcekitd_variant_t var) {
  xpc_object_t obj = XPC_OBJ(var);
  
  xpc_type_t type = xpc_get_type(obj);
  if (type == XPC_TYPE_DICTIONARY)
    return SOURCEKITD_VARIANT_TYPE_DICTIONARY;
  if (type == XPC_TYPE_ARRAY)
    return SOURCEKITD_VARIANT_TYPE_ARRAY;
  if (type == XPC_TYPE_INT64)
    return SOURCEKITD_VARIANT_TYPE_INT64;
  if (type == XPC_TYPE_BOOL)
    return SOURCEKITD_VARIANT_TYPE_BOOL;
  if (type == XPC_TYPE_DOUBLE)
    return SOURCEKITD_VARIANT_TYPE_DOUBLE;
  if (type == XPC_TYPE_STRING)
    return SOURCEKITD_VARIANT_TYPE_STRING;
  // Take over XPC's UINT64 to mean SourceKitD's UID.
  // This means that UINT64 will not be available to be constructed in a
  // sourcekitd message.
  if (type == XPC_TYPE_UINT64)
    return SOURCEKITD_VARIANT_TYPE_UID;

  if (type == XPC_TYPE_DATA) {
    auto BufKind = (size_t)CUSTOM_BUF_KIND(obj);
    switch (BufKind) {
    case (size_t)CustomBufferKind::TokenAnnotationsArray:
    case (size_t)CustomBufferKind::DeclarationsArray:
    case (size_t)CustomBufferKind::DocSupportAnnotationArray:
    case (size_t)CustomBufferKind::CodeCompletionResultsArray:
    case (size_t)CustomBufferKind::DocStructureArray:
    case (size_t)CustomBufferKind::InheritedTypesArray:
    case (size_t)CustomBufferKind::DocStructureElementArray:
    case (size_t)CustomBufferKind::AttributesArray:
    case (size_t)CustomBufferKind::ExpressionTypeArray:
    case (size_t)CustomBufferKind::VariableTypeArray:
      return SOURCEKITD_VARIANT_TYPE_ARRAY;
    case (size_t)CustomBufferKind::RawData:
      return SOURCEKITD_VARIANT_TYPE_DATA;
    default:
      return getPluginVariantFunctions(BufKind)->get_type(
          variantFromXPCObject(obj));
    }
  }

  llvm::report_fatal_error("sourcekitd object did not resolve to a known type");
}

static bool XPCVar_array_apply(sourcekitd_variant_t array,
                               sourcekitd_variant_array_applier_f_t applier,
                               void *context) {
  return xpc_array_apply(XPC_OBJ(array), ^(size_t index, xpc_object_t obj) {
    return applier(index, variantFromXPCObject(obj), context);
  });
}

static bool XPCVar_array_get_bool(sourcekitd_variant_t array, size_t index) {
  return xpc_array_get_bool(XPC_OBJ(array), index);
}

static double XPCVar_array_get_double(sourcekitd_variant_t array,
                                      size_t index) {
  return xpc_array_get_double(XPC_OBJ(array), index);
}

static size_t XPCVar_array_get_count(sourcekitd_variant_t array) {
  return xpc_array_get_count(XPC_OBJ(array));
}

static int64_t XPCVar_array_get_int64(sourcekitd_variant_t array, size_t index) {
  return xpc_array_get_int64(XPC_OBJ(array), index);
}

static const char *
XPCVar_array_get_string(sourcekitd_variant_t array, size_t index) {
  return xpc_array_get_string(XPC_OBJ(array), index);
}

static sourcekitd_uid_t XPCVar_array_get_uid(sourcekitd_variant_t array, size_t index) {
  return sourcekitd_uid_t(xpc_array_get_uint64(XPC_OBJ(array), index));
}

static sourcekitd_variant_t
XPCVar_array_get_value(sourcekitd_variant_t array, size_t index) {
  return variantFromXPCObject(xpc_array_get_value(XPC_OBJ(array), index));
}

static bool XPCVar_bool_get_value(sourcekitd_variant_t obj) {
  return xpc_bool_get_value(XPC_OBJ(obj));
}

static double XPCVar_double_get_value(sourcekitd_variant_t obj) {
  return xpc_double_get_value(XPC_OBJ(obj));
}

static bool
XPCVar_dictionary_apply(sourcekitd_variant_t dict,
                        sourcekitd_variant_dictionary_applier_f_t applier,
                        void *context) {
  return xpc_dictionary_apply(
      XPC_OBJ(dict), ^(const char *key, xpc_object_t obj) {
        return applier(sourcekitd_uid_get_from_cstr(key),
                       variantFromXPCObject(obj), context);
      });
}

static bool
XPCVar_dictionary_get_bool(sourcekitd_variant_t dict, sourcekitd_uid_t key) {
  return xpc_dictionary_get_bool(XPC_OBJ(dict), strFromUID(key));
}

static double XPCVar_dictionary_get_double(sourcekitd_variant_t dict,
                                           sourcekitd_uid_t key) {
  return xpc_dictionary_get_double(XPC_OBJ(dict), strFromUID(key));
}

static int64_t
XPCVar_dictionary_get_int64(sourcekitd_variant_t dict, sourcekitd_uid_t key) {
  return xpc_dictionary_get_int64(XPC_OBJ(dict), strFromUID(key));
}

static const char *
XPCVar_dictionary_get_string(sourcekitd_variant_t dict, sourcekitd_uid_t key) {
  return xpc_dictionary_get_string(XPC_OBJ(dict), strFromUID(key));
}

static sourcekitd_variant_t
XPCVar_dictionary_get_value(sourcekitd_variant_t dict, sourcekitd_uid_t key) {
  return variantFromXPCObject(
                xpc_dictionary_get_value(XPC_OBJ(dict), strFromUID(key)));
}

static sourcekitd_uid_t
XPCVar_dictionary_get_uid(sourcekitd_variant_t dict, sourcekitd_uid_t key) {
  return sourcekitd_uid_t(
               xpc_dictionary_get_uint64(XPC_OBJ(dict), strFromUID(key)));
}

static size_t XPCVar_string_get_length(sourcekitd_variant_t obj) {
  return xpc_string_get_length(XPC_OBJ(obj));
}

static const char *XPCVar_string_get_ptr(sourcekitd_variant_t obj) {
  return xpc_string_get_string_ptr(XPC_OBJ(obj));
}

static size_t XPCVar_data_get_size(sourcekitd_variant_t obj) {
  return xpc_data_get_length(XPC_OBJ(obj));
}

static const void *XPCVar_data_get_ptr(sourcekitd_variant_t obj) {
  return xpc_data_get_bytes_ptr(XPC_OBJ(obj));
}

static int64_t XPCVar_int64_get_value(sourcekitd_variant_t obj) {
  return xpc_int64_get_value(XPC_OBJ(obj));
}

static sourcekitd_uid_t XPCVar_uid_get_value(sourcekitd_variant_t obj) {
  return sourcekitd_uid_t(xpc_uint64_get_value(XPC_OBJ(obj)));
}

static VariantFunctions XPCVariantFuncs = {
    XPCVar_get_type,
    XPCVar_array_apply,
    XPCVar_array_get_bool,
    XPCVar_array_get_double,
    XPCVar_array_get_count,
    XPCVar_array_get_int64,
    XPCVar_array_get_string,
    XPCVar_array_get_uid,
    XPCVar_array_get_value,
    XPCVar_bool_get_value,
    XPCVar_double_get_value,
    XPCVar_dictionary_apply,
    XPCVar_dictionary_get_bool,
    XPCVar_dictionary_get_double,
    XPCVar_dictionary_get_int64,
    XPCVar_dictionary_get_string,
    XPCVar_dictionary_get_value,
    XPCVar_dictionary_get_uid,
    XPCVar_string_get_length,
    XPCVar_string_get_ptr,
    XPCVar_int64_get_value,
    XPCVar_uid_get_value,
    XPCVar_data_get_size,
    XPCVar_data_get_ptr,
};

static sourcekitd_variant_t variantFromXPCObject(xpc_object_t obj) {
  if (!obj)
    return makeNullVariant();

  if (xpc_get_type(obj) == XPC_TYPE_DATA) {
    auto BufKind = (size_t)CUSTOM_BUF_KIND(obj);
    switch (BufKind) {
    case (size_t)CustomBufferKind::TokenAnnotationsArray:
      return {{ (uintptr_t)getVariantFunctionsForTokenAnnotationsArray(),
                (uintptr_t)CUSTOM_BUF_START(obj), 0 }};
    case (size_t)CustomBufferKind::DeclarationsArray:
      return {{(uintptr_t)getVariantFunctionsForDeclarationsArray(),
               (uintptr_t)CUSTOM_BUF_START(obj), 0}};
    case (size_t)CustomBufferKind::DocSupportAnnotationArray:
      return {{ (uintptr_t)getVariantFunctionsForDocSupportAnnotationArray(),
                (uintptr_t)CUSTOM_BUF_START(obj), 0 }};
    case (size_t)CustomBufferKind::CodeCompletionResultsArray:
      return {{ (uintptr_t)getVariantFunctionsForCodeCompletionResultsArray(),
                (uintptr_t)CUSTOM_BUF_START(obj), 0 }};
    case (size_t)CustomBufferKind::DocStructureArray:
      return {{ (uintptr_t)getVariantFunctionsForDocStructureArray(),
                (uintptr_t)CUSTOM_BUF_START(obj), ~size_t(0) }};
    case (size_t)CustomBufferKind::InheritedTypesArray:
      return {{ (uintptr_t)getVariantFunctionsForInheritedTypesArray(),
                (uintptr_t)CUSTOM_BUF_START(obj), 0 }};
    case (size_t)CustomBufferKind::DocStructureElementArray:
      return {{ (uintptr_t)getVariantFunctionsForDocStructureElementArray(),
                (uintptr_t)CUSTOM_BUF_START(obj), 0 }};
    case (size_t)CustomBufferKind::AttributesArray:
      return {{ (uintptr_t)getVariantFunctionsForAttributesArray(),
                (uintptr_t)CUSTOM_BUF_START(obj), 0 }};
    case (size_t)CustomBufferKind::ExpressionTypeArray:
      return {{ (uintptr_t)getVariantFunctionsForExpressionTypeArray(),
                (uintptr_t)CUSTOM_BUF_START(obj), 0 }};
    case (size_t)CustomBufferKind::VariableTypeArray:
      return {{ (uintptr_t)getVariantFunctionsForVariableTypeArray(),
                (uintptr_t)CUSTOM_BUF_START(obj), 0 }};
    case (size_t)sourcekitd::CustomBufferKind::RawData:
      return {{ (uintptr_t)getVariantFunctionsForRawData(),
                (uintptr_t)CUSTOM_BUF_START(obj),
                (uintptr_t)CUSTOM_BUF_SIZE(obj) }};
    default:
      return {{(uintptr_t)getPluginVariantFunctions(BufKind),
               (uintptr_t)CUSTOM_BUF_START(obj),
               (uintptr_t)CUSTOM_BUF_SIZE(obj)}};
    }
  }

  return {{ (uintptr_t)&XPCVariantFuncs, (uintptr_t)obj, 0 }};
}

sourcekitd_response_t
sourcekitd_response_error_create(sourcekitd_error_t kind,
                                 const char *description) {
  CustomXPCData::Kind xpcKind;
  switch (kind) {
  case SOURCEKITD_ERROR_REQUEST_INVALID:
    xpcKind = CustomXPCData::Kind::ErrorRequestInvalid;
    break;
  case SOURCEKITD_ERROR_REQUEST_FAILED:
    xpcKind = CustomXPCData::Kind::ErrorRequestFailed;
    break;
  case SOURCEKITD_ERROR_CONNECTION_INTERRUPTED:
    xpcKind = CustomXPCData::Kind::ErrorRequestInterrupted;
    break;
  case SOURCEKITD_ERROR_REQUEST_CANCELLED:
    xpcKind = CustomXPCData::Kind::ErrorRequestCancelled;
    break;
  }
  return CustomXPCData::createKindAndString(xpcKind, description).getXObj();
}

sourcekitd_response_t
sourcekitd_response_dictionary_create(const sourcekitd_uid_t *keys,
                                      const sourcekitd_response_t *values,
                                      size_t count) {
  llvm::SmallVector<const char *, 8> Keys;
  Keys.reserve(count);
  for (size_t i = 0; i < count; ++i)
    Keys.push_back(strFromUID(keys[i]));
  return xpc_dictionary_create(Keys.data(), values, count);
}

void sourcekitd_response_dictionary_set_value(sourcekitd_response_t dict,
                                              sourcekitd_uid_t key,
                                              sourcekitd_response_t value) {
  xpc_dictionary_set_value(dict, strFromUID(key), value);
}

void sourcekitd_response_dictionary_set_string(sourcekitd_response_t dict,
                                               sourcekitd_uid_t key,
                                               const char *string) {
  xpc_dictionary_set_string(dict, strFromUID(key), string);
}

void sourcekitd_response_dictionary_set_stringbuf(sourcekitd_response_t dict,
                                                  sourcekitd_uid_t key,
                                                  const char *buf,
                                                  size_t length) {
  llvm::SmallString<512> SS;
  SS += StringRef(buf, length);
  sourcekitd_response_dictionary_set_string(dict, key, SS.c_str());
}

void sourcekitd_response_dictionary_set_int64(sourcekitd_response_t dict,
                                              sourcekitd_uid_t key,
                                              int64_t val) {
  xpc_dictionary_set_int64(dict, strFromUID(key), val);
}

void sourcekitd_response_dictionary_set_bool(sourcekitd_response_t dict,
                                             sourcekitd_uid_t key, bool val) {
  xpc_dictionary_set_bool(dict, strFromUID(key), val);
}

void sourcekitd_response_dictionary_set_double(sourcekitd_response_t dict,
                                               sourcekitd_uid_t key,
                                               double val) {
  xpc_dictionary_set_double(dict, strFromUID(key), val);
}

void sourcekitd_response_dictionary_set_uid(sourcekitd_response_t dict,
                                            sourcekitd_uid_t key,
                                            sourcekitd_uid_t uid) {
  xpc_dictionary_set_uint64(dict, strFromUID(key), uintptr_t(uid));
}

sourcekitd_response_t
sourcekitd_response_array_create(const sourcekitd_response_t *objects,
                                 size_t count) {
  return xpc_array_create(objects, count);
}

void sourcekitd_response_array_set_value(sourcekitd_response_t array,
                                         size_t index,
                                         sourcekitd_response_t value) {
  xpc_array_set_value(array, index, value);
}

void sourcekitd_response_array_set_string(sourcekitd_response_t array,
                                          size_t index, const char *string) {
  xpc_array_set_string(array, index, string);
}

void sourcekitd_response_array_set_stringbuf(sourcekitd_response_t array,
                                             size_t index, const char *buf,
                                             size_t length) {
  llvm::SmallString<512> SS;
  SS += StringRef(buf, length);
  sourcekitd_response_array_set_string(array, index, SS.c_str());
}

void sourcekitd_response_array_set_int64(sourcekitd_response_t array,
                                         size_t index, int64_t val) {
  xpc_array_set_int64(array, index, val);
}

void sourcekitd_response_array_set_double(sourcekitd_response_t array,
                                          size_t index, double val) {
  xpc_array_set_double(array, index, val);
}

void sourcekitd_response_array_set_uid(sourcekitd_response_t array,
                                       size_t index, sourcekitd_uid_t uid) {
  xpc_array_set_uint64(array, index, uintptr_t(uid));
}

sourcekitd_response_t sourcekitd_response_retain(sourcekitd_response_t object) {
  return xpc_retain(object);
}

sourcekitd_variant_type_t sourcekitd_request_get_type(sourcekitd_object_t obj) {
  xpc_type_t XType = xpc_get_type(obj);
  if (XType == XPC_TYPE_DICTIONARY)
    return SOURCEKITD_VARIANT_TYPE_DICTIONARY;
  if (XType == XPC_TYPE_ARRAY)
    return SOURCEKITD_VARIANT_TYPE_ARRAY;
  if (XType == XPC_TYPE_INT64)
    return SOURCEKITD_VARIANT_TYPE_INT64;
  if (XType == XPC_TYPE_BOOL)
    return SOURCEKITD_VARIANT_TYPE_BOOL;
  if (XType == XPC_TYPE_UINT64)
    return SOURCEKITD_VARIANT_TYPE_UID;
  if (XType == XPC_TYPE_STRING)
    return SOURCEKITD_VARIANT_TYPE_STRING;
  if (XType == XPC_TYPE_NULL)
    return SOURCEKITD_VARIANT_TYPE_NULL;
  llvm::report_fatal_error("unkown sourcekitd_object_t type");
}

void sourcekitd_response_dictionary_set_custom_buffer(
    sourcekitd_response_t dict, sourcekitd_uid_t key, const void *ptr,
    size_t size) {
#ifndef NDEBUG
  assert(size >= sizeof(uint64_t));
  auto bufKind = *(const uint64_t *)ptr;
  assert(bufKind >= (uint64_t)CustomBufferKind::CustomBufferKind_End);
#endif
  xpc_object_t xdata = xpc_data_create(ptr, size);
  xpc_dictionary_set_value(dict, strFromUID(key), xdata);
  xpc_release(xdata);
}
