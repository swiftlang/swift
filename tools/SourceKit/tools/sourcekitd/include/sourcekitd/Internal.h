//===--- Internal.h - -------------------------------------------*- C++ -*-===//
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

#ifndef LLVM_SOURCEKITD_INTERNAL_H
#define LLVM_SOURCEKITD_INTERNAL_H

#if defined (_MSC_VER)
# define SOURCEKITD_PUBLIC __declspec(dllexport)
#endif

#include "sourcekitd/sourcekitd.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include <functional>
#include <string>

namespace llvm {
  class MemoryBuffer;
  class StringRef;
  template <typename T> class SmallVectorImpl;
  template <typename T> class ArrayRef;
  class raw_ostream;
}
namespace SourceKit {
  class UIdent;
}

bool sourcekitd_variant_dictionary_apply_impl(
    sourcekitd_variant_t dict,
    llvm::function_ref<bool(sourcekitd_uid_t, sourcekitd_variant_t)> applier);

bool sourcekitd_variant_array_apply_impl(
    sourcekitd_variant_t array,
    llvm::function_ref<bool(size_t, sourcekitd_variant_t)> applier);

namespace sourcekitd {

using llvm::Optional;
using llvm::None;

// The IPC protocol version. This can be queried via a request.
static const unsigned ProtocolMajorVersion = 1;
static const unsigned ProtocolMinorVersion = 0;

enum class CustomBufferKind {
  TokenAnnotationsArray,
  DocSupportAnnotationArray,
  CodeCompletionResultsArray,
  DocStructureArray,
  InheritedTypesArray,
  DocStructureElementArray,
  AttributesArray,
  RawData
};

class ResponseBuilder {
public:
  class Array;

  class Dictionary {
  public:
    Dictionary() = default;
    Dictionary(llvm::NoneType) : Dictionary() {}
    explicit Dictionary(void *Impl) : Impl(Impl) { }

    bool isNull() const { return Impl == nullptr; }

    void set(SourceKit::UIdent Key, SourceKit::UIdent UID);
    void set(SourceKit::UIdent Key, sourcekitd_uid_t UID);
    void set(SourceKit::UIdent Key, const char *Str);
    void set(SourceKit::UIdent Key, llvm::StringRef Str);
    void set(SourceKit::UIdent Key, const std::string &Str);
    void set(SourceKit::UIdent Key, int64_t val);
    void set(SourceKit::UIdent Key, llvm::ArrayRef<llvm::StringRef> Strs);
    void set(SourceKit::UIdent Key, llvm::ArrayRef<std::string> Strs);
    void setBool(SourceKit::UIdent Key, bool val);
    Array setArray(SourceKit::UIdent Key);
    Dictionary setDictionary(SourceKit::UIdent Key);
    void setCustomBuffer(SourceKit::UIdent Key, CustomBufferKind Kind,
                         std::unique_ptr<llvm::MemoryBuffer> MemBuf);

  private:
    void *Impl = nullptr;
  };

  class Array {
  public:
    Array() = default;
    Array(llvm::NoneType) : Array() {}
    explicit Array(void *Impl) : Impl(Impl) { }

    bool isNull() const { return Impl == nullptr; }

    Dictionary appendDictionary();

  private:
    void *Impl = nullptr;
  };

  ResponseBuilder();
  ~ResponseBuilder();

  ResponseBuilder(const ResponseBuilder &Other);
  ResponseBuilder &operator =(const ResponseBuilder &Other);

  Dictionary getDictionary();
  sourcekitd_response_t createResponse();

private:
  void *Impl;
};

class RequestDict {
  sourcekitd_object_t Dict;

public:
  explicit RequestDict(sourcekitd_object_t Dict) : Dict(Dict) {}

  sourcekitd_uid_t getUID(SourceKit::UIdent Key);
  Optional<llvm::StringRef> getString(SourceKit::UIdent Key);
  Optional<RequestDict> getDictionary(SourceKit::UIdent Key);

  /// \brief Populate the vector with an array of C strings.
  /// \param isOptional true if the key is optional. If false and the key is
  /// missing, the function will return true to indicate an error.
  /// \returns true if there is an error, like the key is not of an array type or
  /// the array does not contain strings.
  bool getStringArray(SourceKit::UIdent Key,
                      llvm::SmallVectorImpl<const char *> &Arr,
                      bool isOptional);
  bool getUIDArray(SourceKit::UIdent Key,
                   llvm::SmallVectorImpl<sourcekitd_uid_t> &Arr,
                   bool isOptional);

  bool dictionaryArrayApply(SourceKit::UIdent key,
                            llvm::function_ref<bool(RequestDict)> applier);

  bool getInt64(SourceKit::UIdent Key, int64_t &Val, bool isOptional);
  Optional<int64_t> getOptionalInt64(SourceKit::UIdent Key);
};

void initialize();
void shutdown();
void set_interrupted_connection_handler(llvm::function_ref<void()> handler);

typedef std::function<void(sourcekitd_response_t)> ResponseReceiver;

void handleRequest(sourcekitd_object_t Request, ResponseReceiver Receiver);

void printRequestObject(sourcekitd_object_t Obj, llvm::raw_ostream &OS);
void printResponse(sourcekitd_response_t Resp, llvm::raw_ostream &OS);

sourcekitd_response_t createErrorRequestInvalid(const char *Description);
sourcekitd_response_t createErrorRequestFailed(const char *Description);
sourcekitd_response_t createErrorRequestInterrupted(const char *Description);
sourcekitd_response_t createErrorRequestCancelled();

/// Send notification object.
/// The ownership of the object is transferred to the function.
void postNotification(sourcekitd_response_t Notification);

// The client & service have their own implementations for these.
sourcekitd_uid_t SKDUIDFromUIdent(SourceKit::UIdent UID);
SourceKit::UIdent UIdentFromSKDUID(sourcekitd_uid_t uid);

std::string getRuntimeLibPath();

void writeEscaped(llvm::StringRef Str, llvm::raw_ostream &OS);

static inline sourcekitd_variant_t makeNullVariant() {
  return {{ 0, 0, 0 }};
}
static inline sourcekitd_variant_t makeIntVariant(int64_t value) {
  return {{ 0, (uint64_t)value, SOURCEKITD_VARIANT_TYPE_INT64 }};
}
static inline sourcekitd_variant_t makeBoolVariant(bool value) {
  return {{ 0, value, SOURCEKITD_VARIANT_TYPE_BOOL }};
}
static inline sourcekitd_variant_t makeStringVariant(const char *value) {
  return {{ 0, (uintptr_t)value, SOURCEKITD_VARIANT_TYPE_STRING }};
}
static inline sourcekitd_variant_t makeUIDVariant(sourcekitd_uid_t value) {
  return {{ 0, (uintptr_t)value, SOURCEKITD_VARIANT_TYPE_UID }};
}

/// The function pointers that implement the interface to a sourcekitd_variant_t
/// object.
///
/// sourcekitd_variant_t contains a pointer to such a structure.
struct VariantFunctions {
  sourcekitd_variant_type_t (*get_type)(sourcekitd_variant_t obj);
  bool (*array_apply)(
      sourcekitd_variant_t array,
      llvm::function_ref<bool(size_t, sourcekitd_variant_t)> applier);
  bool (*array_get_bool)(sourcekitd_variant_t array, size_t index);
  size_t (*array_get_count)(sourcekitd_variant_t array);
  int64_t (*array_get_int64)(sourcekitd_variant_t array, size_t index);
  const char *(*array_get_string)(sourcekitd_variant_t array, size_t index);
  sourcekitd_uid_t (*array_get_uid)(sourcekitd_variant_t array, size_t index);
  sourcekitd_variant_t (*array_get_value)(sourcekitd_variant_t array, size_t index);
  bool (*bool_get_value)(sourcekitd_variant_t obj);
  bool (*dictionary_apply)(
      sourcekitd_variant_t dict,
      llvm::function_ref<bool(sourcekitd_uid_t, sourcekitd_variant_t)> applier);
  bool (*dictionary_get_bool)(sourcekitd_variant_t dict, sourcekitd_uid_t key);
  int64_t (*dictionary_get_int64)(sourcekitd_variant_t dict, sourcekitd_uid_t key);
  const char *(*dictionary_get_string)(sourcekitd_variant_t dict, sourcekitd_uid_t key);
  sourcekitd_variant_t (*dictionary_get_value)(sourcekitd_variant_t dict, sourcekitd_uid_t key);
  sourcekitd_uid_t (*dictionary_get_uid)(sourcekitd_variant_t dict, sourcekitd_uid_t key);
  size_t (*string_get_length)(sourcekitd_variant_t obj);
  const char *(*string_get_ptr)(sourcekitd_variant_t obj);
  int64_t (*int64_get_value)(sourcekitd_variant_t obj);
  sourcekitd_uid_t (*uid_get_value)(sourcekitd_variant_t obj);
  size_t (*data_get_size)(sourcekitd_variant_t obj);
  const void *(*data_get_ptr)(sourcekitd_variant_t obj);
};

}

#endif
