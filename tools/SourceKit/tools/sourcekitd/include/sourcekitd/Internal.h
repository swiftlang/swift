//===--- Internal.h - -------------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_SOURCEKITD_INTERNAL_H
#define LLVM_SOURCEKITD_INTERNAL_H

#include "SourceKit/Support/CancellationToken.h"
#include "sourcekitd/plugin.h"
#include "sourcekitd/sourcekitd.h"
#include "llvm/ADT/STLExtras.h"
#include <functional>
#include <optional>
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
    sourcekitd_variant_dictionary_applier_f_t applier, void *context);

bool sourcekitd_variant_array_apply_impl(
    sourcekitd_variant_t array, sourcekitd_variant_array_applier_f_t applier,
    void *context);

namespace sourcekitd {

using SourceKit::SourceKitCancellationToken;

// The IPC protocol version. This can be queried via a request.
static const unsigned ProtocolMajorVersion = 1;
static const unsigned ProtocolMinorVersion = 0;

enum class CustomBufferKind : size_t {
  TokenAnnotationsArray,
  DeclarationsArray,
  DocSupportAnnotationArray,
  CodeCompletionResultsArray,
  DocStructureArray,
  InheritedTypesArray,
  DocStructureElementArray,
  AttributesArray,
  ExpressionTypeArray,
  VariableTypeArray,
  RawData,
  CustomBufferKind_End
};

class ResponseBuilder {
public:
  class Array;

  class Dictionary {
  public:
    Dictionary() = default;
    Dictionary(std::nullopt_t) : Dictionary() {}
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
    void set(SourceKit::UIdent Key, llvm::ArrayRef<SourceKit::UIdent> UIDs);
    void setBool(SourceKit::UIdent Key, bool val);
    Array setArray(SourceKit::UIdent Key);
    Dictionary setDictionary(SourceKit::UIdent Key);
    void setCustomBuffer(SourceKit::UIdent Key,
                         std::unique_ptr<llvm::MemoryBuffer> MemBuf);

  private:
    void *Impl = nullptr;
  };

  class Array {
  public:
    Array() = default;
    Array(std::nullopt_t) : Array() {}
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
  explicit RequestDict(sourcekitd_object_t Dict) : Dict(Dict) {
    sourcekitd_request_retain(Dict);
  }
  RequestDict(const RequestDict &other) : RequestDict(other.Dict) {}
  ~RequestDict() {
    sourcekitd_request_release(Dict);
  }

  sourcekitd_uid_t getUID(SourceKit::UIdent Key) const;
  std::optional<llvm::StringRef> getString(SourceKit::UIdent Key) const;
  std::optional<RequestDict> getDictionary(SourceKit::UIdent Key) const;

  /// Populate the vector with an array of C strings.
  /// \param isOptional true if the key is optional. If false and the key is
  /// missing, the function will return true to indicate an error.
  /// \returns true if there is an error, like the key is not of an array type or
  /// the array does not contain strings.
  bool getStringArray(SourceKit::UIdent Key,
                      llvm::SmallVectorImpl<const char *> &Arr,
                      bool isOptional) const;
  bool getUIDArray(SourceKit::UIdent Key,
                   llvm::SmallVectorImpl<sourcekitd_uid_t> &Arr,
                   bool isOptional) const;

  bool
  dictionaryArrayApply(SourceKit::UIdent key,
                       llvm::function_ref<bool(RequestDict)> applier) const;

  bool getInt64(SourceKit::UIdent Key, int64_t &Val, bool isOptional) const;
  std::optional<int64_t> getOptionalInt64(SourceKit::UIdent Key) const;
};

/// Initialize the sourcekitd client library. Returns true if this is the first
/// time it is initialized.
bool initializeClient();
/// Shutdown the sourcekitd client. Returns true if this is the last active
/// client and the service should be shutdown.
bool shutdownClient();

void set_interrupted_connection_handler(llvm::function_ref<void()> handler);

/// Register a custom buffer kind. Must be called only during plugin loading.
void pluginRegisterCustomBufferKind(uint64_t kind,
                                    sourcekitd_variant_functions_t funcs);

void printRequestObject(sourcekitd_object_t Obj, llvm::raw_ostream &OS);
void printResponse(sourcekitd_response_t Resp, llvm::raw_ostream &OS);

sourcekitd_response_t createErrorRequestInvalid(llvm::StringRef Description);
sourcekitd_response_t createErrorRequestFailed(llvm::StringRef Description);
sourcekitd_response_t createErrorRequestInterrupted(llvm::StringRef Descr);
sourcekitd_response_t createErrorRequestCancelled();

// The client & service have their own implementations for these.
sourcekitd_uid_t SKDUIDFromUIdent(SourceKit::UIdent UID);
SourceKit::UIdent UIdentFromSKDUID(sourcekitd_uid_t uid);

static inline sourcekitd_variant_t makeNullVariant() {
  return {{ 0, 0, 0 }};
}
static inline sourcekitd_variant_t makeIntVariant(int64_t value) {
  return {{ 0, (uint64_t)value, SOURCEKITD_VARIANT_TYPE_INT64 }};
}
static inline sourcekitd_variant_t makeBoolVariant(bool value) {
  return {{ 0, value, SOURCEKITD_VARIANT_TYPE_BOOL }};
}
static inline sourcekitd_variant_t makeDoubleVariant(double value) {
  uint64_t data;
  std::memcpy(&data, &value, sizeof(double));
  return {{0, data, SOURCEKITD_VARIANT_TYPE_DOUBLE}};
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
  sourcekitd_variant_functions_get_type_t get_type;
  sourcekitd_variant_functions_array_apply_t array_apply;
  sourcekitd_variant_functions_array_get_bool_t array_get_bool;
  sourcekitd_variant_functions_array_get_double_t array_get_double;
  sourcekitd_variant_functions_array_get_count_t array_get_count;
  sourcekitd_variant_functions_array_get_int64_t array_get_int64;
  sourcekitd_variant_functions_array_get_string_t array_get_string;
  sourcekitd_variant_functions_array_get_uid_t array_get_uid;
  sourcekitd_variant_functions_array_get_value_t array_get_value;
  sourcekitd_variant_functions_bool_get_value_t bool_get_value;
  sourcekitd_variant_functions_double_get_value_t double_get_value;
  sourcekitd_variant_functions_dictionary_apply_t dictionary_apply;
  sourcekitd_variant_functions_dictionary_get_bool_t dictionary_get_bool;
  sourcekitd_variant_functions_dictionary_get_double_t dictionary_get_double;
  sourcekitd_variant_functions_dictionary_get_int64_t dictionary_get_int64;
  sourcekitd_variant_functions_dictionary_get_string_t dictionary_get_string;
  sourcekitd_variant_functions_dictionary_get_value_t dictionary_get_value;
  sourcekitd_variant_functions_dictionary_get_uid_t dictionary_get_uid;
  sourcekitd_variant_functions_string_get_length_t string_get_length;
  sourcekitd_variant_functions_string_get_ptr_t string_get_ptr;
  sourcekitd_variant_functions_int64_get_value_t int64_get_value;
  sourcekitd_variant_functions_uid_get_value_t uid_get_value;
  sourcekitd_variant_functions_data_get_size_t data_get_size;
  sourcekitd_variant_functions_data_get_ptr_t data_get_ptr;
};

// Parameters for plugin initialization.
struct PluginInitParams {
  bool isClientOnly;
  uint64_t customBufferStart = (uint64_t)CustomBufferKind::CustomBufferKind_End;
  sourcekitd_uid_get_from_cstr_t uidGetFromCstr = sourcekitd_uid_get_from_cstr;
  sourcekitd_uid_get_string_ptr_t uidGetStringPtr =
      sourcekitd_uid_get_string_ptr;
  std::function<void(sourcekitd_cancellable_request_handler_t)>
      registerRequestHandler;
  std::function<void(sourcekitd_cancellation_handler_t)>
      registerCancellationHandler;
  std::function<void(uint64_t, sourcekitd_variant_functions_t)>
      registerCustomBuffer;
  void *opaqueIDEInspectionInstance;

  PluginInitParams(bool isClientOnly,
                   std::function<void(sourcekitd_cancellable_request_handler_t)>
                       registerRequestHandler,
                   std::function<void(sourcekitd_cancellation_handler_t)>
                       registerCancellationHandler,
                   void *opaqueIDEInspectionInstance = nullptr);
};

void loadPlugins(llvm::ArrayRef<std::string> registeredPlugins,
                 PluginInitParams &pluginParams);

VariantFunctions *getPluginVariantFunctions(size_t BufKind);
}

#endif
