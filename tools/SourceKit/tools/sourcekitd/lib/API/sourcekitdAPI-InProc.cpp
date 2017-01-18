//===--- sourcekitdAPI-InProc.cpp -----------------------------------------===//
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

#include "DictionaryKeys.h"
#include "sourcekitd/sourcekitd.h"
#include "sourcekitd/Internal.h"
#include "sourcekitd/CodeCompletionResultsArray.h"
#include "sourcekitd/DocSupportAnnotationArray.h"
#include "sourcekitd/TokenAnnotationsArray.h"
#include "sourcekitd/Logging.h"
#include "SourceKit/Core/LLVM.h"
#include "SourceKit/Support/UIdent.h"
#include "swift/Basic/ThreadSafeRefCounted.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"

#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MemoryBuffer.h"

#include <map>

using namespace SourceKit;
using namespace sourcekitd;

namespace {
class SKDObject;
typedef RefPtr<SKDObject> SKDObjectRef;

class SKDObject : public ThreadSafeRefCountedBaseVPTR {
public:
  enum class ObjectKind {
    Dictionary,
    Array,
    String,
    Int64,
    UID,
    Bool,
    CustomData,
    Error,
  };
private:
  ObjectKind Kind;
protected:
  SKDObject(ObjectKind K) : Kind(K) {}
public:
  SKDObject(SKDObject const&) = delete;
  SKDObject &operator=(SKDObject const&) = delete;

  ObjectKind getKind() const { return Kind; }

  virtual sourcekitd_variant_type_t getVariantType() const = 0;

  virtual void set(sourcekitd_uid_t Key, SKDObjectRef Value) {}
  virtual void set(size_t Index, SKDObjectRef Value) {}

  virtual SKDObjectRef get(sourcekitd_uid_t Key) const { return nullptr; }

  virtual SKDObjectRef get(size_t Index) const { return nullptr; }
  virtual size_t getCount() const { return 0; }

  virtual sourcekitd_uid_t getUID() const { return nullptr; }
  virtual Optional<int64_t> getInt64() const { return None; }
  virtual Optional<StringRef> getString() const { return None; }
  virtual const char *getCString() const { return nullptr; }
  virtual bool getBool() const { return false; }
};

class SKDDictionary: public SKDObject {
public:
  SKDDictionary() : SKDObject(ObjectKind::Dictionary) {}
  SKDDictionary(SKDDictionary const&) = delete;
  SKDDictionary &operator=(SKDDictionary const&) = delete;

  sourcekitd_variant_type_t getVariantType() const override {
    return SOURCEKITD_VARIANT_TYPE_DICTIONARY;
  }

  void set(sourcekitd_uid_t Key, SKDObjectRef Value) override {
    Storage[Key] = Value;
  }

  SKDObjectRef get(sourcekitd_uid_t Key) const override {
    auto it = Storage.find(Key);
    return it != Storage.end() ? it->second : nullptr;
  }

  bool 
  apply(llvm::function_ref<bool(sourcekitd_uid_t, SKDObjectRef)> Applier) const {
    for (const auto& kv : Storage) {
      if (!Applier(kv.first, kv.second))
        return false;
    }
    return true;
  }

  static bool classof(const SKDObject *O) {
    return O->getKind() == ObjectKind::Dictionary;
  }
private:
  std::map<sourcekitd_uid_t, SKDObjectRef> Storage;
};

class SKDArray: public SKDObject {
public:
  SKDArray() : SKDObject(ObjectKind::Array) {}
  SKDArray(SKDArray const&) = delete;
  SKDArray &operator=(SKDArray const&) = delete;

  sourcekitd_variant_type_t getVariantType() const override {
    return SOURCEKITD_VARIANT_TYPE_ARRAY;
  }

  void set(size_t Index, SKDObjectRef Value) override {
    if (Index == SOURCEKITD_ARRAY_APPEND) {
      Storage.push_back(Value);
    } else {
      Storage[Index] = Value;
    }
  }

  SKDObjectRef get(size_t Index) const override {
    return Storage[Index];
  }

  size_t getCount() const override {
    return Storage.size();
  }

  bool apply(llvm::function_ref<bool(size_t, SKDObjectRef)> Applier) const {
    for (size_t i = 0; i < Storage.size(); ++i) {
      if (!Applier(i, Storage[i]))
        return false;
    }
    return true;
  }

  static bool classof(const SKDObject *O) {
    return O->getKind() == ObjectKind::Array;
  }
private:
  std::vector<SKDObjectRef> Storage;
};

class SKDString: public SKDObject {
public:
  SKDString(std::string &&Value) : SKDObject(ObjectKind::String), Storage(Value) {}

  sourcekitd_variant_type_t getVariantType() const override {
    return SOURCEKITD_VARIANT_TYPE_STRING;
  }

  Optional<StringRef> getString() const override {
    return Optional<StringRef>(Storage);
  }

  const char *getCString() const override {
    return Storage.c_str();
  }

  static bool classof(const SKDObject *O) {
    return O->getKind() == ObjectKind::String;
  }
private:
  std::string Storage;
};

class SKDInt64: public SKDObject {
public:
  SKDInt64(int64_t Value) : SKDObject(ObjectKind::Int64), Storage(Value) {}

  sourcekitd_variant_type_t getVariantType() const override {
    return SOURCEKITD_VARIANT_TYPE_INT64;
  }

  Optional<int64_t> getInt64() const override {
    return Storage;
  }

  static bool classof(const SKDObject *O) {
    return O->getKind() == ObjectKind::Int64;
  }
private:
  int64_t Storage;
};

class SKDUID: public SKDObject {
public:
  SKDUID(sourcekitd_uid_t Value) : SKDObject(ObjectKind::UID), Storage(Value) {}

  sourcekitd_variant_type_t getVariantType() const override {
    return SOURCEKITD_VARIANT_TYPE_UID;
  }

  sourcekitd_uid_t getUID() const override {
    return Storage;
  }

  static bool classof(const SKDObject *O) {
    return O->getKind() == ObjectKind::UID;
  }
private:
  sourcekitd_uid_t Storage;
};

class SKDBool: public SKDObject {
public:
  SKDBool(bool Value) : SKDObject(ObjectKind::Bool), Storage(Value) {}

  sourcekitd_variant_type_t getVariantType() const override {
    return SOURCEKITD_VARIANT_TYPE_BOOL;
  }

  bool getBool() const override {
    return Storage;
  }

  static bool classof(const SKDObject *O) {
    return O->getKind() == ObjectKind::Bool;
  }
private:
  bool Storage;
};

class SKDCustomData: public SKDObject {
public:
  SKDCustomData(CustomBufferKind BufferKind, 
                std::unique_ptr<llvm::MemoryBuffer>& MemBuf)
  : SKDObject(ObjectKind::CustomData), BufferKind(BufferKind),
    BufferPtr(llvm::MemoryBuffer::getMemBufferCopy(
                                                MemBuf->getBuffer(), 
                                                MemBuf->getBufferIdentifier())) 
    {}

  SKDCustomData(SKDCustomData const&) = delete;
  SKDCustomData &operator=(SKDCustomData const&) = delete;

  sourcekitd_variant_type_t getVariantType() const override {
    switch (BufferKind) {
      case CustomBufferKind::TokenAnnotationsArray:
      case CustomBufferKind::DocSupportAnnotationArray:
      case CustomBufferKind::CodeCompletionResultsArray:
        return SOURCEKITD_VARIANT_TYPE_ARRAY;
    }
    llvm::report_fatal_error("sourcekitd object did not resolve to a known type");
  }

  CustomBufferKind getBufferKind() const {
    return BufferKind;
  }

  const void *getDataPtr() const {
    return BufferPtr->getBuffer().data();
  }

  static bool classof(const SKDObject *O) {
    return O->getKind() == ObjectKind::CustomData;
  }
private:
  CustomBufferKind BufferKind;
  std::unique_ptr<llvm::MemoryBuffer> BufferPtr;
};

class SKDError: public SKDObject {
public:
  SKDError(sourcekitd_error_t ErrorKind, StringRef Description)
  : SKDObject(ObjectKind::Error), ErrorKind(ErrorKind), Description(Description) {}

  SKDError(SKDError const&) = delete;
  SKDError &operator=(SKDError const&) = delete;

  sourcekitd_variant_type_t getVariantType() const override {
    llvm::report_fatal_error("sourcekitd error object is not a variant type");
  }

  sourcekitd_error_t getErrorKind() const {
    return ErrorKind;
  }

  const std::string &getDescription() const {
    return Description;
  }

  static bool classof(const SKDObject *O) {
    return O->getKind() == ObjectKind::Error;
  }
private:
  sourcekitd_error_t ErrorKind;
  std::string Description;
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
    auto Object = static_cast<SKDObject *>(Obj);

    if (auto DictionaryObject = dyn_cast<SKDDictionary>(Object)) {
      DictMap Dict;
      DictionaryObject->apply([&](sourcekitd_uid_t Key, SKDObjectRef Element){
        UIdent UID = UIdentFromSKDUID(Key);
        Dict.push_back({ UID, Element.get() });
        return true;
      });
      std::sort(Dict.begin(), Dict.end(), compKeys);
      return static_cast<ImplClass*>(this)->visitDictionary(Dict);
    }
    if (auto ArrayObject = dyn_cast<SKDArray>(Object)) {
      std::vector<sourcekitd_object_t> Vec;
      ArrayObject->apply([&](size_t Index, SKDObjectRef Element){
        Vec.push_back(Element.get());
        return true;
      });
      return static_cast<ImplClass*>(this)->visitArray(Vec);
    }
    if (auto SKDUID = Object->getUID()) {
      UIdent UID = UIdentFromSKDUID(SKDUID);
      return static_cast<ImplClass*>(this)->visitUID(UID.getName());
    }
    if (auto CString = Object->getCString()) {
      return static_cast<ImplClass*>(this)->visitString(CString);
    }
    auto OptInt = Object->getInt64();
    if (OptInt.hasValue()) {
      return static_cast<ImplClass*>(this)->visitInt64(OptInt.getValue());
    }
    llvm_unreachable("unknown sourcekitd_object_t");
  }
};

class SKDObjectPrinter : public SKDObjectVisitor<SKDObjectPrinter> {
  raw_ostream &OS;
  unsigned Indent;
public:
  SKDObjectPrinter(raw_ostream &OS, unsigned Indent = 0)
  : OS(OS), Indent(Indent) { }

  void visitDictionary(const DictMap &Map) {
    OS << "{\n";
    Indent += 2;
    for (unsigned i = 0, e = Map.size(); i != e; ++i) {
      auto &Pair = Map[i];
      OS.indent(Indent);
      OSColor(OS, DictKeyColor) << Pair.first.getName();
      OS << ": ";
      SKDObjectPrinter(OS, Indent).visit(Pair.second);
      if (i < e-1)
        OS << ',';
      OS << '\n';
    }
    Indent -= 2;
    OS.indent(Indent) << '}';
  }

  void visitArray(ArrayRef<sourcekitd_object_t> Arr) {
    OS << "[\n";
    Indent += 2;
    for (unsigned i = 0, e = Arr.size(); i != e; ++i) {
      auto Obj = Arr[i];
      OS.indent(Indent);
      SKDObjectPrinter(OS, Indent).visit(Obj);
      if (i < e-1)
        OS << ',';
      OS << '\n';
    }
    Indent -= 2;
    OS.indent(Indent) << ']';
  }

  void visitInt64(int64_t Val) {
    OS << Val;
  }

  void visitString(StringRef Str) {
    OS << '\"';
    // Avoid raw_ostream's write_escaped, we don't want to escape unicode
    // characters because it will be invalid JSON.
    writeEscaped(Str, OS);
    OS << '\"';
  }
  
  void visitUID(StringRef UID) {
    OSColor(OS, UIDColor) << UID;
  }
};

static SKDObject *retained(SKDObject *object) {
  object->Retain();
  return object;
}

} // anonymous namespace

//===----------------------------------------------------------------------===//
// Public Request API
//===----------------------------------------------------------------------===//

sourcekitd_object_t
sourcekitd_request_retain(sourcekitd_object_t object) {
  return retained(static_cast<SKDObject *>(object));
}

void sourcekitd_request_release(sourcekitd_object_t object) {
  static_cast<SKDObject *>(object)->Release();
}

sourcekitd_object_t
sourcekitd_request_dictionary_create(const sourcekitd_uid_t *keys,
                                     const sourcekitd_object_t *values,
                                     size_t count) {
  SKDDictionary *Dict = new SKDDictionary();
  for (size_t i = 0; i < count; ++i) {
    Dict->set(keys[i], static_cast<SKDObject *>(values[i]));
  }
  return retained(Dict);
}

void
sourcekitd_request_dictionary_set_value(sourcekitd_object_t dict,
                                        sourcekitd_uid_t key,
                                        sourcekitd_object_t value) {
  static_cast<SKDObject *>(dict)->set(key, static_cast<SKDObject *>(value));
}

void sourcekitd_request_dictionary_set_string(sourcekitd_object_t dict,
                                              sourcekitd_uid_t key,
                                              const char *string) {
  static_cast<SKDObject *>(dict)->set(key, new SKDString(std::string(string)));
}

void
sourcekitd_request_dictionary_set_stringbuf(sourcekitd_object_t dict,
                                            sourcekitd_uid_t key,
                                            const char *buf, size_t length) {
  static_cast<SKDObject *>(dict)->set(key, 
                                      new SKDString(std::string(buf, length)));
}

void sourcekitd_request_dictionary_set_int64(sourcekitd_object_t dict,
                                             sourcekitd_uid_t key,
                                             int64_t val) {
  static_cast<SKDObject *>(dict)->set(key, new SKDInt64(val));
}

void
sourcekitd_request_dictionary_set_uid(sourcekitd_object_t dict,
                                      sourcekitd_uid_t key,
                                      sourcekitd_uid_t uid) {
  static_cast<SKDObject *>(dict)->set(key, new SKDUID(uid));
}

sourcekitd_object_t
sourcekitd_request_array_create(const sourcekitd_object_t *objects,
                                size_t count) {
  SKDArray *Array = new SKDArray();
  for (size_t i = 0; i < count; ++i) {
    Array->set(SOURCEKITD_ARRAY_APPEND, static_cast<SKDObject *>(objects[i]));
  }
  return retained(Array);
}

void
sourcekitd_request_array_set_value(sourcekitd_object_t array, size_t index,
                                   sourcekitd_object_t value) {
  static_cast<SKDObject *>(array)->set(index, static_cast<SKDObject *>(value));
}

void
sourcekitd_request_array_set_string(sourcekitd_object_t array, size_t index,
                                    const char *string) {
  static_cast<SKDObject *>(array)->set(index, 
                                       new SKDString(std::string(string)));
}

void
sourcekitd_request_array_set_stringbuf(sourcekitd_object_t array, size_t index,
                                       const char *buf, size_t length) {
  static_cast<SKDObject *>(array)->set(index,
                                       new SKDString(std::string(buf, length)));
}

void
sourcekitd_request_array_set_int64(sourcekitd_object_t array, size_t index,
                                   int64_t val) {
  static_cast<SKDObject *>(array)->set(index, new SKDInt64(val));
}

void
sourcekitd_request_array_set_uid(sourcekitd_object_t array, size_t index,
                                 sourcekitd_uid_t uid) {
  static_cast<SKDObject *>(array)->set(index, new SKDUID(uid));
}


sourcekitd_object_t
sourcekitd_request_int64_create(int64_t val) {
  return retained(new SKDInt64(val));
}

sourcekitd_object_t
sourcekitd_request_string_create(const char *string) {
  return retained(new SKDString(std::string(string)));
}

sourcekitd_object_t
sourcekitd_request_uid_create(sourcekitd_uid_t uid) {
  return retained(new SKDUID(uid));
}

//===----------------------------------------------------------------------===//
// Public Response API
//===----------------------------------------------------------------------===//

void
sourcekitd_response_dispose(sourcekitd_response_t obj) {
  static_cast<SKDObject *>(obj)->Release();
}

bool
sourcekitd_response_is_error(sourcekitd_response_t obj) {
  return isa<SKDError>(static_cast<SKDObject *>(obj));
}

sourcekitd_error_t
sourcekitd_response_error_get_kind(sourcekitd_response_t obj) {
  if (auto *Error = dyn_cast<SKDError>(static_cast<SKDObject *>(obj)))
    return Error->getErrorKind();
  llvm::report_fatal_error("invalid sourcekitd error object");
}

const char *
sourcekitd_response_error_get_description(sourcekitd_response_t obj) {
  if (auto *Error = dyn_cast<SKDError>(static_cast<SKDObject *>(obj)))
    return Error->getDescription().c_str();
  llvm::report_fatal_error("invalid sourcekitd error object");
}

static sourcekitd_variant_t variantFromSKDObject(SKDObjectRef Object);

sourcekitd_variant_t
sourcekitd_response_get_value(sourcekitd_response_t resp) {
  if (sourcekitd_response_is_error(resp))
    return makeNullVariant();
  return variantFromSKDObject(static_cast<SKDObject *>(resp));
}

//===----------------------------------------------------------------------===//
// Internal Request API
//===----------------------------------------------------------------------===//

void sourcekitd::printRequestObject(sourcekitd_object_t Obj, raw_ostream &OS) {
  if (!Obj) {
    OS << "<<NULL>>";
    return;
  }

  SKDObjectPrinter(OS).visit(Obj);
}

//===----------------------------------------------------------------------===//
// Internal ResponseBuilder Implementation
//===----------------------------------------------------------------------===//

ResponseBuilder::ResponseBuilder() {
  Impl = retained(new SKDDictionary());
}

ResponseBuilder::~ResponseBuilder() {
  static_cast<SKDObject *>(Impl)->Release();
}

ResponseBuilder::ResponseBuilder(const ResponseBuilder &Other) {
  Impl = retained(static_cast<SKDObject *>(Other.Impl));
}

ResponseBuilder &ResponseBuilder::operator =(const ResponseBuilder &Other) {
  auto Tmp = retained(static_cast<SKDObject *>(Other.Impl));
  static_cast<SKDObject *>(Impl)->Release();
  Impl = Tmp;
  return *this;
}

ResponseBuilder::Dictionary ResponseBuilder::getDictionary() {
  return Dictionary(Impl);
}

sourcekitd_response_t ResponseBuilder::createResponse() {
  return retained(static_cast<SKDObject *>(Impl));
}

void ResponseBuilder::Dictionary::set(UIdent Key, SourceKit::UIdent UID) {
  set(Key, SKDUIDFromUIdent(UID));
}

void ResponseBuilder::Dictionary::set(UIdent Key, sourcekitd_uid_t UID) {
  static_cast<SKDObject *>(Impl)->set(SKDUIDFromUIdent(Key), new SKDUID(UID));
}

void ResponseBuilder::Dictionary::set(UIdent Key, const char *Str) {
  set(Key, StringRef(Str));
}

void ResponseBuilder::Dictionary::set(UIdent Key, StringRef Str) {
  static_cast<SKDObject *>(Impl)->set(SKDUIDFromUIdent(Key), 
                                      new SKDString(Str));
}

void ResponseBuilder::Dictionary::set(UIdent Key, int64_t Val) {
  static_cast<SKDObject *>(Impl)->set(SKDUIDFromUIdent(Key), new SKDInt64(Val));
}

void ResponseBuilder::Dictionary::set(SourceKit::UIdent Key,
                                      ArrayRef<StringRef> Strs) {
  auto ArrayObject = new SKDArray();
  for (auto Str : Strs) {
    ArrayObject->set(SOURCEKITD_ARRAY_APPEND, new SKDString(Str));
  }
  static_cast<SKDObject *>(Impl)->set(SKDUIDFromUIdent(Key), ArrayObject);
}

void ResponseBuilder::Dictionary::setBool(UIdent Key, bool Val) {
  static_cast<SKDObject *>(Impl)->set(SKDUIDFromUIdent(Key), new SKDBool(Val));
}

ResponseBuilder::Dictionary
ResponseBuilder::Dictionary::setDictionary(UIdent Key) {
  auto DictionaryObject = new SKDDictionary();
  static_cast<SKDObject *>(Impl)->set(SKDUIDFromUIdent(Key), DictionaryObject);
  return Dictionary(DictionaryObject);
}

void ResponseBuilder::Dictionary::setCustomBuffer(
      SourceKit::UIdent Key,
      CustomBufferKind Kind, std::unique_ptr<llvm::MemoryBuffer> MemBuf) {
  static_cast<SKDObject *>(Impl)->set(SKDUIDFromUIdent(Key), 
                                      new SKDCustomData(Kind, MemBuf));
}

ResponseBuilder::Array
ResponseBuilder::Dictionary::setArray(UIdent Key) {
  auto ArrayObject = new SKDArray();
  static_cast<SKDObject *>(Impl)->set(SKDUIDFromUIdent(Key), ArrayObject);
  return Array(ArrayObject);
}

ResponseBuilder::Dictionary ResponseBuilder::Array::appendDictionary() {
  auto DictionaryObject = new SKDDictionary();
  static_cast<SKDObject *>(Impl)->set(SOURCEKITD_ARRAY_APPEND, DictionaryObject);
  return Dictionary(DictionaryObject);
}

//===----------------------------------------------------------------------===//
// Internal RequestDict Implementation
//===----------------------------------------------------------------------===//

sourcekitd_uid_t RequestDict::getUID(UIdent Key) {
  auto Object = static_cast<SKDObject *>(Dict)->get(SKDUIDFromUIdent(Key));
  return Object ? Object->getUID() : nullptr;
}

Optional<StringRef> RequestDict::getString(UIdent Key) {
  if (auto Object = static_cast<SKDObject *>(Dict)->get(SKDUIDFromUIdent(Key))) {
    return Object->getString();
  }
  return None;
}

Optional<RequestDict> RequestDict::getDictionary(SourceKit::UIdent Key) {
  SKDDictionary *DictObject = nullptr;
  if (auto Object = static_cast<SKDObject *>(Dict)->get(SKDUIDFromUIdent(Key))) {
    DictObject = dyn_cast<SKDDictionary>(Object);
  }
  return DictObject ? Optional<RequestDict>(RequestDict(DictObject)) : None;
}

bool RequestDict::getStringArray(SourceKit::UIdent Key,
                                 llvm::SmallVectorImpl<const char *> &Arr,
                                 bool isOptional) {
  auto Object = static_cast<SKDObject *>(Dict)->get(SKDUIDFromUIdent(Key));
  if (!Object)
    return !isOptional;
  auto Array = dyn_cast<SKDArray>(Object);
  if (!Array)
    return true;
  size_t count = Array->getCount();
  Arr.reserve(count);
  for (size_t i = 0; i != count; ++i) {
    auto Str = Array->get(i)->getCString();
    if (!Str)
      return true;
    Arr.push_back(Str);
  }
  return false;
}

bool RequestDict::getUIDArray(SourceKit::UIdent Key,
                              llvm::SmallVectorImpl<sourcekitd_uid_t> &Arr,
                              bool isOptional) {
  auto Object = static_cast<SKDObject *>(Dict)->get(SKDUIDFromUIdent(Key));
  if (!Object)
    return !isOptional;
  auto Array = dyn_cast<SKDArray>(Object);
  if (!Array)
    return true;
  size_t count = Array->getCount();
  Arr.reserve(count);
  for (size_t i = 0; i != count; ++i) {
    auto UID = Array->get(i)->getUID();
    if (!UID)
      return true;
    Arr.push_back(UID);
  }
  return false;
}

bool RequestDict::dictionaryArrayApply(
    SourceKit::UIdent Key, llvm::function_ref<bool(RequestDict)> Applier) {
  auto Object = static_cast<SKDObject *>(Dict)->get(SKDUIDFromUIdent(Key));
  if (!Object)
    return true;
  auto Array = dyn_cast<SKDArray>(Object);
  if (!Array)
    return true;

  return !Array->apply([&](size_t index, SKDObjectRef object){
    auto Dict = dyn_cast<SKDDictionary>(object);
    if (!Dict)
      return false;
    return !Applier(RequestDict(Dict));
  });
}

bool RequestDict::getInt64(SourceKit::UIdent Key, int64_t &Val,
                           bool isOptional) {
  auto Object = static_cast<SKDObject *>(Dict)->get(SKDUIDFromUIdent(Key));
  if (!Object)
    return !isOptional;
  Val = Object->getInt64().getValueOr(0);
  return false;
}

sourcekitd_response_t
sourcekitd::createErrorRequestInvalid(const char *Description) {
  return retained(new SKDError(SOURCEKITD_ERROR_REQUEST_INVALID, 
                               StringRef(Description)));
}

sourcekitd_response_t
sourcekitd::createErrorRequestFailed(const char *Description) {
  return retained(new SKDError(SOURCEKITD_ERROR_REQUEST_FAILED, 
                      StringRef(Description)));
}

sourcekitd_response_t
sourcekitd::createErrorRequestCancelled() {
  return retained(new SKDError(SOURCEKITD_ERROR_REQUEST_CANCELLED, 
                               StringRef("")));
}

//===----------------------------------------------------------------------===//
// Variant functions
//===----------------------------------------------------------------------===//

#define SKD_OBJ(var) ((SKDObject *)(var).data[1])

static sourcekitd_variant_type_t SKDVar_get_type(sourcekitd_variant_t var) {
  if (auto Object = SKD_OBJ(var)) {
    return Object->getVariantType();
  }
  llvm::report_fatal_error("sourcekitd object did not resolve to a known type");
}

static bool SKDVar_array_apply(
                               sourcekitd_variant_t array,
                               sourcekitd_variant_array_applier_t applier) {
  return dyn_cast<SKDArray>(SKD_OBJ(array))->apply([&](size_t Index, 
                                                       SKDObjectRef Object){
    return applier(Index, variantFromSKDObject(Object));
  });
}

static bool SKDVar_array_get_bool(sourcekitd_variant_t array, size_t index) {
  return SKD_OBJ(array)->get(index)->getBool();
}

static size_t SKDVar_array_get_count(sourcekitd_variant_t array) {
  return SKD_OBJ(array)->getCount();
}

static int64_t SKDVar_array_get_int64(sourcekitd_variant_t array, size_t index) {
  return SKD_OBJ(array)->get(index)->getInt64().getValueOr(0);
}

static const char *
SKDVar_array_get_string(sourcekitd_variant_t array, size_t index) {
  return SKD_OBJ(array)->get(index)->getCString();
}

static sourcekitd_uid_t SKDVar_array_get_uid(sourcekitd_variant_t array, 
                                             size_t index) {
  return SKD_OBJ(array)->get(index)->getUID();
}

static sourcekitd_variant_t
SKDVar_array_get_value(sourcekitd_variant_t array, size_t index) {
  return variantFromSKDObject(SKD_OBJ(array)->get(index));
}

static bool SKDVar_bool_get_value(sourcekitd_variant_t obj) {
  return SKD_OBJ(obj)->getBool();
}

static bool SKDVar_dictionary_apply(
                              sourcekitd_variant_t dict,
                              sourcekitd_variant_dictionary_applier_t applier) {
  return dyn_cast<SKDDictionary>(SKD_OBJ(dict))->apply([&](sourcekitd_uid_t Key, 
                                                           SKDObjectRef Object){
    return applier(Key, variantFromSKDObject(Object));
  });
}

static bool
SKDVar_dictionary_get_bool(sourcekitd_variant_t dict, sourcekitd_uid_t key) {
  if (auto Object = SKD_OBJ(dict)->get(key)) {
    return Object->getBool();
  }
  return false;
}

static int64_t
SKDVar_dictionary_get_int64(sourcekitd_variant_t dict, sourcekitd_uid_t key) {
  if (auto Object = SKD_OBJ(dict)->get(key)) {
    return Object->getInt64().getValueOr(0);
  }
  return 0;
}

static const char *
SKDVar_dictionary_get_string(sourcekitd_variant_t dict, sourcekitd_uid_t key) {
  if (auto Object = SKD_OBJ(dict)->get(key)) {
    return Object->getCString();
  }
  return nullptr;
}

static sourcekitd_variant_t
SKDVar_dictionary_get_value(sourcekitd_variant_t dict, sourcekitd_uid_t key) {
  return variantFromSKDObject(SKD_OBJ(dict)->get(key));
}

static sourcekitd_uid_t
SKDVar_dictionary_get_uid(sourcekitd_variant_t dict, sourcekitd_uid_t key) {
  if (auto Object = SKD_OBJ(dict)->get(key)) {
    return Object->getUID();
  }
  return nullptr;
}

static size_t SKDVar_string_get_length(sourcekitd_variant_t obj) {
  auto String = SKD_OBJ(obj)->getString();
  return String.hasValue() ? String->size() : 0;
}

static const char *SKDVar_string_get_ptr(sourcekitd_variant_t obj) {
  return SKD_OBJ(obj)->getCString();
}

static int64_t SKDVar_int64_get_value(sourcekitd_variant_t obj) {
  return SKD_OBJ(obj)->getInt64().getValueOr(0);
}

static sourcekitd_uid_t SKDVar_uid_get_value(sourcekitd_variant_t obj) {
  return SKD_OBJ(obj)->getUID();
}


static VariantFunctions SKDVariantFuncs = {
  SKDVar_get_type,
  SKDVar_array_apply,
  SKDVar_array_get_bool,
  SKDVar_array_get_count,
  SKDVar_array_get_int64,
  SKDVar_array_get_string,
  SKDVar_array_get_uid,
  SKDVar_array_get_value,
  SKDVar_bool_get_value,
  SKDVar_dictionary_apply,
  SKDVar_dictionary_get_bool,
  SKDVar_dictionary_get_int64,
  SKDVar_dictionary_get_string,
  SKDVar_dictionary_get_value,
  SKDVar_dictionary_get_uid,
  SKDVar_string_get_length,
  SKDVar_string_get_ptr,
  SKDVar_int64_get_value,
  SKDVar_uid_get_value
};

static sourcekitd_variant_t variantFromSKDObject(SKDObjectRef Object) {
  if (!Object)
    return makeNullVariant();

  if (auto DataObject = dyn_cast<SKDCustomData>(Object)) {
    switch(DataObject->getBufferKind()) {
      case CustomBufferKind::TokenAnnotationsArray:
        return {{ (uintptr_t)getVariantFunctionsForTokenAnnotationsArray(),
          (uintptr_t)DataObject->getDataPtr(), 0 }};
      case CustomBufferKind::DocSupportAnnotationArray:
        return {{ (uintptr_t)getVariantFunctionsForDocSupportAnnotationArray(),
          (uintptr_t)DataObject->getDataPtr(), 0 }};
      case CustomBufferKind::CodeCompletionResultsArray:
        return {{ (uintptr_t)getVariantFunctionsForCodeCompletionResultsArray(),
          (uintptr_t)DataObject->getDataPtr(), 0 }};
    }
  }
  
  return {{ (uintptr_t)&SKDVariantFuncs, (uintptr_t)Object.get(), 0 }};
}
