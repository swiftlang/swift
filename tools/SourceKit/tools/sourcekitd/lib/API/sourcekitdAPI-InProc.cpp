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

#include "SourceKit/Core/LLVM.h"
#include "SourceKit/Support/UIdent.h"
#include "sourcekitd/CodeCompletionResultsArray.h"
#include "sourcekitd/DeclarationsArray.h"
#include "sourcekitd/DictionaryKeys.h"
#include "sourcekitd/DocStructureArray.h"
#include "sourcekitd/DocSupportAnnotationArray.h"
#include "sourcekitd/ExpressionTypeArray.h"
#include "sourcekitd/Internal.h"
#include "sourcekitd/Logging.h"
#include "sourcekitd/RawData.h"
#include "sourcekitd/TokenAnnotationsArray.h"
#include "sourcekitd/VariableTypeArray.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Basic/ThreadSafeRefCounted.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/StringRef.h"

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
    Double,
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
  virtual std::optional<int64_t> getInt64() const { return std::nullopt; }
  virtual std::optional<StringRef> getString() const { return std::nullopt; }
  virtual const char *getCString() const { return nullptr; }
  virtual bool getBool() const { return false; }
  virtual double getDouble() const { return 0.0; }
  virtual const void *getDataPtr() const { return nullptr; }
  virtual size_t getDataSize() const { return 0; }
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

  bool apply(
      llvm::function_ref<bool(sourcekitd_uid_t, SKDObjectRef)> Applier) const {
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

  std::optional<StringRef> getString() const override {
    return std::optional<StringRef>(Storage);
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

  std::optional<int64_t> getInt64() const override { return Storage; }

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

class SKDDouble : public SKDObject {
public:
  SKDDouble(double Value) : SKDObject(ObjectKind::Double), Storage(Value) {}

  sourcekitd_variant_type_t getVariantType() const override {
    return SOURCEKITD_VARIANT_TYPE_DOUBLE;
  }

  double getDouble() const override { return Storage; }

  static bool classof(const SKDObject *O) {
    return O->getKind() == ObjectKind::Double;
  }

private:
  double Storage;
};

} // end anonymous namespace

static sourcekitd_variant_t variantFromSKDObject(SKDObjectRef Object);
static sourcekitd_variant_t variantFromSKDObjectPtr(const SKDObject *Object);

namespace {

class SKDCustomData: public SKDObject {
public:
  SKDCustomData(std::unique_ptr<llvm::MemoryBuffer> MemBuf)
  : SKDObject(ObjectKind::CustomData), BufferPtr(std::move(MemBuf))
    {}

  SKDCustomData(SKDCustomData const&) = delete;
  SKDCustomData &operator=(SKDCustomData const&) = delete;

  sourcekitd_variant_type_t getVariantType() const override {
    switch (getBufferKind()) {
      case CustomBufferKind::TokenAnnotationsArray:
      case CustomBufferKind::DeclarationsArray:
      case CustomBufferKind::DocSupportAnnotationArray:
      case CustomBufferKind::CodeCompletionResultsArray:
      case CustomBufferKind::DocStructureArray:
      case CustomBufferKind::InheritedTypesArray:
      case CustomBufferKind::DocStructureElementArray:
      case CustomBufferKind::AttributesArray:
      case CustomBufferKind::ExpressionTypeArray:
      case CustomBufferKind::VariableTypeArray:
        return SOURCEKITD_VARIANT_TYPE_ARRAY;
      case CustomBufferKind::RawData:
        return SOURCEKITD_VARIANT_TYPE_DATA;
      default:
        return getPluginVariantFunctions(static_cast<size_t>(getBufferKind()))
            ->get_type(variantFromSKDObjectPtr(this));
      }
  }

  CustomBufferKind getBufferKind() const {
    return ((CustomBufferKind)*(const uint64_t*)_getStartPtr());
  }

  const void *getDataPtr() const override {
    return ((const void*)(((const uint64_t*)_getStartPtr())+1));
  }

  size_t getDataSize() const override {
    return BufferPtr->getBuffer().size() - sizeof(uint64_t);
  }

  static bool classof(const SKDObject *O) {
    return O->getKind() == ObjectKind::CustomData;
  }
private:
  std::unique_ptr<llvm::MemoryBuffer> BufferPtr;

  const void *_getStartPtr() const { return BufferPtr->getBuffer().data(); }
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
    if (OptInt.has_value()) {
      return static_cast<ImplClass*>(this)->visitInt64(OptInt.value());
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
    swift::writeEscaped(Str, OS);
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
sourcekitd_request_dictionary_get_value(sourcekitd_object_t dict,
                                        sourcekitd_uid_t key) {
  return static_cast<sourcekitd_object_t>(static_cast<SKDObject *>(dict)->get(key).get());
}

const char *sourcekitd_request_dictionary_get_string(sourcekitd_object_t dict,
                                                     sourcekitd_uid_t key) {
  auto value = static_cast<SKDObject *>(dict)->get(key);
  if (!value) {
    return nullptr;
  }
  return value->getCString();
}

int64_t sourcekitd_request_dictionary_get_int64(sourcekitd_object_t dict,
                                                sourcekitd_uid_t key) {
  return *static_cast<SKDObject *>(dict)->get(key)->getInt64();
}

bool sourcekitd_request_dictionary_get_bool(sourcekitd_object_t dict,
                                            sourcekitd_uid_t key) {
  return static_cast<SKDObject *>(dict)->get(key)->getBool();
}

sourcekitd_uid_t sourcekitd_request_dictionary_get_uid(sourcekitd_object_t dict,
                                                       sourcekitd_uid_t key) {
  return static_cast<SKDObject *>(dict)->get(key)->getUID();
}

size_t sourcekitd_request_array_get_count(sourcekitd_object_t array) {
  return static_cast<SKDObject *>(array)->getCount();
}

sourcekitd_object_t
sourcekitd_request_array_get_value(sourcekitd_object_t array, size_t index) {
  return static_cast<sourcekitd_object_t>(static_cast<SKDObject *>(array)->get(index).get());
}

const char *sourcekitd_request_array_get_string(sourcekitd_object_t array,
                                                size_t index) {
  return static_cast<SKDObject *>(array)->get(index)->getCString();
}

int64_t sourcekitd_request_array_get_int64(sourcekitd_object_t array,
                                           size_t index) {
  return *static_cast<SKDObject *>(array)->get(index)->getInt64();
}

bool sourcekitd_request_array_get_bool(sourcekitd_object_t array,
                                       size_t index) {
  return static_cast<SKDObject *>(array)->get(index)->getBool();
}

sourcekitd_uid_t sourcekitd_request_array_get_uid(sourcekitd_object_t array,
                                                  size_t index) {
  return static_cast<SKDObject *>(array)->get(index)->getUID();
}

int64_t sourcekitd_request_int64_get_value(sourcekitd_object_t obj) {
  return *static_cast<SKDObject *>(obj)->getInt64();
}

bool sourcekitd_request_bool_get_value(sourcekitd_object_t obj) {
  return static_cast<SKDObject *>(obj)->getBool();
}

size_t sourcekitd_request_string_get_length(sourcekitd_object_t obj) {
  return static_cast<SKDString *>(obj)->getString()->size();
}

const char *sourcekitd_request_string_get_ptr(sourcekitd_object_t obj) {
  return static_cast<SKDString *>(obj)->getCString();
}

sourcekitd_uid_t sourcekitd_request_uid_get_value(sourcekitd_object_t obj) {
  return static_cast<SKDUID *>(obj)->getUID();
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
                                      new SKDString(std::string(Str)));
}

void ResponseBuilder::Dictionary::set(UIdent Key, const std::string &Str) {
  static_cast<SKDObject *>(Impl)->set(SKDUIDFromUIdent(Key),
                                      new SKDString(std::string(Str)));
}

void ResponseBuilder::Dictionary::set(UIdent Key, int64_t Val) {
  static_cast<SKDObject *>(Impl)->set(SKDUIDFromUIdent(Key), new SKDInt64(Val));
}

void ResponseBuilder::Dictionary::set(SourceKit::UIdent Key,
                                      ArrayRef<StringRef> Strs) {
  auto ArrayObject = new SKDArray();
  for (auto Str : Strs) {
    ArrayObject->set(SOURCEKITD_ARRAY_APPEND, new SKDString(std::string(Str)));
  }
  static_cast<SKDObject *>(Impl)->set(SKDUIDFromUIdent(Key), ArrayObject);
}

void ResponseBuilder::Dictionary::set(SourceKit::UIdent Key,
                                      ArrayRef<std::string> Strs) {
  auto ArrayObject = new SKDArray();
  for (auto Str : Strs) {
    ArrayObject->set(SOURCEKITD_ARRAY_APPEND, new SKDString(std::string(Str)));
  }
  static_cast<SKDObject *>(Impl)->set(SKDUIDFromUIdent(Key), ArrayObject);
}

void ResponseBuilder::Dictionary::set(SourceKit::UIdent Key,
                                      ArrayRef<SourceKit::UIdent> UIDs) {
  auto ArrayObject = new SKDArray();
  for (auto UID : UIDs) {
    ArrayObject->set(SOURCEKITD_ARRAY_APPEND, new SKDUID(SKDUIDFromUIdent(UID)));
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
      SourceKit::UIdent Key, std::unique_ptr<llvm::MemoryBuffer> MemBuf) {
  static_cast<SKDObject *>(Impl)->set(SKDUIDFromUIdent(Key), 
                                      new SKDCustomData(std::move(MemBuf)));
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

sourcekitd_uid_t RequestDict::getUID(UIdent Key) const {
  auto Object = static_cast<SKDObject *>(Dict)->get(SKDUIDFromUIdent(Key));
  return Object ? Object->getUID() : nullptr;
}

std::optional<StringRef> RequestDict::getString(UIdent Key) const {
  if (auto Object = static_cast<SKDObject *>(Dict)->get(SKDUIDFromUIdent(Key))) {
    return Object->getString();
  }
  return std::nullopt;
}

std::optional<RequestDict>
RequestDict::getDictionary(SourceKit::UIdent Key) const {
  SKDDictionary *DictObject = nullptr;
  if (auto Object = static_cast<SKDObject *>(Dict)->get(SKDUIDFromUIdent(Key))) {
    DictObject = dyn_cast<SKDDictionary>(Object);
  }
  return DictObject ? std::optional<RequestDict>(RequestDict(DictObject))
                    : std::nullopt;
}

bool RequestDict::getStringArray(SourceKit::UIdent Key,
                                 llvm::SmallVectorImpl<const char *> &Arr,
                                 bool isOptional) const {
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
                              bool isOptional) const {
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
    SourceKit::UIdent Key,
    llvm::function_ref<bool(RequestDict)> Applier) const {
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
                           bool isOptional) const {
  auto Object = static_cast<SKDObject *>(Dict)->get(SKDUIDFromUIdent(Key));
  if (!Object)
    return !isOptional;
  Val = Object->getInt64().value_or(0);
  return false;
}

std::optional<int64_t>
RequestDict::getOptionalInt64(SourceKit::UIdent Key) const {
  auto Object = static_cast<SKDObject *>(Dict)->get(SKDUIDFromUIdent(Key));
  if (!Object)
    return std::nullopt;
  return Object->getInt64().value_or(0);
}

sourcekitd_response_t
sourcekitd::createErrorRequestInvalid(StringRef Description) {
  return retained(new SKDError(SOURCEKITD_ERROR_REQUEST_INVALID, 
                               Description));
}

sourcekitd_response_t
sourcekitd::createErrorRequestFailed(StringRef Description) {
  return retained(new SKDError(SOURCEKITD_ERROR_REQUEST_FAILED, 
                               Description));
}

sourcekitd_response_t
sourcekitd::createErrorRequestInterrupted(StringRef Description) {
  return retained(new SKDError(SOURCEKITD_ERROR_CONNECTION_INTERRUPTED,
                               Description));
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
    sourcekitd_variant_array_applier_f_t applier,
    void *context) {
  return dyn_cast<SKDArray>(SKD_OBJ(array))->apply([&](size_t Index, 
                                                       SKDObjectRef Object){
    return applier(Index, variantFromSKDObject(Object), context);
  });
}

static bool SKDVar_array_get_bool(sourcekitd_variant_t array, size_t index) {
  return SKD_OBJ(array)->get(index)->getBool();
}

static double SKDVar_array_get_double(sourcekitd_variant_t array,
                                      size_t index) {
  return SKD_OBJ(array)->get(index)->getDouble();
}

static size_t SKDVar_array_get_count(sourcekitd_variant_t array) {
  return SKD_OBJ(array)->getCount();
}

static int64_t SKDVar_array_get_int64(sourcekitd_variant_t array, size_t index) {
  return SKD_OBJ(array)->get(index)->getInt64().value_or(0);
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

static double SKDVar_double_get_value(sourcekitd_variant_t obj) {
  return SKD_OBJ(obj)->getDouble();
}

static bool SKDVar_dictionary_apply(
    sourcekitd_variant_t dict,
    sourcekitd_variant_dictionary_applier_f_t applier,
    void *context) {
  return dyn_cast<SKDDictionary>(SKD_OBJ(dict))->apply([&](sourcekitd_uid_t Key, 
                                                           SKDObjectRef Object){
    return applier(Key, variantFromSKDObject(Object), context);
  });
}

static bool
SKDVar_dictionary_get_bool(sourcekitd_variant_t dict, sourcekitd_uid_t key) {
  if (auto Object = SKD_OBJ(dict)->get(key)) {
    return Object->getBool();
  }
  return false;
}

static double SKDVar_dictionary_get_double(sourcekitd_variant_t dict,
                                           sourcekitd_uid_t key) {
  if (auto Object = SKD_OBJ(dict)->get(key)) {
    return Object->getDouble();
  }
  return 0.0;
}

static int64_t
SKDVar_dictionary_get_int64(sourcekitd_variant_t dict, sourcekitd_uid_t key) {
  if (auto Object = SKD_OBJ(dict)->get(key)) {
    return Object->getInt64().value_or(0);
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
  return String.has_value() ? String->size() : 0;
}

static const char *SKDVar_string_get_ptr(sourcekitd_variant_t obj) {
  return SKD_OBJ(obj)->getCString();
}

static int64_t SKDVar_int64_get_value(sourcekitd_variant_t obj) {
  return SKD_OBJ(obj)->getInt64().value_or(0);
}

static sourcekitd_uid_t SKDVar_uid_get_value(sourcekitd_variant_t obj) {
  return SKD_OBJ(obj)->getUID();
}

static const void *SKDVar_data_get_ptr(sourcekitd_variant_t obj) {
  return SKD_OBJ(obj)->getDataPtr();
}

static size_t SKDVar_data_get_size(sourcekitd_variant_t obj) {
  return SKD_OBJ(obj)->getDataSize();
}

static VariantFunctions SKDVariantFuncs = {
    SKDVar_get_type,
    SKDVar_array_apply,
    SKDVar_array_get_bool,
    SKDVar_array_get_double,
    SKDVar_array_get_count,
    SKDVar_array_get_int64,
    SKDVar_array_get_string,
    SKDVar_array_get_uid,
    SKDVar_array_get_value,
    SKDVar_bool_get_value,
    SKDVar_double_get_value,
    SKDVar_dictionary_apply,
    SKDVar_dictionary_get_bool,
    SKDVar_dictionary_get_double,
    SKDVar_dictionary_get_int64,
    SKDVar_dictionary_get_string,
    SKDVar_dictionary_get_value,
    SKDVar_dictionary_get_uid,
    SKDVar_string_get_length,
    SKDVar_string_get_ptr,
    SKDVar_int64_get_value,
    SKDVar_uid_get_value,
    SKDVar_data_get_size,
    SKDVar_data_get_ptr,
};

static sourcekitd_variant_t variantFromSKDObject(SKDObjectRef Object) {
  return variantFromSKDObjectPtr(Object.get());
}

static sourcekitd_variant_t variantFromSKDObjectPtr(const SKDObject *Object) {
  if (!Object)
    return makeNullVariant();

  if (auto DataObject = dyn_cast<SKDCustomData>(Object)) {
    switch(DataObject->getBufferKind()) {
      case CustomBufferKind::TokenAnnotationsArray:
        return {{ (uintptr_t)getVariantFunctionsForTokenAnnotationsArray(),
          (uintptr_t)DataObject->getDataPtr(), 0 }};
      case CustomBufferKind::DeclarationsArray:
        return {
            {(uintptr_t)sourcekitd::getVariantFunctionsForDeclarationsArray(),
             (uintptr_t)DataObject->getDataPtr(), 0}};
      case CustomBufferKind::DocSupportAnnotationArray:
        return {{ (uintptr_t)getVariantFunctionsForDocSupportAnnotationArray(),
          (uintptr_t)DataObject->getDataPtr(), 0 }};
      case CustomBufferKind::CodeCompletionResultsArray:
        return {{ (uintptr_t)getVariantFunctionsForCodeCompletionResultsArray(),
          (uintptr_t)DataObject->getDataPtr(), 0 }};
      case CustomBufferKind::DocStructureArray:
        return {{ (uintptr_t)getVariantFunctionsForDocStructureArray(),
          (uintptr_t)DataObject->getDataPtr(), ~size_t(0) }};
      case CustomBufferKind::InheritedTypesArray:
        return {{ (uintptr_t)getVariantFunctionsForInheritedTypesArray(),
          (uintptr_t)DataObject->getDataPtr(), 0 }};
      case CustomBufferKind::DocStructureElementArray:
        return {{ (uintptr_t)getVariantFunctionsForDocStructureElementArray(),
          (uintptr_t)DataObject->getDataPtr(), 0 }};
      case CustomBufferKind::AttributesArray:
        return {{ (uintptr_t)getVariantFunctionsForAttributesArray(),
          (uintptr_t)DataObject->getDataPtr(), 0 }};
      case CustomBufferKind::ExpressionTypeArray:
        return {{ (uintptr_t)getVariantFunctionsForExpressionTypeArray(),
          (uintptr_t)DataObject->getDataPtr(), 0 }};
      case CustomBufferKind::VariableTypeArray:
        return {{ (uintptr_t)getVariantFunctionsForVariableTypeArray(),
                  (uintptr_t)DataObject->getDataPtr(), 0 }};
      case CustomBufferKind::RawData:
        return {{ (uintptr_t)getVariantFunctionsForRawData(),
                  (uintptr_t)DataObject->getDataPtr(),
                  (uintptr_t)DataObject->getDataSize() }};
      default:
        return {{(uintptr_t)getPluginVariantFunctions(
                     static_cast<size_t>(DataObject->getBufferKind())),
                 (uintptr_t)DataObject->getDataPtr(),
                 (uintptr_t)DataObject->getDataSize()}};
    }
  }

  return {{(uintptr_t)&SKDVariantFuncs, (uintptr_t)Object, 0}};
}

sourcekitd_response_t
sourcekitd_response_error_create(sourcekitd_error_t kind,
                                 const char *description) {
  SKDError *error = new SKDError(kind, description);
  return retained(error);
}

sourcekitd_response_t
sourcekitd_response_dictionary_create(const sourcekitd_uid_t *keys,
                                      const sourcekitd_response_t *values,
                                      size_t count) {
  // Request and response dictionaries are represented by the same type.
  return sourcekitd_request_dictionary_create(keys, values, count);
}

void sourcekitd_response_dictionary_set_value(sourcekitd_response_t dict,
                                              sourcekitd_uid_t key,
                                              sourcekitd_response_t value) {
  static_cast<SKDObject *>(dict)->set(key, static_cast<SKDObject *>(value));
}

void sourcekitd_response_dictionary_set_string(sourcekitd_response_t dict,
                                               sourcekitd_uid_t key,
                                               const char *string) {
  static_cast<SKDObject *>(dict)->set(key, new SKDString(std::string(string)));
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
  static_cast<SKDObject *>(dict)->set(key, new SKDInt64(val));
}

void sourcekitd_response_dictionary_set_bool(sourcekitd_response_t dict,
                                             sourcekitd_uid_t key, bool val) {
  static_cast<SKDObject *>(dict)->set(key, new SKDBool(val));
}

void sourcekitd_response_dictionary_set_double(sourcekitd_response_t dict,
                                               sourcekitd_uid_t key,
                                               double val) {
  static_cast<SKDObject *>(dict)->set(key, new SKDDouble(val));
}

void sourcekitd_response_dictionary_set_uid(sourcekitd_response_t dict,
                                            sourcekitd_uid_t key,
                                            sourcekitd_uid_t uid) {
  static_cast<SKDObject *>(dict)->set(key, new SKDUID(uid));
}

sourcekitd_response_t
sourcekitd_response_array_create(const sourcekitd_response_t *objects,
                                 size_t count) {
  // Request and response arrays are represented by the same type.
  return sourcekitd_request_array_create(objects, count);
}

void sourcekitd_response_array_set_value(sourcekitd_response_t array,
                                         size_t index,
                                         sourcekitd_response_t value) {
  static_cast<SKDObject *>(array)->set(index, static_cast<SKDObject *>(value));
}

void sourcekitd_response_array_set_string(sourcekitd_response_t array,
                                          size_t index, const char *string) {
  static_cast<SKDObject *>(array)->set(index,
                                       new SKDString(std::string(string)));
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
  static_cast<SKDObject *>(array)->set(index, new SKDInt64(val));
}

void sourcekitd_response_array_set_double(sourcekitd_response_t array,
                                          size_t index, double val) {
  static_cast<SKDObject *>(array)->set(index, new SKDDouble(val));
}

void sourcekitd_response_array_set_uid(sourcekitd_response_t array,
                                       size_t index, sourcekitd_uid_t uid) {
  static_cast<SKDObject *>(array)->set(index, new SKDUID(uid));
}

sourcekitd_response_t sourcekitd_response_retain(sourcekitd_response_t object) {
  return retained(static_cast<SKDObject *>(object));
}

sourcekitd_variant_type_t sourcekitd_request_get_type(sourcekitd_object_t obj) {
  if (!obj) {
    return SOURCEKITD_VARIANT_TYPE_NULL;
  }
  switch (static_cast<SKDObject *>(obj)->getKind()) {
  case SKDObject::ObjectKind::Dictionary:
    return SOURCEKITD_VARIANT_TYPE_DICTIONARY;
  case SKDObject::ObjectKind::Array:
    return SOURCEKITD_VARIANT_TYPE_ARRAY;
  case SKDObject::ObjectKind::String:
    return SOURCEKITD_VARIANT_TYPE_STRING;
  case SKDObject::ObjectKind::Int64:
    return SOURCEKITD_VARIANT_TYPE_INT64;
  case SKDObject::ObjectKind::UID:
    return SOURCEKITD_VARIANT_TYPE_UID;
  case SKDObject::ObjectKind::Bool:
    return SOURCEKITD_VARIANT_TYPE_BOOL;
  case SKDObject::ObjectKind::Double:
    return SOURCEKITD_VARIANT_TYPE_DOUBLE;
  case SKDObject::ObjectKind::CustomData:
    return static_cast<SKDObject *>(obj)->getVariantType();
  case SKDObject::ObjectKind::Error:
    llvm_unreachable("Should not be part of a request");
  }
}

void sourcekitd_response_dictionary_set_custom_buffer(
    sourcekitd_response_t dict, sourcekitd_uid_t key, const void *ptr,
    size_t size) {
#ifndef NDEBUG
  assert(size >= sizeof(uint64_t));
  auto bufKind = *(const uint64_t *)ptr;
  assert(bufKind >= (uint64_t)CustomBufferKind::CustomBufferKind_End);
#endif
  std::unique_ptr<llvm::WritableMemoryBuffer> Buf;
  Buf = llvm::WritableMemoryBuffer::getNewUninitMemBuffer(size);
  memcpy(Buf->getBufferStart(), ptr, size);
  static_cast<SKDObject *>(dict)->set(key,
                                      new SKDCustomData(std::move(Buf)));
}
