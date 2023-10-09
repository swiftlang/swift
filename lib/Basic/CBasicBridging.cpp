//===--- CBasicBridging.cpp -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/CBasicBridging.h"
#include "llvm/Support/JSON.h"

void BridgedData_free(BridgedData data) {
  if (data.baseAddress == nullptr)
    return;
  free(const_cast<char *>(data.baseAddress));
}

//===----------------------------------------------------------------------===//
// JSON
//===----------------------------------------------------------------------===//

void *JSON_newValue() { return new llvm::json::Value(nullptr); }

void *JSON_deserializedValue(BridgedData data) {
  auto result = llvm::json::parse({data.baseAddress, data.size});
  if (!result) {
    return nullptr;
  }
  return new llvm::json::Value(std::move(*result));
}

void JSON_value_serialize(void *value, BridgedData *out) {
  llvm::SmallVector<char, 0> result;
  llvm::raw_svector_ostream OS(result);
  OS << *static_cast<llvm::json::Value *>(value);

  auto outPtr = malloc(result.size());
  memcpy(outPtr, result.data(), result.size());
  *out = BridgedData{(const char *)outPtr, (size_t)result.size()};
}

void JSON_value_delete(void *value) {
  delete static_cast<llvm::json::Value *>(value);
}

_Bool JSON_value_getAsNull(void *value) {
  if (auto val = static_cast<llvm::json::Value *>(value)->getAsNull()) {
    return true;
  }
  return false;
}

_Bool JSON_value_getAsBoolean(void *value, _Bool *result) {
  if (auto val = static_cast<llvm::json::Value *>(value)->getAsBoolean()) {
    *result = *val;
    return false;
  }
  return true;
}

_Bool JSON_value_getAsDouble(void *value, double *result) {
  if (auto val = static_cast<llvm::json::Value *>(value)->getAsNumber()) {
    *result = *val;
    return false;
  }
  return true;
}

_Bool JSON_value_getAsInteger(void *value, int64_t *result) {
  if (auto val = static_cast<llvm::json::Value *>(value)->getAsInteger()) {
    *result = *val;
    return false;
  }
  return true;
}

_Bool JSON_value_getAsString(void *value, BridgedData *result) {
  if (auto val = static_cast<llvm::json::Value *>(value)->getAsString()) {
    *result = {val->data(), val->size()};
    return false;
  }
  return true;
}

_Bool JSON_value_getAsObject(void *value, void **result) {
  if (auto val = static_cast<llvm::json::Value *>(value)->getAsObject()) {
    *result = val;
    return false;
  }
  return true;
}
_Bool JSON_value_getAsArray(void *value, void **result) {
  if (auto val = static_cast<llvm::json::Value *>(value)->getAsArray()) {
    *result = val;
    return false;
  }
  return true;
}

size_t JSON_object_getSize(void *objectPtr) {
  llvm::json::Object *object = static_cast<llvm::json::Object *>(objectPtr);
  return object->size();
}

BridgedData JSON_object_getKey(void *objectPtr, size_t i) {
  llvm::json::Object *object = static_cast<llvm::json::Object *>(objectPtr);
  std::map<int, float> map;
  auto iter = object->begin();
  std::advance(iter, i);
  auto str = llvm::StringRef(iter->first);
  return {str.data(), str.size()};
}

_Bool JSON_object_hasKey(void *objectPtr, const char *key) {
  llvm::json::Object *object = static_cast<llvm::json::Object *>(objectPtr);
  return object->find(key) != object->end();
}
void *JSON_object_getValue(void *objectPtr, const char *key) {
  llvm::json::Object *object = static_cast<llvm::json::Object *>(objectPtr);
  return object->get(key);
}

size_t JSON_array_getSize(void *objectPtr) {
  llvm::json::Array *array = static_cast<llvm::json::Array *>(objectPtr);
  return array->size();
}
void *JSON_array_getValue(void *objectPtr, size_t index) {
  llvm::json::Array *array = static_cast<llvm::json::Array *>(objectPtr);
  return array->data() + index;
}

void JSON_value_emplaceNull(void *valuePtr) {
  auto *value = static_cast<llvm::json::Value *>(valuePtr);
  *value = nullptr;
}
void JSON_value_emplaceBoolean(void *valuePtr, _Bool newValue) {
  auto *value = static_cast<llvm::json::Value *>(valuePtr);
  *value = newValue;
}
void JSON_value_emplaceString(void *valuePtr, const char *newValue) {
  auto *value = static_cast<llvm::json::Value *>(valuePtr);
  *value = std::string(newValue);
}
void JSON_value_emplaceDouble(void *valuePtr, double newValue) {
  auto *value = static_cast<llvm::json::Value *>(valuePtr);
  *value = newValue;
}
void JSON_value_emplaceInteger(void *valuePtr, int64_t newValue) {
  auto *value = static_cast<llvm::json::Value *>(valuePtr);
  *value = newValue;
}
void *JSON_value_emplaceNewObject(void *valuePtr) {
  auto *value = static_cast<llvm::json::Value *>(valuePtr);
  *value = llvm::json::Object();
  return value->getAsObject();
}
void *JSON_value_emplaceNewArray(void *valuePtr) {
  auto *value = static_cast<llvm::json::Value *>(valuePtr);
  *value = llvm::json::Array();
  return value->getAsArray();
}

void JSON_object_setNull(void *objectPtr, const char *key) {
  llvm::json::Object *object = static_cast<llvm::json::Object *>(objectPtr);
  auto keyStr = std::string(key);
  (*object)[keyStr] = nullptr;
}
void JSON_object_setBoolean(void *objectPtr, const char *key, _Bool value) {
  llvm::json::Object *object = static_cast<llvm::json::Object *>(objectPtr);
  auto keyStr = std::string(key);
  (*object)[keyStr] = value;
}
void JSON_object_setString(void *objectPtr, const char *key,
                           const char *value) {
  llvm::json::Object *object = static_cast<llvm::json::Object *>(objectPtr);
  auto keyStr = std::string(key);
  (*object)[keyStr] = std::string(value);
}
void JSON_object_setDouble(void *objectPtr, const char *key, double value) {
  llvm::json::Object *object = static_cast<llvm::json::Object *>(objectPtr);
  auto keyStr = std::string(key);
  (*object)[keyStr] = value;
}
void JSON_object_setInteger(void *objectPtr, const char *key, int64_t value) {
  llvm::json::Object *object = static_cast<llvm::json::Object *>(objectPtr);
  auto keyStr = std::string(key);
  (*object)[keyStr] = value;
}
void *JSON_object_setNewObject(void *objectPtr, const char *key) {
  llvm::json::Object *object = static_cast<llvm::json::Object *>(objectPtr);
  auto keyStr = std::string(key);
  (*object)[keyStr] = llvm::json::Object();
  return object->getObject(keyStr);
}
void *JSON_object_setNewArray(void *objectPtr, const char *key) {
  llvm::json::Object *object = static_cast<llvm::json::Object *>(objectPtr);
  auto keyStr = std::string(key);
  (*object)[keyStr] = llvm::json::Array();
  return object->getArray(keyStr);
}
void *JSON_object_setNewValue(void *objectPtr, const char *key) {
  llvm::json::Object *object = static_cast<llvm::json::Object *>(objectPtr);
  auto keyStr = std::string(key);
  (*object)[keyStr] = llvm::json::Value(nullptr);
  return object->get(keyStr);
}

void JSON_array_pushNull(void *arrayPtr) {
  llvm::json::Array *array = static_cast<llvm::json::Array *>(arrayPtr);
  array->emplace_back(nullptr);
}
void JSON_array_pushBoolean(void *arrayPtr, _Bool value) {
  llvm::json::Array *array = static_cast<llvm::json::Array *>(arrayPtr);
  array->emplace_back(value);
}
void JSON_array_pushString(void *arrayPtr, const char *value) {
  llvm::json::Array *array = static_cast<llvm::json::Array *>(arrayPtr);
  array->emplace_back(std::string(value));
}
void JSON_array_pushDouble(void *arrayPtr, double value) {
  llvm::json::Array *array = static_cast<llvm::json::Array *>(arrayPtr);
  array->emplace_back(value);
}
void JSON_array_pushInteger(void *arrayPtr, int64_t value) {
  llvm::json::Array *array = static_cast<llvm::json::Array *>(arrayPtr);
  array->emplace_back(value);
}
void *JSON_array_pushNewObject(void *arrayPtr) {
  llvm::json::Array *array = static_cast<llvm::json::Array *>(arrayPtr);
  array->emplace_back(llvm::json::Object());
  return array->back().getAsObject();
}
void *JSON_array_pushNewArray(void *arrayPtr) {
  llvm::json::Array *array = static_cast<llvm::json::Array *>(arrayPtr);
  array->emplace_back(llvm::json::Array());
  return array->back().getAsArray();
}
void *JSON_array_pushNewValue(void *arrayPtr) {
  llvm::json::Array *array = static_cast<llvm::json::Array *>(arrayPtr);
  array->emplace_back(nullptr);
  return &array->back();
}
