//===--- APIGen.cpp - Swift API Generation --------------------------------===//
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
//
//  This file implements the entrypoints into API file generation.
//
//===----------------------------------------------------------------------===//


#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/JSON.h"
#include "llvm/Support/raw_ostream.h"

#include "APIGen.h"

namespace swift {
namespace apigen {

void API::addSymbol(llvm::StringRef symbol, APILoc loc, APILinkage linkage,
                    APIFlags flags, APIAccess access,
                    APIAvailability availability) {
  auto *global = new (allocator) GlobalRecord(
      symbol, loc, linkage, flags, access, GVKind::Function, availability);
  globals.push_back(global);
}

ObjCInterfaceRecord *API::addObjCClass(llvm::StringRef name, APILinkage linkage,
                                       APILoc loc, APIAccess access,
                                       APIAvailability availability,
                                       llvm::StringRef superClassName) {
  auto *interface = new (allocator) ObjCInterfaceRecord(
      name, linkage, loc, access, availability, superClassName);
  interfaces.push_back(interface);
  return interface;
}

void API::addObjCMethod(ObjCInterfaceRecord *cls, llvm::StringRef name,
                        APILoc loc, APIAccess access, bool isInstanceMethod,
                        bool isOptional, APIAvailability availability) {
  auto method = new (allocator) ObjCMethodRecord(
      name, loc, access, isInstanceMethod, isOptional, availability);
  cls->methods.push_back(method);
}

static void serialize(llvm::json::OStream &OS, APIAccess access) {
  switch (access) {
  case APIAccess::Public:
    OS.attribute("access", "public");
    break;
  case APIAccess::Private:
    OS.attribute("access", "private");
    break;
  case APIAccess::Project:
    OS.attribute("access", "project");
    break;
  case APIAccess::Unknown:
    break;
  }
}

static void serialize(llvm::json::OStream &OS, APIAvailability availability) {
  if (availability.empty())
    return;
  if (!availability.introduced.empty())
    OS.attribute("introduced", availability.introduced);
  if (!availability.obsoleted.empty())
    OS.attribute("obsoleted", availability.obsoleted);
  if (availability.unavailable)
    OS.attribute("unavailable", availability.unavailable);
}

static void serialize(llvm::json::OStream &OS, APILinkage linkage) {
  switch (linkage) {
  case APILinkage::Exported:
    OS.attribute("linkage", "exported");
    break;
  case APILinkage::Reexported:
    OS.attribute("linkage", "re-exported");
    break;
  case APILinkage::Internal:
    OS.attribute("linkage", "internal");
    break;
  case APILinkage::External:
    OS.attribute("linkage", "external");
    break;
  case APILinkage::Unknown:
    // do nothing;
    break;
  }
}

static void serialize(llvm::json::OStream &OS, APILoc loc) {
  OS.attribute("file", loc.getFilename());
}

static void serialize(llvm::json::OStream &OS, const GlobalRecord &record) {
  OS.object([&]() {
    OS.attribute("name", record.name);
    serialize(OS, record.access);
    serialize(OS, record.loc);
    serialize(OS, record.linkage);
    serialize(OS, record.availability);
  });
}

static void serialize(llvm::json::OStream &OS, const ObjCMethodRecord &record) {
  OS.object([&]() {
    OS.attribute("name", record.name);
    serialize(OS, record.access);
    serialize(OS, record.loc);
    serialize(OS, record.linkage);
    serialize(OS, record.availability);
    if (record.isOptional)
      OS.attribute("optional", record.isOptional);
  });
}

static bool sortAPIRecords(const APIRecord *base, const APIRecord *compare) {
  return base->name < compare->name;
}

static void serialize(llvm::json::OStream &OS,
                      const ObjCInterfaceRecord &record) {
  OS.object([&]() {
    OS.attribute("name", record.name);
    serialize(OS, record.access);
    serialize(OS, record.loc);
    serialize(OS, record.linkage);
    serialize(OS, record.availability);
    OS.attribute("super", record.superClassName);
    OS.attributeArray("instanceMethods", [&]() {
      for (auto &method : record.methods) {
        if (method->isInstanceMethod)
          serialize(OS, *method);
      }
    });
    OS.attributeArray("classMethods", [&]() {
      for (auto &method : record.methods) {
        if (!method->isInstanceMethod)
          serialize(OS, *method);
      }
    });
  });
}

void API::writeAPIJSONFile(llvm::raw_ostream &os, bool PrettyPrint) {
  unsigned indentSize = PrettyPrint ? 2 : 0;
  llvm::json::OStream JSON(os, indentSize);

  JSON.object([&]() {
    JSON.attribute("target", target.str());
    JSON.attributeArray("globals", [&]() {
      llvm::sort(globals, sortAPIRecords);
      for (const auto *g : globals)
        serialize(JSON, *g);
    });
    JSON.attributeArray("interfaces", [&]() {
      llvm::sort(interfaces, sortAPIRecords);
      for (const auto *i : interfaces)
        serialize(JSON, *i);
    });
    JSON.attribute("version", "1.0");
  });
}

} // end namespace apigen
} // end namespace swift
