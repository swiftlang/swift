//===--- SideCarWriter.cpp - Side Car Writer --------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements the \c SideCarWriter class that writes out
// source side-car data providing additional information about source
// code as a separate input, such as the non-nil/nilable annotations
// for method parameters.
//
//===----------------------------------------------------------------------===//
#include "swift/SideCar/SideCarWriter.h"
#include "SideCarFormat.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringMap.h"
#include <tuple>
#include <vector>
using namespace swift;
using namespace side_car;

class SideCarWriter::Implementation {
  /// Mapping from strings to identifier IDs.
  llvm::StringMap<IdentifierID> IdentifierIDs;

  /// Mapping from names to class ID.
  llvm::StringMap<ClassID> ClassIDs;

  /// Mapping from names to module ID.
  llvm::StringMap<ModuleID> ModuleIDs;

  /// Mapping from selectors to selector ID.
  llvm::StringMap<SelectorID> SelectorIDs;

public:
  /// Information about Objective-C classes.
  ///
  /// Indexed by the class ID, and provides a sequence of (module ID,
  /// class info) tuples with information about the class scoped to a particular
  /// module.
  llvm::DenseMap<
    unsigned,
    std::vector<std::tuple<ModuleID, ObjCClassInfo>>> ObjCClasses;

  /// Information about Objective-C properties.
  ///
  /// Indexed by the class ID and property name.
  llvm::DenseMap<
    std::pair<unsigned, unsigned>,
    std::vector<std::tuple<ModuleID, ObjCPropertyInfo>>> ObjCProperties;

  /// Information about Objective-C methods.
  ///
  /// Indexed by the class ID and selector ID.
  llvm::DenseMap<
    std::pair<unsigned, unsigned>,
    std::vector<std::tuple<bool, ModuleID, ObjCMethodInfo>>> ObjCMethods;

  /// Retrieve the ID for the given identifier.
  IdentifierID getIdentifier(StringRef identifier) {
    auto known = IdentifierIDs.find(identifier);
    if (known != IdentifierIDs.end())
      return known->second;

    // Add to the identifier table.
    known = IdentifierIDs.insert({identifier, IdentifierIDs.size()}).first;
    return known->second;
  }

  /// Retrieve the ID for the given class.
  ClassID getClass(StringRef name) {
    auto known = ClassIDs.find(name);
    if (known != ClassIDs.end())
      return known->second;

    // Add to the class table.
    known = ClassIDs.insert({name, ClassIDs.size()}).first;
    return known->second;
  }

  /// Retrieve the ID for the given module.
  ModuleID getModule(StringRef name) {
    auto known = ModuleIDs.find(name);
    if (known != ModuleIDs.end())
      return known->second;

    // Add to the module table.
    known = ModuleIDs.insert({name, ModuleIDs.size()}).first;
    return known->second;
  }

  /// Retrieve the ID for the given selector.
  SelectorID getSelector(StringRef name) {
    auto known = SelectorIDs.find(name);
    if (known != SelectorIDs.end())
      return known->second;

    // Add to the selector table.
    known = SelectorIDs.insert({name, SelectorIDs.size()}).first;
    return known->second;
  }
};

SideCarWriter::SideCarWriter()
  : Impl(*new Implementation)
{
}

SideCarWriter::~SideCarWriter() {
  delete &Impl;
}

void SideCarWriter::addObjCClass(StringRef moduleName, StringRef name,
                                 const ObjCClassInfo &info) {
  ModuleID moduleID = Impl.getModule(moduleName);
  ClassID classID = Impl.getClass(name);
  Impl.ObjCClasses[classID].push_back({moduleID, info});
}

void SideCarWriter::addObjCProperty(StringRef moduleName, StringRef className,
                                    StringRef name,
                                    const ObjCPropertyInfo &info) {
  ModuleID moduleID = Impl.getModule(moduleName);
  ClassID classID = Impl.getClass(className);
  IdentifierID nameID = Impl.getIdentifier(name);
  Impl.ObjCProperties[{classID, nameID}].push_back({moduleID, info});
}

void SideCarWriter::addObjCMethod(StringRef moduleName, StringRef className,
                                  StringRef selector, bool isInstanceMethod,
                                  const ObjCMethodInfo &info) {
  ModuleID moduleID = Impl.getModule(moduleName);
  ClassID classID = Impl.getClass(className);
  SelectorID selectorID = Impl.getSelector(selector);
  Impl.ObjCMethods[{classID, selectorID}]
    .push_back({isInstanceMethod, moduleID, info});
}

