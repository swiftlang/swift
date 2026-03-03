//===--- SwiftMangling.cpp ------------------------------------------------===//
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

#include "SwiftLangSupport.h"

#include "swift/Demangling/Demangle.h"
#include "swift/Demangling/Demangler.h"

using namespace SourceKit;
using namespace swift;
using namespace ide;

void SwiftLangSupport::demangleNames(
    ArrayRef<const char *> MangledNames, bool Simplified,
    std::function<void(const RequestResult<ArrayRef<std::string>> &)>
        Receiver) {

  swift::Demangle::DemangleOptions DemangleOptions;
  if (Simplified) {
    DemangleOptions =
        swift::Demangle::DemangleOptions::SimplifiedUIDemangleOptions();
  }

  auto getDemangledName = [&](StringRef MangledName) -> std::string {
    if (!swift::Demangle::isSwiftSymbol(MangledName))
      return std::string(); // Not a mangled name

    std::string Result =
        swift::Demangle::demangleSymbolAsString(MangledName, DemangleOptions);

    if (Result == MangledName)
      return std::string(); // Not a mangled name

    return Result;
  };

  SmallVector<std::string, 4> results;
  for (auto &MangledName : MangledNames) {
    results.push_back(getDemangledName(MangledName));
  }

  Receiver(RequestResult<ArrayRef<std::string>>::fromResult(results));
}

static ManglingErrorOr<std::string> mangleSimpleClass(StringRef moduleName,
                                                      StringRef className) {
  using namespace swift::Demangle;
  Demangler Dem;
  auto moduleNode = Dem.createNode(Node::Kind::Module, moduleName);
  auto IdNode = Dem.createNode(Node::Kind::Identifier, className);
  auto classNode = Dem.createNode(Node::Kind::Class);
  auto typeNode = Dem.createNode(Node::Kind::Type);
  auto typeManglingNode = Dem.createNode(Node::Kind::TypeMangling);
  auto globalNode = Dem.createNode(Node::Kind::Global);

  classNode->addChild(moduleNode, Dem);
  classNode->addChild(IdNode, Dem);
  typeNode->addChild(classNode, Dem);
  typeManglingNode->addChild(typeNode, Dem);
  globalNode->addChild(typeManglingNode, Dem);
  return mangleNode(globalNode, Mangle::ManglingFlavor::Default);
}

void SwiftLangSupport::mangleSimpleClassNames(
    ArrayRef<std::pair<StringRef, StringRef>> ModuleClassPairs,
    std::function<void(const RequestResult<ArrayRef<std::string>> &)>
        Receiver) {

  SmallVector<std::string, 4> results;
  for (auto &pair : ModuleClassPairs) {
    auto mangling = mangleSimpleClass(pair.first, pair.second);
    if (!mangling.isSuccess()) {
      std::string message = "name mangling failed for ";
      message += pair.first;
      message += ".";
      message += pair.second;
      Receiver(RequestResult<ArrayRef<std::string>>::fromError(message));
      return;
    }
    results.push_back(mangling.result());
  }
  Receiver(RequestResult<ArrayRef<std::string>>::fromResult(results));
}
