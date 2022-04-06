//===--- Context.cpp - Demangler Context ----------------------------------===//
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
//  This file implements the demangler Context.
//
//===----------------------------------------------------------------------===//

#include "swift/Demangling/Demangler.h"
#include "swift/Demangling/ManglingMacros.h"
#include "swift/Demangling/ManglingUtils.h"
#include "swift/Demangling/NamespaceMacros.h"

namespace swift {
namespace Demangle {
SWIFT_BEGIN_INLINE_NAMESPACE

//////////////////////////////////
// Context member functions     //
//////////////////////////////////

Context::Context() : D(new Demangler) {
}

Context::~Context() {
  delete D;
}

void Context::clear() {
  D->clear();
}

NodePointer Context::demangleSymbolAsNode(llvm::StringRef MangledName) {
#if SWIFT_SUPPORT_OLD_MANGLING
  if (isMangledName(MangledName)) {
    return D->demangleSymbol(MangledName);
  }
  return demangleOldSymbolAsNode(MangledName, *D);
#else
  return D->demangleSymbol(MangledName);
#endif
}

NodePointer Context::demangleTypeAsNode(llvm::StringRef MangledName) {
  return D->demangleType(MangledName);
}

#if SWIFT_STDLIB_HAS_TYPE_PRINTING

std::string Context::demangleSymbolAsString(llvm::StringRef MangledName,
                                            const DemangleOptions &Options) {
  NodePointer root = demangleSymbolAsNode(MangledName);
  if (!root) return MangledName.str();

  std::string demangling = nodeToString(root, Options);
  if (demangling.empty())
    return MangledName.str();
  return demangling;
}

std::string Context::demangleTypeAsString(llvm::StringRef MangledName,
                                          const DemangleOptions &Options) {
  NodePointer root = demangleTypeAsNode(MangledName);
  if (!root) return MangledName.str();
  
  std::string demangling = nodeToString(root, Options);
  if (demangling.empty())
    return MangledName.str();
  return demangling;
}

#endif

// Removes a '.<n>' suffix from \p Name. <n> is either a number or a combination of
// '.<other-text>.<n>'.
// Such symbols are produced in IRGen or in LLVM optimizations.
static llvm::StringRef stripSuffix(llvm::StringRef Name) {
  // A suffix always ends with a digit. Do this quick check to avoid scanning through the whole
  // symbol name if the symbol has no suffix (= the common case).
  if (swift::Mangle::isDigit(Name.back())) {
    size_t dotPos = Name.find('.');
    if (dotPos != StringRef::npos) {
      Name = Name.substr(0, dotPos);
    }
  }
  return Name;
}

bool Context::isThunkSymbol(llvm::StringRef MangledName) {
  if (isMangledName(MangledName)) {
    MangledName = stripSuffix(MangledName);
    // First do a quick check
    if (MangledName.endswith("TA") ||  // partial application forwarder
        MangledName.endswith("Ta") ||  // ObjC partial application forwarder
        MangledName.endswith("To") ||  // swift-as-ObjC thunk
        MangledName.endswith("TO") ||  // ObjC-as-swift thunk
        MangledName.endswith("TR") ||  // reabstraction thunk helper function
        MangledName.endswith("Tr") ||  // reabstraction thunk
        MangledName.endswith("TW") ||  // protocol witness thunk
        MangledName.endswith("fC")) {  // allocating constructor

      // To avoid false positives, we need to fully demangle the symbol.
      NodePointer Nd = D->demangleSymbol(MangledName);
      if (!Nd || Nd->getKind() != Node::Kind::Global ||
          Nd->getNumChildren() == 0)
        return false;

      switch (Nd->getFirstChild()->getKind()) {
        case Node::Kind::ObjCAttribute:
        case Node::Kind::NonObjCAttribute:
        case Node::Kind::PartialApplyObjCForwarder:
        case Node::Kind::PartialApplyForwarder:
        case Node::Kind::ReabstractionThunkHelper:
        case Node::Kind::ReabstractionThunk:
        case Node::Kind::ProtocolWitness:
        case Node::Kind::Allocator:
          return true;
        default:
          break;
      }
    }
    return false;
  }

  if (MangledName.startswith("_T")) {
    // Old mangling.
    StringRef Remaining = MangledName.substr(2);
    if (Remaining.startswith("To") ||   // swift-as-ObjC thunk
        Remaining.startswith("TO") ||   // ObjC-as-swift thunk
        Remaining.startswith("PA_") ||  // partial application forwarder
        Remaining.startswith("PAo_")) { // ObjC partial application forwarder
      return true;
    }
  }
  return false;
}

std::string Context::getThunkTarget(llvm::StringRef MangledName) {
  if (!isThunkSymbol(MangledName))
    return std::string();

  if (isMangledName(MangledName)) {
    // If the symbol has a suffix we cannot derive the target.
    if (stripSuffix(MangledName) != MangledName)
      return std::string();

    // The targets of those thunks not derivable from the mangling.
    if (MangledName.endswith("TR") ||
        MangledName.endswith("Tr") ||
        MangledName.endswith("TW") )
      return std::string();

    if (MangledName.endswith("fC")) {
      std::string target = MangledName.str();
      target[target.size() - 1] = 'c';
      return target;
    }

    return MangledName.substr(0, MangledName.size() - 2).str();
  }
  // Old mangling.
  assert(MangledName.startswith("_T"));
  StringRef Remaining = MangledName.substr(2);
  if (Remaining.startswith("PA_"))
    return Remaining.substr(3).str();
  if (Remaining.startswith("PAo_"))
    return Remaining.substr(4).str();
  assert(Remaining.startswith("To") || Remaining.startswith("TO"));
  return std::string("_T") + Remaining.substr(2).str();
}

bool Context::hasSwiftCallingConvention(llvm::StringRef MangledName) {
  Node *Global = demangleSymbolAsNode(MangledName);
  if (!Global || Global->getKind() != Node::Kind::Global ||
      Global->getNumChildren() == 0)
    return false;

  Node *TopLevel = Global->getFirstChild();
  switch (TopLevel->getKind()) {
    // Functions, which don't have the swift calling conventions:
    case Node::Kind::TypeMetadataAccessFunction:
    case Node::Kind::ValueWitness:
    case Node::Kind::ProtocolWitnessTableAccessor:
    case Node::Kind::GenericProtocolWitnessTableInstantiationFunction:
    case Node::Kind::LazyProtocolWitnessTableAccessor:
    case Node::Kind::AssociatedTypeMetadataAccessor:
    case Node::Kind::AssociatedTypeWitnessTableAccessor:
    case Node::Kind::BaseWitnessTableAccessor:
    case Node::Kind::ObjCAttribute:
      return false;
    default:
      break;
  }
  return true;
}

std::string Context::getModuleName(llvm::StringRef mangledName) {
  NodePointer node = demangleSymbolAsNode(mangledName);
  while (node) {
    switch (node->getKind()) {
    case Demangle::Node::Kind::Module:
      return node->getText().str();
    case Demangle::Node::Kind::TypeMangling:
    case Demangle::Node::Kind::Type:
      node = node->getFirstChild();
      break;
    case Demangle::Node::Kind::Global: {
      NodePointer newNode = nullptr;
      for (NodePointer child : *node) {
        if (!isFunctionAttr(child->getKind())) {
          newNode = child;
          break;
        }
      }
      node = newNode;
      break;
    }
    default:
      if (isSpecialized(node)) {
        auto unspec = getUnspecialized(node, *D);
        if (!unspec.isSuccess())
          node = nullptr;
        else
          node = unspec.result();
        break;
      }
      if (isContext(node->getKind())) {
        node = node->getFirstChild();
        break;
      }
      return std::string();
    }
  }
  return std::string();
}


//////////////////////////////////
// Public utility functions     //
//////////////////////////////////

#if SWIFT_STDLIB_HAS_TYPE_PRINTING

std::string demangleSymbolAsString(const char *MangledName,
                                   size_t MangledNameLength,
                                   const DemangleOptions &Options) {
  Context Ctx;
  return Ctx.demangleSymbolAsString(StringRef(MangledName, MangledNameLength),
                                    Options);
}

std::string demangleTypeAsString(const char *MangledName,
                                 size_t MangledNameLength,
                                 const DemangleOptions &Options) {
  Context Ctx;
  return Ctx.demangleTypeAsString(StringRef(MangledName, MangledNameLength),
                                  Options);
}

#endif

SWIFT_END_INLINE_NAMESPACE
} // namespace Demangle
} // namespace swift
