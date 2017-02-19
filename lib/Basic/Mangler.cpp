//===--- Mangler.cpp - Base class for Swift name mangling -----------------===//
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

#define CHECK_MANGLING_AGAINST_OLD

#include "swift/Basic/Mangler.h"
#include "swift/Basic/Punycode.h"
#include "swift/Basic/ManglingMacros.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/CommandLine.h"
#ifdef CHECK_MANGLING_AGAINST_OLD
#include "swift/Basic/Demangle.h"
#include "swift/Basic/DemangleWrappers.h"
#endif
#include <algorithm>

using namespace swift;
using namespace NewMangling;

llvm::cl::opt<bool> NewManglingForTests(
                       "new-mangling-for-tests", llvm::cl::init(false),
                       llvm::cl::desc("Use new mangling for compiler tests"));

#ifndef USE_NEW_MANGLING

static bool containsNonSwiftModule(Demangle::NodePointer Nd) {
  switch (Nd->getKind()) {
    case Demangle::Node::Kind::Module:
      if (Nd->getText() != "Swift")
        return true;
      break;
    case Demangle::Node::Kind::ReabstractionThunk:
    case Demangle::Node::Kind::ReabstractionThunkHelper:
      return true;
    default:
      break;
  }

  for (auto Child : *Nd) {
    if (containsNonSwiftModule(Child))
      return true;
  }
  return false;
}

#endif // USE_NEW_MANGLING

bool swift::useNewMangling(Demangle::NodePointer Node) {
#ifdef USE_NEW_MANGLING
  return true;
#else
  if (NewManglingForTests && Node && containsNonSwiftModule(Node))
    return true;
  return false;
#endif
}

#ifndef NDEBUG

llvm::cl::opt<bool> PrintSwiftManglingStats(
    "print-swift-mangling-stats", llvm::cl::init(false),
    llvm::cl::desc("Print statistics about Swift symbol mangling"));

namespace {

#ifdef CHECK_MANGLING_AGAINST_OLD

static bool areTreesEqual(Demangle::NodePointer Old, Demangle::NodePointer New) {
  if ((Old != nullptr) != (New != nullptr))
    return false;
  if (!Old)
    return true;

  if (Old->getKind() == Demangle::Node::Kind::CurryThunk)
    Old = Old->getFirstChild();
  if (New->getKind() == Demangle::Node::Kind::CurryThunk)
    New = New->getFirstChild();

  if (Old->getKind() != New->getKind()) {
    if (Old->getKind() != Demangle::Node::Kind::UncurriedFunctionType ||
        New->getKind() != Demangle::Node::Kind::FunctionType)
      return false;
  }
  if (Old->hasText() != New->hasText())
    return false;
  if (Old->hasIndex() != New->hasIndex())
    return false;
  if (Old->hasText() && Old->getText() != New->getText())
    return false;
  if (Old->hasIndex() && Old->getIndex() != New->getIndex())
    return false;

  size_t OldNum = Old->getNumChildren();
  size_t NewNum = New->getNumChildren();

  if (OldNum >= 1 && NewNum == 1 &&
      Old->getChild(OldNum - 1)->getKind() == Demangle::Node::Kind::Suffix) {
    switch (New->getFirstChild()->getKind()) {
      case Demangle::Node::Kind::ReflectionMetadataBuiltinDescriptor:
      case Demangle::Node::Kind::ReflectionMetadataFieldDescriptor:
      case Demangle::Node::Kind::ReflectionMetadataAssocTypeDescriptor:
      case Demangle::Node::Kind::ReflectionMetadataSuperclassDescriptor:
      case Demangle::Node::Kind::PartialApplyForwarder:
        return true;
      default:
        return false;
    }
  }

  if (Old->getKind() == Demangle::Node::Kind::DependentAssociatedTypeRef &&
      OldNum + NewNum == 1) {
    OldNum = 0;
    NewNum = 0;
  }
  if (Old->getKind() == Demangle::Node::Kind::GenericSpecializationParam &&
      OldNum > 1 && NewNum == 1)
    OldNum = 1;

  if (OldNum != NewNum) {
    return false;
  }
  for (unsigned Idx = 0, End = OldNum; Idx < End; ++Idx) {
    if (!areTreesEqual(Old->getChild(Idx), New->getChild(Idx)))
      return false;
  }
  return true;
}

static bool treeContains(Demangle::NodePointer Nd, Demangle::Node::Kind Kind) {
  if (Nd->getKind() == Kind)
    return true;

  for (auto Child : *Nd) {
    if (treeContains(Child, Kind))
      return true;
  }
  return false;
}

#endif // CHECK_MANGLING_AGAINST_OLD

struct SizeStatEntry {
  int sizeDiff;
  std::string Old;
  std::string New;
};

static std::vector<SizeStatEntry> SizeStats;

static int numSmaller = 0;
static int numEqual = 0;
static int numLarger = 0;
static int totalOldSize = 0;
static int totalNewSize = 0;
static int mergedSubsts = 0;

struct OpStatEntry {
  OpStatEntry() : num(0), size(0) { }
  
  int num;
  int size;
};

static llvm::StringMap<OpStatEntry> OpStats;

} // end anonymous namespace

void Mangler::recordOpStatImpl(StringRef op, size_t OldPos) {
  if (PrintSwiftManglingStats) {
    OpStatEntry &E = OpStats[op];
    E.num++;
    E.size += Storage.size() - OldPos;
  }
}

#endif // NDEBUG

std::string NewMangling::selectMangling(const std::string &Old,
                                        const std::string &New,
                                        bool compareTrees) {
  using namespace Demangle;

  NodePointer NewNode = demangleSymbolAsNode(New);

  if (!NewNode && StringRef(New).startswith("s:")) {
    std::string demangleStr = MANGLING_PREFIX_STR + New.substr(2);
    NewNode = demangleSymbolAsNode(demangleStr);
  }

#ifndef NDEBUG
#ifdef CHECK_MANGLING_AGAINST_OLD

  static int numCmp = 0;

  NodePointer OldNode;
  if (compareTrees)
    OldNode = demangleSymbolAsNode(Old);

  if (StringRef(New).startswith(MANGLING_PREFIX_STR) &&
      (!NewNode || treeContains(NewNode, Demangle::Node::Kind::Suffix))) {
    llvm::errs() << "Can't demangle " << New << '\n';
    assert(false);
  }

  if (OldNode && !treeContains(OldNode, Demangle::Node::Kind::Suffix)) {
    if (!areTreesEqual(OldNode, NewNode)) {
      llvm::errs() << "Mangling differs at #" << numCmp << ":\n"
                      "old: " << Old << "\n"
                      "new: " << New << "\n\n"
                      "### old tree: ###\n";
      demangle_wrappers::NodeDumper(OldNode).print(llvm::errs());
      llvm::errs() << "\n### new tree: ###\n";
      demangle_wrappers::NodeDumper(NewNode).print(llvm::errs());
      llvm::errs() << '\n';
      assert(false);
    }
    if (StringRef(New).startswith(MANGLING_PREFIX_STR)) {
      std::string Remangled = mangleNodeNew(NewNode);
      if (New != Remangled) {
        bool isEqual = false;
        if (treeContains(NewNode,
                         Demangle::Node::Kind::DependentAssociatedTypeRef) ||
            // Does the mangling contain an identifier which is the name of
            // an old-mangled function?
            New.find("_T", 2) != std::string::npos) {
          NodePointer RemangledNode = demangleSymbolAsNode(Remangled);
          isEqual = areTreesEqual(NewNode, RemangledNode);
        }
        
// TODO: Check disabled until rdar://problem/30592808 is fixed. 
#if 0
        if (!isEqual) {
          llvm::errs() << "Remangling failed at #" << numCmp << ":\n"
                          "original:  " << New << "\n"
                          "remangled: " << Remangled << "\n";
          assert(false);
        }
#endif
      }
    }
  }
  numCmp++;
#endif // CHECK_MANGLING_AGAINST_OLD

  if (PrintSwiftManglingStats) {
    int OldSize = (int)Old.size();
    int NewSize = (int)New.size();
    if (NewSize > OldSize) {
      numLarger++;
      SizeStats.push_back({NewSize - OldSize, Old, New});
    } else if (OldSize > NewSize) {
      numSmaller++;
    } else {
      numEqual++;
    }
    totalOldSize += OldSize;
    totalNewSize += NewSize;
  }
#endif // NDEBUG

  return useNewMangling(NewNode) ? New : Old;
}

void NewMangling::printManglingStats() {
#ifndef NDEBUG
  if (!PrintSwiftManglingStats)
    return;

  std::sort(SizeStats.begin(), SizeStats.end(),
    [](const SizeStatEntry &LHS, const SizeStatEntry &RHS) {
      return LHS.sizeDiff < RHS.sizeDiff;
    });

  llvm::outs() << "Mangling size stats:\n"
                  "  num smaller: " << numSmaller << "\n"
                  "  num larger:  " << numLarger << "\n"
                  "  num equal:   " << numEqual << "\n"
                  "  total old size: " << totalOldSize << "\n"
                  "  total new size: " << totalNewSize << "\n"
                  "  new - old size: " << (totalNewSize - totalOldSize) << "\n"
                  "List or larger:\n";
  for (const SizeStatEntry &E : SizeStats) {
    llvm::outs() << "  delta " << E.sizeDiff << ": " << E.Old << " - " << E.New
                 << '\n';
  }
  
  llvm::outs() << "Mangling operator stats:\n";
  
  typedef llvm::StringMapEntry<OpStatEntry> MapEntry;
  std::vector<const MapEntry *> SortedOpStats;
  for (const MapEntry &ME : OpStats) {
    SortedOpStats.push_back(&ME);
  }
  std::sort(SortedOpStats.begin(), SortedOpStats.end(),
    [](const MapEntry *LHS, const MapEntry *RHS) {
      return LHS->getKey() < RHS->getKey();
    });

  for (const MapEntry *E : SortedOpStats) {
    llvm::outs() << "  " << E->getKey() << ": num = " << E->getValue().num
                 << ", size = " << E->getValue().size << '\n';
  }
  llvm::outs() << "  merged substitutions: " << mergedSubsts << '\n';
#endif
}

void Mangler::beginMangling() {
  Storage.clear();
  Substitutions.clear();
  StringSubstitutions.clear();
  lastSubstIdx = -2;
  Words.clear();
  Buffer << MANGLING_PREFIX_STR;
}

/// Finish the mangling of the symbol and return the mangled name.
std::string Mangler::finalize() {
  assert(Storage.size() && "Mangling an empty name");
  std::string result = std::string(Storage.data(), Storage.size());
  Storage.clear();
  return result;
}

/// Finish the mangling of the symbol and write the mangled name into
/// \p stream.
void Mangler::finalize(llvm::raw_ostream &stream) {
  std::string result = finalize();
  stream.write(result.data(), result.size());
}

void Mangler::appendIdentifier(StringRef ident) {
  auto Iter = StringSubstitutions.find(ident);
  if (Iter != StringSubstitutions.end())
    return mangleSubstitution(Iter->second);

  size_t OldPos = Storage.size();
  addSubstitution(ident);

  mangleIdentifier(*this, ident);

  recordOpStat("<identifier>", OldPos);
}

bool Mangler::tryMangleSubstitution(const void *ptr) {
  auto ir = Substitutions.find(ptr);
  if (ir == Substitutions.end())
    return false;

  mangleSubstitution(ir->second);
  return true;
}

void Mangler::mangleSubstitution(unsigned Idx) {
  if (Idx >= 26)
    return appendOperator("A", Index(Idx - 26));

  char c = Idx + 'A';
  if (lastSubstIdx == (int)Storage.size() - 1) {
    assert(isUpperLetter(Storage[lastSubstIdx]));
    Storage[lastSubstIdx] = Storage[lastSubstIdx] - 'A' + 'a';
    Buffer << c;
#ifndef NDEBUG
    mergedSubsts++;
#endif
  } else {
    appendOperator("A", StringRef(&c, 1));
  }
  lastSubstIdx = (int)Storage.size() - 1;
}

