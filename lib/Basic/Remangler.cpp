//===--- Remangler.cpp - Swift re-mangling from a demangling tree ---------===//
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
//  This file implements the remangler, which turns a demangling parse
//  tree back into a mangled string.  This is useful for tools which
//  want to extract subtrees from mangled strings.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/Demangler.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/Punycode.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/UUID.h"
#include "swift/Basic/ManglingUtils.h"
#include "swift/Basic/ManglingMacros.h"
#include "swift/Strings.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/Compiler.h"
#include <vector>
#include <cstdio>
#include <cstdlib>
#include <unordered_map>

using namespace swift;
using namespace Demangle;
using namespace NewMangling;

[[noreturn]]
static void unreachable(const char *Message) {
  fprintf(stderr, "fatal error: %s\n", Message);
  std::abort();
}

namespace {

class SubstitutionEntry {
  Node *TheNode = nullptr;
  size_t StoredHash = 0;
  bool treatAsIdentifier = false;

public:
  void setNode(Node *node, bool treatAsIdentifier) {
    this->treatAsIdentifier = treatAsIdentifier;
    TheNode = node;
    deepHash(node);
  }

  struct Hasher {
    size_t operator()(const SubstitutionEntry &entry) const {
      return entry.StoredHash;
    }
  };

private:
  friend bool operator==(const SubstitutionEntry &lhs,
                         const SubstitutionEntry &rhs) {
    if (lhs.StoredHash != rhs.StoredHash)
      return false;
    if (lhs.treatAsIdentifier != rhs.treatAsIdentifier)
      return false;
    if (lhs.treatAsIdentifier)
      return lhs.TheNode->getText() == rhs.TheNode->getText();
    return lhs.deepEquals(lhs.TheNode, rhs.TheNode);
  }

  void combineHash(size_t newValue) {
    StoredHash = 33 * StoredHash + newValue;
  }

  void combineHash(StringRef Text) {
    for (char c : Text) {
      combineHash((unsigned char) c);
    }
  }

  void deepHash(Node *node) {
    if (treatAsIdentifier) {
      combineHash((size_t) Node::Kind::Identifier);
      combineHash(node->getText());
      return;
    }
    combineHash((size_t) node->getKind());
    if (node->hasIndex()) {
      combineHash(node->getIndex());
    } else if (node->hasText()) {
      combineHash(node->getText());
    }
    for (Node *child : *node) {
      deepHash(child);
    }
  }

  bool deepEquals(Node *lhs, Node *rhs) const;
};

bool SubstitutionEntry::deepEquals(Node *lhs, Node *rhs) const {
  if (lhs->getKind() != rhs->getKind())
    return false;
  if (lhs->hasIndex()) {
    if (!rhs->hasIndex())
      return false;
    if (lhs->getIndex() != rhs->getIndex())
      return false;
  } else if (lhs->hasText()) {
    if (!rhs->hasText())
      return false;
    if (lhs->getText() != rhs->getText())
      return false;
  } else if (rhs->hasIndex() || rhs->hasText()) {
    return false;
  }

  if (lhs->getNumChildren() != rhs->getNumChildren())
    return false;

  for (auto li = lhs->begin(), ri = rhs->begin(), le = lhs->end();
       li != le; ++li, ++ri) {
    if (!deepEquals(*li, *ri))
      return false;
  }
  
  return true;
}

class Remangler {
  template <typename Mangler>
  friend void NewMangling::mangleIdentifier(Mangler &M, StringRef ident);

  const bool UsePunycode = true;

  DemanglerPrinter &Buffer;

  std::vector<SubstitutionWord> Words;
  std::vector<WordReplacement> SubstWordsInIdent;

  static const size_t MaxNumWords = 26;

  std::unordered_map<SubstitutionEntry, unsigned,
                     SubstitutionEntry::Hasher> Substitutions;

  int lastSubstIdx = -2;

  // We have to cons up temporary nodes sometimes when remangling
  // nested generics. This factory owns them.
  NodeFactory Factory;

  StringRef getBufferStr() const { return Buffer.getStringRef(); }

  template <typename Mangler>
  friend void mangleIdentifier(Mangler &M, StringRef ident);

  class EntityContext {
    bool AsContext = false;
  public:
    bool isAsContext() const {
      return AsContext;
    }

    class ManglingContextRAII {
      EntityContext &Ctx;
      bool SavedValue;
    public:
      ManglingContextRAII(EntityContext &ctx)
        : Ctx(ctx), SavedValue(ctx.AsContext) {
        ctx.AsContext = true;
      }

      ~ManglingContextRAII() {
        Ctx.AsContext = SavedValue;
      }
    };
  };

  Node *getSingleChild(Node *node) {
    assert(node->getNumChildren() == 1);
    return node->getFirstChild();
  }

  Node *getSingleChild(Node *node, Node::Kind kind) {
    Node *Child = getSingleChild(node);
    assert(Child->getKind() == kind);
    return Child;
  }

  Node *skipType(Node *node) {
    if (node->getKind() == Node::Kind::Type)
      return getSingleChild(node);
    return node;
  }

  Node *getChildOfType(Node *node) {
    assert(node->getKind() == Node::Kind::Type);
    return getSingleChild(node);
  }

  void mangleIndex(Node::IndexType value) {
    if (value == 0) {
      Buffer << '_';
    } else {
      Buffer << (value - 1) << '_';
    }
  }

  void mangleChildNodes(Node *node) {
    mangleNodes(node->begin(), node->end());
  }
  void mangleChildNodesReversed(Node *node) {
    for (size_t Idx = 0, Num = node->getNumChildren(); Idx < Num; ++Idx) {
      mangleChildNode(node, Num - Idx - 1);
    }
  }

  void mangleListSeparator(bool &isFirstListItem) {
    if (isFirstListItem) {
      Buffer << '_';
      isFirstListItem = false;
    }
  }

  void mangleEndOfList(bool isFirstListItem) {
    if (isFirstListItem)
      Buffer << 'y';
  }

  void mangleNodes(Node::iterator i, Node::iterator e) {
    for (; i != e; ++i) {
      mangle(*i);
    }
  }

  void mangleSingleChildNode(Node *node) {
    assert(node->getNumChildren() == 1);
    mangle(*node->begin());
  }

  void mangleChildNode(Node *node, unsigned index) {
    assert(index < node->getNumChildren());
    mangle(node->begin()[index]);
  }

  void manglePureProtocol(Node *Proto) {
    Proto = skipType(Proto);
    mangleChildNodes(Proto);
  }

  bool trySubstitution(Node *node, SubstitutionEntry &entry,
                       bool treatAsIdentifier = false);
  void addSubstitution(const SubstitutionEntry &entry);

  void mangleIdentifierImpl(Node *node, bool isOperator);

  void mangleDependentGenericParamIndex(Node *node,
                                        const char *nonZeroPrefix = "",
                                        char zeroOp = 'z');

  std::pair<int, Node *> mangleConstrainedType(Node *node);

  void mangleFunctionSignature(Node *FuncType) {
    mangleChildNodesReversed(FuncType);
  }

  void mangleAnyNominalType(Node *node);
  void mangleNominalType(Node *node, char TypeOp);
  void mangleGenericArgs(Node *node, char &Separator);

#define NODE(ID)                                                        \
  void mangle##ID(Node *node);
#define CONTEXT_NODE(ID)                                                \
  void mangle##ID(Node *node);                                        \
//    void mangle##ID(Node *node, EntityContext &ctx);
#include "swift/Basic/DemangleNodes.def"

public:
  Remangler(DemanglerPrinter &Buffer) : Buffer(Buffer) {}

  void mangle(Node *node) {
    switch (node->getKind()) {
#define NODE(ID) case Node::Kind::ID: return mangle##ID(node);
#include "swift/Basic/DemangleNodes.def"
    }
    unreachable("bad demangling tree node");
  }
};

bool Remangler::trySubstitution(Node *node, SubstitutionEntry &entry,
                                bool treatAsIdentifier) {
  if (mangleStandardSubstitution(node, Buffer))
    return true;

  // Go ahead and initialize the substitution entry.
  entry.setNode(node, treatAsIdentifier);

  auto it = Substitutions.find(entry);
  if (it == Substitutions.end())
    return false;

  unsigned Idx = it->second;
  if (Idx >= 26) {
    Buffer << 'A';
    mangleIndex(Idx - 26);
    return true;
  }
  char c = Idx + 'A';
  if (lastSubstIdx == (int)Buffer.getStringRef().size() - 1) {
    char &lastChar = Buffer.lastChar();
    assert(isUpperLetter(lastChar));
    lastChar = lastChar - 'A' + 'a';
    Buffer << c;
  } else {
    Buffer << 'A' << c;
  }
  lastSubstIdx = Buffer.getStringRef().size() - 1;
  return true;
}

void Remangler::addSubstitution(const SubstitutionEntry &entry) {
  unsigned Idx = Substitutions.size();
#if false
  llvm::outs() << "add subst ";
  if (Idx < 26) {
    llvm::outs() << char('A' + Idx);
  } else {
    llvm::outs() << Idx;
  }
  llvm::outs() << " at pos " << getBufferStr().size() << '\n';
#endif
  auto result = Substitutions.insert({entry, Idx});
  assert(result.second);
  (void) result;
}

void Remangler::mangleIdentifierImpl(Node *node, bool isOperator) {
  SubstitutionEntry entry;
  if (trySubstitution(node, entry, /*treatAsIdentifier*/ true)) return;
  if (isOperator) {
    NewMangling::mangleIdentifier(*this,
                              NewMangling::translateOperator(node->getText()));
  } else {
    NewMangling::mangleIdentifier(*this, node->getText());
  }
  addSubstitution(entry);
}


void Remangler::mangleDependentGenericParamIndex(Node *node,
                                                    const char *nonZeroPrefix,
                                                    char zeroOp) {
  auto depth = node->getChild(0)->getIndex();
  auto index = node->getChild(1)->getIndex();

  if (depth != 0) {
    Buffer << nonZeroPrefix << 'd';
    mangleIndex(depth - 1);
    mangleIndex(index);
    return;
  }
  if (index != 0) {
    Buffer << nonZeroPrefix;
    mangleIndex(index - 1);
    return;
  }
  // depth == index == 0
  Buffer << zeroOp;
}

std::pair<int, Node *> Remangler::mangleConstrainedType(Node *node) {
  if (node->getKind() == Node::Kind::Type)
    node = getChildOfType(node);

  SubstitutionEntry entry;
  if (trySubstitution(node, entry))
    return {-1, nullptr};

  std::vector<Node *> Chain;
  while (node->getKind() == Node::Kind::DependentMemberType) {
    Chain.push_back(node->getChild(1));
    node = getChildOfType(node->getFirstChild());
  }
  assert(node->getKind() == Node::Kind::DependentGenericParamType);

  const char *ListSeparator = (Chain.size() > 1 ? "_" : "");
  for (Node *DepAssocTyRef : reversed(Chain)) {
    mangle(DepAssocTyRef);
    Buffer << ListSeparator;
    ListSeparator = "";
  }
  if (Chain.size() > 0)
    addSubstitution(entry);
  return {(int)Chain.size(), node};
}

void Remangler::mangleNominalType(Node *node, char TypeOp) {
  SubstitutionEntry entry;
  if (trySubstitution(node, entry)) return;
  mangleChildNodes(node);
  Buffer << TypeOp;
  addSubstitution(entry);
}

void Remangler::mangleAnyNominalType(Node *node) {
  if (isSpecialized(node)) {
    NodePointer unboundType = getUnspecialized(node, Factory);
    mangleAnyNominalType(unboundType);
    char Separator = 'y';
    mangleGenericArgs(node, Separator);
    Buffer << 'G';
    return;
  }
  switch (node->getKind()) {
    case Node::Kind::Structure: return mangleNominalType(node, 'V');
    case Node::Kind::Enum: return mangleNominalType(node, 'O');
    case Node::Kind::Class: return mangleNominalType(node, 'C');
    default:
      unreachable("bad nominal type kind");
  }
}

void Remangler::mangleGenericArgs(Node *node, char &Separator) {
  switch (node->getKind()) {
    case Node::Kind::Structure:
    case Node::Kind::Enum:
    case Node::Kind::Class: {
      NodePointer parentOrModule = node->getChild(0);
      mangleGenericArgs(parentOrModule, Separator);
      Buffer << Separator;
      Separator = '_';
      break;
    }

    case Node::Kind::BoundGenericStructure:
    case Node::Kind::BoundGenericEnum:
    case Node::Kind::BoundGenericClass: {
      NodePointer unboundType = node->getChild(0);
      assert(unboundType->getKind() == Node::Kind::Type);
      NodePointer nominalType = unboundType->getChild(0);
      NodePointer parentOrModule = nominalType->getChild(0);
      mangleGenericArgs(parentOrModule, Separator);
      Buffer << Separator;
      Separator = '_';
      mangleChildNodes(node->getChild(1));
      break;
    }
      
    default:
      break;
  }
}

void Remangler::mangleAllocator(Node *node) {
  mangleChildNodes(node);
  Buffer << "fC";
}

void Remangler::mangleArgumentTuple(Node *node) {
  Node *Child = skipType(getSingleChild(node));
  if (Child->getKind() == Node::Kind::NonVariadicTuple &&
      Child->getNumChildren() == 0) {
    Buffer << 'y';
    return;
  }
  mangle(Child);
}

void Remangler::mangleAssociatedType(Node *node) {
  unreachable("unsupported node");
}

void Remangler::mangleAssociatedTypeRef(Node *node) {
  SubstitutionEntry entry;
  if (trySubstitution(node, entry)) return;
  mangleChildNodes(node);
  Buffer << "Qa";
  addSubstitution(entry);
}

void Remangler::mangleAssociatedTypeMetadataAccessor(Node *node) {
  mangleChildNodes(node); // protocol conformance, identifier
  Buffer << "Wt";
}

void Remangler::mangleAssociatedTypeWitnessTableAccessor(Node *node) {
  mangleChildNodes(node); // protocol conformance, identifier, type
  Buffer << "WT";
}

void Remangler::mangleAutoClosureType(Node *node) {
  mangleChildNodesReversed(node); // argument tuple, result type
  Buffer << "XK";
}

void Remangler::mangleBoundGenericClass(Node *node) {
  mangleAnyNominalType(node);
}

void Remangler::mangleBoundGenericEnum(Node *node) {
  Node *Enum = node->getChild(0)->getChild(0);
  assert(Enum->getKind() == Node::Kind::Enum);
  Node *Mod = Enum->getChild(0);
  Node *Id = Enum->getChild(1);
  if (Mod->getKind() == Node::Kind::Module && Mod->getText() == STDLIB_NAME &&
      Id->getKind() == Node::Kind::Identifier && Id->getText() == "Optional") {
    mangleSingleChildNode(node->getChild(1));
    Buffer << "Sg";
    return;
  }
  mangleAnyNominalType(node);
}

void Remangler::mangleBoundGenericStructure(Node *node) {
  mangleAnyNominalType(node);
}

template <size_t N>
static bool stripPrefix(StringRef &string, const char (&data)[N]) {
  constexpr size_t prefixLength = N - 1;
  if (!string.startswith(StringRef(data, prefixLength)))
    return false;
  string = string.drop_front(prefixLength);
  return true;
}

void Remangler::mangleBuiltinTypeName(Node *node) {
  Buffer << 'B';
  StringRef text = node->getText();

  if (text == "Builtin.BridgeObject") {
    Buffer << 'b';
  } else if (text == "Builtin.UnsafeValueBuffer") {
    Buffer << 'B';
  } else if (text == "Builtin.UnknownObject") {
    Buffer << 'O';
  } else if (text == "Builtin.NativeObject") {
    Buffer << 'o';
  } else if (text == "Builtin.RawPointer") {
    Buffer << 'p';
  } else if (text == "Builtin.Word") {
    Buffer << 'w';
  } else if (stripPrefix(text, "Builtin.Int")) {
    Buffer << 'i' << text << '_';
  } else if (stripPrefix(text, "Builtin.Float")) {
    Buffer << 'f' << text << '_';
  } else if (stripPrefix(text, "Builtin.Vec")) {
    auto split = text.split('x');
    if (split.second == "RawPointer") {
      Buffer << 'p';
    } else if (stripPrefix(split.second, "Float")) {
      Buffer << 'f' << split.second << '_';
    } else if (stripPrefix(split.second, "Int")) {
      Buffer << 'i' << split.second << '_';
    } else {
      unreachable("unexpected builtin vector type");
    }
    Buffer << "Bv" << split.first << '_';
  } else {
    unreachable("unexpected builtin type");
  }
}

void Remangler::mangleCFunctionPointer(Node *node) {
  mangleChildNodesReversed(node); // argument tuple, result type
  Buffer << "XC";
}

void Remangler::mangleClass(Node *node) {
  mangleAnyNominalType(node);
}

void Remangler::mangleConstructor(Node *node) {
  mangleChildNodes(node);
  Buffer << "fc";
}

void Remangler::mangleDeallocator(Node *node) {
  mangleChildNodes(node);
  Buffer << "fD";
}

void Remangler::mangleDeclContext(Node *node) {
  mangleSingleChildNode(node);
}

void Remangler::mangleDefaultArgumentInitializer(Node *node) {
  mangleChildNode(node, 0);
  Buffer << "fA";
  mangleChildNode(node, 1);
}

void Remangler::mangleDependentAssociatedTypeRef(Node *node) {
  mangleIdentifier(node);
  if (node->getNumChildren() != 0)
    mangleSingleChildNode(node);
}

void Remangler::mangleDependentGenericConformanceRequirement(Node *node) {
  Node *ProtoOrClass = node->getChild(1);
  if (ProtoOrClass->getFirstChild()->getKind() == Node::Kind::Protocol) {
    manglePureProtocol(ProtoOrClass);
    auto NumMembersAndParamIdx = mangleConstrainedType(node->getChild(0));
    switch (NumMembersAndParamIdx.first) {
      case -1: Buffer << "RQ"; return; // substitution
      case 0: Buffer << "R"; break;
      case 1: Buffer << "Rp"; break;
      default: Buffer << "RP"; break;
    }
    mangleDependentGenericParamIndex(NumMembersAndParamIdx.second);
    return;
  }
  mangle(ProtoOrClass);
  auto NumMembersAndParamIdx = mangleConstrainedType(node->getChild(0));
  switch (NumMembersAndParamIdx.first) {
    case -1: Buffer << "RB"; return; // substitution
    case 0: Buffer << "Rb"; break;
    case 1: Buffer << "Rc"; break;
    default: Buffer << "RC"; break;
  }
  mangleDependentGenericParamIndex(NumMembersAndParamIdx.second);
  return;
}

void Remangler::mangleDependentGenericParamCount(Node *node) {
  unreachable("handled inline in DependentGenericSignature");
}

void Remangler::mangleDependentGenericParamType(Node *node) {
  if (node->getChild(0)->getIndex() == 0
      && node->getChild(1)->getIndex() == 0) {
    Buffer << 'x';
    return;
  }
  Buffer << 'q';
  mangleDependentGenericParamIndex(node);
}

void Remangler::mangleDependentGenericSameTypeRequirement(Node *node) {
  mangleChildNode(node, 1);
  auto NumMembersAndParamIdx = mangleConstrainedType(node->getChild(0));
  switch (NumMembersAndParamIdx.first) {
    case -1: Buffer << "RS"; return; // substitution
    case 0: Buffer << "Rs"; break;
    case 1: Buffer << "Rt"; break;
    default: Buffer << "RT"; break;
  }
  mangleDependentGenericParamIndex(NumMembersAndParamIdx.second);
}

void Remangler::mangleDependentGenericLayoutRequirement(Node *node) {
  auto NumMembersAndParamIdx = mangleConstrainedType(node->getChild(0));
  switch (NumMembersAndParamIdx.first) {
    case -1: Buffer << "RL"; return; // substitution
    case 0: Buffer << "Rl"; break;
    case 1: Buffer << "Rm"; break;
    default: Buffer << "RM"; break;
  }
  mangleDependentGenericParamIndex(NumMembersAndParamIdx.second);
  assert(node->getChild(1)->getKind() == Node::Kind::Identifier);
  assert(node->getChild(1)->getText().size() == 1);
  Buffer << node->getChild(1)->getText()[0];
  if (node->getNumChildren() >=3)
    mangleChildNode(node, 2);
  if (node->getNumChildren() >=4)
    mangleChildNode(node, 3);
}

void Remangler::mangleDependentGenericSignature(Node *node) {
  size_t ParamCountEnd = 0;
  for (size_t Idx = 0, Num = node->getNumChildren(); Idx < Num; Idx++) {
    Node *Child = node->getChild(Idx);
    if (Child->getKind() == Node::Kind::DependentGenericParamCount) {
      ParamCountEnd = Idx + 1;
    } else {
      // requirement
      mangleChildNode(node, Idx);
    }
  }
  // If there's only one generic param, mangle nothing.
  if (ParamCountEnd == 1 && node->getChild(0)->getIndex() == 1) {
    Buffer << 'l';
    return;
  }

  // Remangle generic params.
  Buffer << 'r';
  for (size_t Idx = 0; Idx < ParamCountEnd; ++Idx) {
    Node *Count = node->getChild(Idx);
    if (Count->getIndex() > 0) {
      mangleIndex(Count->getIndex() - 1);
    } else {
      Buffer << 'z';
    }
  }
  Buffer << 'l';
}

void Remangler::mangleDependentGenericType(Node *node) {
  mangleChildNodesReversed(node); // type, generic signature
  Buffer << 'u';
}

void Remangler::mangleDependentMemberType(Node *node) {
  auto NumMembersAndParamIdx = mangleConstrainedType(node);
  switch (NumMembersAndParamIdx.first) {
    case -1:
      break; // substitution
    case 0:
      unreachable("wrong dependent member type");
    case 1:
      Buffer << 'Q';
      mangleDependentGenericParamIndex(NumMembersAndParamIdx.second, "y", 'z');
      break;
    default:
      Buffer << 'Q';
      mangleDependentGenericParamIndex(NumMembersAndParamIdx.second, "Y", 'Z');
      break;
  }
}

void Remangler::mangleDependentPseudogenericSignature(Node *node) {
  unreachable("handled inline");
}

void Remangler::mangleDestructor(Node *node) {
  mangleChildNodes(node);
  Buffer << "fd";
}

void Remangler::mangleDidSet(Node *node) {
  mangleChildNodes(node);
  Buffer << "fW";
}

void Remangler::mangleDirectness(Node *node) {
  if (node->getIndex() == unsigned(Directness::Direct)) {
    Buffer << 'd';
  } else {
    assert(node->getIndex() == unsigned(Directness::Indirect));
    Buffer << 'i';
  }
}

void Remangler::mangleDynamicAttribute(Node *node) {
  Buffer << "TD";
}

void Remangler::mangleDirectMethodReferenceAttribute(Node *node) {
  Buffer << "Td";
}

void Remangler::mangleDynamicSelf(Node *node) {
  mangleSingleChildNode(node); // type
  Buffer << "XD";
}

void Remangler::mangleEnum(Node *node) {
  mangleAnyNominalType(node);
}

void Remangler::mangleErrorType(Node *node) {
  Buffer << "Xe";
}

void Remangler::mangleExistentialMetatype(Node *node) {
  if (node->getFirstChild()->getKind() == Node::Kind::MetatypeRepresentation) {
    mangleChildNode(node, 1);
    Buffer << "Xm";
    mangleChildNode(node, 0);
  } else {
    mangleSingleChildNode(node);
    Buffer << "Xp";
  }
}

void Remangler::mangleExplicitClosure(Node *node) {
  mangleChildNode(node, 0); // context
  mangleChildNode(node, 2); // type
  Buffer << "fU";
  mangleChildNode(node, 1); // index
}

void Remangler::mangleExtension(Node *node) {
  mangleChildNode(node, 1);
  mangleChildNode(node, 0);
  if (node->getNumChildren() == 3)
    mangleChildNode(node, 2); // generic signature
  Buffer << 'E';
}

void Remangler::mangleFieldOffset(Node *node) {
  mangleChildNode(node, 1); // variable
  Buffer << "Wv";
  mangleChildNode(node, 0); // directness
}

void Remangler::mangleFullTypeMetadata(Node *node) {
  mangleSingleChildNode(node);
  Buffer << "Mf";
}

void Remangler::mangleFunction(Node *node) {
  mangleChildNode(node, 0); // context
  mangleChildNode(node, 1); // name
  Node *FuncType = getSingleChild(node->getChild(2));
  if (FuncType->getKind() == Node::Kind::DependentGenericType) {
    mangleFunctionSignature(getSingleChild(FuncType->getChild(1)));
    mangleChildNode(FuncType, 0); // generic signature
  } else {
    mangleFunctionSignature(FuncType);
  }
  Buffer << "F";
}

void Remangler::mangleFunctionSignatureSpecialization(Node *node) {
  for (NodePointer Param : *node) {
    if (Param->getKind() == Node::Kind::FunctionSignatureSpecializationParam &&
        Param->getNumChildren() > 0) {
      Node *KindNd = Param->getChild(0);
      switch (FunctionSigSpecializationParamKind(KindNd->getIndex())) {
        case FunctionSigSpecializationParamKind::ConstantPropFunction:
        case FunctionSigSpecializationParamKind::ConstantPropGlobal:
          mangleIdentifier(Param->getChild(1));
          break;
        case FunctionSigSpecializationParamKind::ConstantPropString: {
          NodePointer TextNd = Param->getChild(2);
          StringRef Text = TextNd->getText();
          if (Text.size() > 0 && (isDigit(Text[0]) || Text[0] == '_')) {
            std::string Buffer = "_";
            Buffer.append(Text.data(), Text.size());
            TextNd = Factory.createNode(Node::Kind::Identifier, Buffer);
          }
          mangleIdentifier(TextNd);
          break;
        }
        case FunctionSigSpecializationParamKind::ClosureProp:
          mangleIdentifier(Param->getChild(1));
          for (unsigned i = 2, e = Param->getNumChildren(); i != e; ++i) {
            mangleType(Param->getChild(i));
          }
          break;
        default:
          break;
      }
    }
  }
  Buffer << "Tf";
  bool returnValMangled = false;
  for (NodePointer Child : *node) {
    if (Child->getKind() == Node::Kind::FunctionSignatureSpecializationParam) {
      if (Child->getIndex() == Node::IndexType(~0)) {
        Buffer << '_';
        returnValMangled = true;
      }
    }
    mangle(Child);

    if (Child->getKind() == Node::Kind::SpecializationPassID &&
        node->hasIndex()) {
      Buffer << node->getIndex();
    }
  }
  if (!returnValMangled)
    Buffer << "_n";
}

void Remangler::mangleFunctionSignatureSpecializationParam(Node *node) {
  if (!node->hasChildren()) {
    Buffer << 'n';
    return;
  }

  // The first child is always a kind that specifies the type of param that we
  // have.
  Node *KindNd = node->getChild(0);
  unsigned kindValue = KindNd->getIndex();
  auto kind = FunctionSigSpecializationParamKind(kindValue);

  switch (kind) {
    case FunctionSigSpecializationParamKind::ConstantPropFunction:
      Buffer << "pf";
      return;
    case FunctionSigSpecializationParamKind::ConstantPropGlobal:
      Buffer << "pg";
      return;
    case FunctionSigSpecializationParamKind::ConstantPropInteger:
      Buffer << "pi" << node->getChild(1)->getText();
      return;
    case FunctionSigSpecializationParamKind::ConstantPropFloat:
      Buffer << "pd" << node->getChild(1)->getText();
      return;
    case FunctionSigSpecializationParamKind::ConstantPropString: {
      Buffer << "ps";
      StringRef encodingStr = node->getChild(1)->getText();
      if (encodingStr == "u8") {
        Buffer << 'b';
      } else if (encodingStr == "u16") {
        Buffer << 'w';
      } else if (encodingStr == "objc") {
        Buffer << 'c';
      } else {
        unreachable("Unknown encoding");
      }
      return;
    }
    case FunctionSigSpecializationParamKind::ClosureProp:
      Buffer << 'c';
      return;
    case FunctionSigSpecializationParamKind::BoxToValue:
      Buffer << 'i';
      return;
    case FunctionSigSpecializationParamKind::BoxToStack:
      Buffer << 's';
      return;
    case FunctionSigSpecializationParamKind::SROA:
      Buffer << 'x';
      return;
    default:
      if (kindValue & unsigned(FunctionSigSpecializationParamKind::Dead)) {
        Buffer << 'd';
        if (kindValue &
            unsigned(FunctionSigSpecializationParamKind::OwnedToGuaranteed))
          Buffer << 'G';
      } else if (kindValue &
              unsigned(FunctionSigSpecializationParamKind::OwnedToGuaranteed)) {
        Buffer << 'g';
      }
      if (kindValue & unsigned(FunctionSigSpecializationParamKind::SROA))
        Buffer << 'X';
      return;
  }
}

void Remangler::mangleFunctionSignatureSpecializationParamKind(Node *node) {
  unreachable("handled inline");
}

void Remangler::mangleFunctionSignatureSpecializationParamPayload(Node *node) {
  unreachable("handled inline");
}

void Remangler::mangleFunctionType(Node *node) {
  mangleFunctionSignature(node);
  Buffer << 'c';
}

void Remangler::mangleGenericProtocolWitnessTable(Node *node) {
  mangleSingleChildNode(node);
  Buffer << "WG";
}

void Remangler::mangleGenericProtocolWitnessTableInstantiationFunction(Node *node) {
  mangleSingleChildNode(node);
  Buffer << "WI";
}

void Remangler::mangleGenericPartialSpecialization(Node *node) {
  for (NodePointer Child : *node) {
    if (Child->getKind() == Node::Kind::GenericSpecializationParam) {
      mangleChildNode(Child, 0);
      break;
    }
  }
  Buffer << (node->getKind() ==
        Node::Kind::GenericPartialSpecializationNotReAbstracted ? "TP" : "Tp");
  for (NodePointer Child : *node) {
    if (Child->getKind() != Node::Kind::GenericSpecializationParam)
      mangle(Child);
  }
}

void Remangler::mangleGenericPartialSpecializationNotReAbstracted(Node *node) {
  mangleGenericPartialSpecialization(node);
}

void Remangler::mangleGenericSpecialization(Node *node) {
  bool FirstParam = true;
  for (NodePointer Child : *node) {
    if (Child->getKind() == Node::Kind::GenericSpecializationParam) {
      mangleChildNode(Child, 0);
      mangleListSeparator(FirstParam);
    }
  }
  assert(!FirstParam && "generic specialization with no substitutions");

  Buffer << (node->getKind() ==
               Node::Kind::GenericSpecializationNotReAbstracted ? "TG" : "Tg");
  for (NodePointer Child : *node) {
    if (Child->getKind() != Node::Kind::GenericSpecializationParam)
      mangle(Child);
  }
}

void Remangler::mangleGenericSpecializationNotReAbstracted(Node *node) {
  mangleGenericSpecialization(node);
}

void Remangler::mangleGenericSpecializationParam(Node *node) {
  unreachable("handled inline");
}

void Remangler::mangleGenericTypeMetadataPattern(Node *node) {
  mangleSingleChildNode(node);
  Buffer << "MP";
}

void Remangler::mangleGenericTypeParamDecl(Node *node) {
  mangleChildNodes(node);
  Buffer << "fp";
}

void Remangler::mangleGetter(Node *node) {
  mangleChildNodes(node);
  Buffer << "fg";
}

void Remangler::mangleGlobal(Node *node) {
  Buffer << MANGLING_PREFIX_STR;
  bool mangleInReverseOrder = false;
  for (auto Iter = node->begin(), End = node->end(); Iter != End; ++Iter) {
    Node *Child = *Iter;
    switch (Child->getKind()) {
      case Node::Kind::FunctionSignatureSpecialization:
      case Node::Kind::GenericSpecialization:
      case Node::Kind::GenericSpecializationNotReAbstracted:
      case Node::Kind::GenericPartialSpecialization:
      case Node::Kind::GenericPartialSpecializationNotReAbstracted:
      case Node::Kind::ObjCAttribute:
      case Node::Kind::NonObjCAttribute:
      case Node::Kind::DynamicAttribute:
      case Node::Kind::VTableAttribute:
      case Node::Kind::DirectMethodReferenceAttribute:
        mangleInReverseOrder = true;
        break;
      default:
        mangle(Child);
        if (mangleInReverseOrder) {
          auto ReverseIter = Iter;
          while (ReverseIter != node->begin()) {
            --ReverseIter;
            mangle(*ReverseIter);
          }
          mangleInReverseOrder = false;
        }
        break;
    }
  }
}

void Remangler::mangleGlobalGetter(Node *node) {
  mangleChildNodes(node);
  Buffer << "fG";
}

void Remangler::mangleIdentifier(Node *node) {
  mangleIdentifierImpl(node, /*isOperator*/ false);
}

void Remangler::mangleIndex(Node *node) {
  unreachable("handled inline");
}

void Remangler::mangleIVarInitializer(Node *node) {
  mangleSingleChildNode(node);
  Buffer << "fe";
}

void Remangler::mangleIVarDestroyer(Node *node) {
  mangleSingleChildNode(node);
  Buffer << "fE";
}

void Remangler::mangleImplConvention(Node *node) {
  char ConvCh = llvm::StringSwitch<char>(node->getText())
                  .Case("@callee_unowned", 'y')
                  .Case("@callee_guaranteed", 'g')
                  .Case("@callee_owned", 'x')
                  .Default(0);
  assert(ConvCh && "invalid impl callee convention");
  Buffer << ConvCh;
}

void Remangler::mangleImplFunctionAttribute(Node *node) {
  unreachable("handled inline");
}

void Remangler::mangleImplFunctionType(Node *node) {
  const char *PseudoGeneric = "";
  Node *GenSig = nullptr;
  for (NodePointer Child : *node) {
    switch (Child->getKind()) {
      case Node::Kind::ImplParameter:
      case Node::Kind::ImplResult:
      case Node::Kind::ImplErrorResult:
        mangleChildNode(Child, 1);
        break;
      case Node::Kind::DependentPseudogenericSignature:
        PseudoGeneric = "P";
        LLVM_FALLTHROUGH;
      case Node::Kind::DependentGenericSignature:
        GenSig = Child;
        break;
      default:
        break;
    }
  }
  if (GenSig)
    mangle(GenSig);

  Buffer << 'I' << PseudoGeneric;
  for (NodePointer Child : *node) {
    switch (Child->getKind()) {
      case Node::Kind::ImplConvention: {
        char ConvCh = llvm::StringSwitch<char>(Child->getText())
                        .Case("@callee_unowned", 'y')
                        .Case("@callee_guaranteed", 'g')
                        .Case("@callee_owned", 'x')
                        .Case("@convention(thin)", 't')
                        .Default(0);
        assert(ConvCh && "invalid impl callee convention");
        Buffer << ConvCh;
        break;
      }
      case Node::Kind::ImplFunctionAttribute: {
        char FuncAttr = llvm::StringSwitch<char>(Child->getText())
                        .Case("@convention(block)", 'B')
                        .Case("@convention(c)", 'C')
                        .Case("@convention(method)", 'M')
                        .Case("@convention(objc_method)", 'O')
                        .Case("@convention(closure)", 'K')
                        .Case("@convention(witness_method)", 'W')
                        .Default(0);
        assert(FuncAttr && "invalid impl function attribute");
        Buffer << FuncAttr;
        break;
      }
      case Node::Kind::ImplParameter: {
        char ConvCh = llvm::StringSwitch<char>(Child->getFirstChild()->getText())
                        .Case("@in", 'i')
                        .Case("@inout", 'l')
                        .Case("@inout_aliasable", 'b')
                        .Case("@in_guaranteed", 'n')
                        .Case("@owned", 'x')
                        .Case("@guaranteed", 'g')
                        .Case("@deallocating", 'e')
                        .Case("@unowned", 'y')
                        .Default(0);
        assert(ConvCh && "invalid impl parameter convention");
        Buffer << ConvCh;
        break;
      }
      case Node::Kind::ImplErrorResult:
        Buffer << 'z';
        LLVM_FALLTHROUGH;
      case Node::Kind::ImplResult: {
        char ConvCh = llvm::StringSwitch<char>(Child->getFirstChild()->getText())
                        .Case("@out", 'r')
                        .Case("@owned", 'o')
                        .Case("@unowned", 'd')
                        .Case("@unowned_inner_pointer", 'u')
                        .Case("@autoreleased", 'a')
                        .Default(0);
        assert(ConvCh && "invalid impl parameter convention");
        Buffer << ConvCh;
        break;
      }
      default:
        break;
    }
  }
  Buffer << '_';
}

void Remangler::mangleImplicitClosure(Node *node) {
  mangleChildNode(node, 0); // context
  mangleChildNode(node, 2); // type
  Buffer << "fu";
  mangleChildNode(node, 1); // index
}

void Remangler::mangleImplParameter(Node *node) {
  unreachable("handled inline");
}

void Remangler::mangleImplResult(Node *node) {
  unreachable("handled inline");
}

void Remangler::mangleImplErrorResult(Node *node) {
  unreachable("handled inline");
}

void Remangler::mangleInOut(Node *node) {
  mangleSingleChildNode(node);
  Buffer << 'z';
}

void Remangler::mangleInfixOperator(Node *node) {
  mangleIdentifierImpl(node, /*isOperator*/ true);
  Buffer << "oi";
}

void Remangler::mangleInitializer(Node *node) {
  mangleChildNodes(node);
  Buffer << "fi";
}

void Remangler::mangleLazyProtocolWitnessTableAccessor(Node *node) {
  mangleChildNodes(node);
  Buffer << "Wl";
}

void Remangler::mangleLazyProtocolWitnessTableCacheVariable(Node *node) {
  mangleChildNodes(node);
  Buffer << "WL";
}

void Remangler::mangleLocalDeclName(Node *node) {
  mangleChildNode(node, 1); // identifier
  Buffer << 'L';
  mangleChildNode(node, 0); // index
}

void Remangler::mangleMaterializeForSet(Node *node) {
  mangleChildNodes(node);
  Buffer << "fm";
}

void Remangler::mangleMetatype(Node *node) {
  if (node->getFirstChild()->getKind() == Node::Kind::MetatypeRepresentation) {
    mangleChildNode(node, 1);
    Buffer << "XM";
    mangleChildNode(node, 0);
  } else {
    mangleSingleChildNode(node);
    Buffer << 'm';
  }
}

void Remangler::mangleMetatypeRepresentation(Node *node) {
  if (node->getText() == "@thin") {
    Buffer << 't';
  } else if (node->getText() == "@thick") {
    Buffer << 'T';
  } else if (node->getText() == "@objc_metatype") {
    Buffer << 'o';
  } else {
    unreachable("wrong metatype representation");
  }
}

void Remangler::mangleMetaclass(Node *node) {
  mangleChildNodes(node);
  Buffer << "Mm";
}

void Remangler::mangleModule(Node *node) {
  mangleIdentifier(node);
}

void Remangler::mangleNativeOwningAddressor(Node *node) {
  mangleChildNodes(node);
  Buffer << "flo";
}

void Remangler::mangleNativeOwningMutableAddressor(Node *node) {
  mangleChildNodes(node);
  Buffer << "fao";
}

void Remangler::mangleNativePinningAddressor(Node *node) {
  mangleChildNodes(node);
  Buffer << "flp";
}

void Remangler::mangleNativePinningMutableAddressor(Node *node) {
  mangleChildNodes(node);
  Buffer << "faP";
}

void Remangler::mangleNominalTypeDescriptor(Node *node) {
  mangleSingleChildNode(node);
  Buffer << "Mn";
}

void Remangler::mangleNonObjCAttribute(Node *node) {
  Buffer << "TO";
}

void Remangler::mangleNonVariadicTuple(Node *node) {
  mangleTypeList(node);
  Buffer << 't';
}

void Remangler::mangleNumber(Node *node) {
  mangleIndex(node->getIndex());
}

void Remangler::mangleObjCAttribute(Node *node) {
  Buffer << "To";
}

void Remangler::mangleObjCBlock(Node *node) {
  mangleChildNodesReversed(node);
  Buffer << "XB";
}

void Remangler::mangleOwningAddressor(Node *node) {
  mangleChildNodes(node);
  Buffer << "flO";
}

void Remangler::mangleOwningMutableAddressor(Node *node) {
  mangleChildNodes(node);
  Buffer << "faO";
}

void Remangler::manglePartialApplyForwarder(Node *node) {
  mangleChildNodesReversed(node);
  Buffer << "TA";
}

void Remangler::manglePartialApplyObjCForwarder(Node *node) {
  mangleChildNodesReversed(node);
  Buffer << "Ta";
}

void Remangler::manglePostfixOperator(Node *node) {
  mangleIdentifierImpl(node, /*isOperator*/ true);
  Buffer << "oP";
}

void Remangler::manglePrefixOperator(Node *node) {
  mangleIdentifierImpl(node, /*isOperator*/ true);
  Buffer << "op";
}

void Remangler::manglePrivateDeclName(Node *node) {
  mangleChildNodesReversed(node);
  Buffer << "LL";
}

void Remangler::mangleProtocol(Node *node) {
  mangleNominalType(node, 'P');
}

void Remangler::mangleProtocolConformance(Node *node) {
  Node *Ty = getChildOfType(node->getChild(0));
  Node *GenSig = nullptr;
  if (Ty->getKind() == Node::Kind::DependentGenericType) {
    GenSig = Ty->getFirstChild();
    Ty = Ty->getChild(1);
  }
  mangle(Ty);
  if (node->getNumChildren() == 4)
    mangleChildNode(node, 3);
  manglePureProtocol(node->getChild(1));
  mangleChildNode(node, 2);
  if (GenSig)
    mangle(GenSig);
}

void Remangler::mangleProtocolDescriptor(Node *node) {
  manglePureProtocol(getSingleChild(node));
  Buffer << "Mp";
}

void Remangler::mangleProtocolList(Node *node) {
  node = getSingleChild(node, Node::Kind::TypeList);
  bool FirstElem = true;
  for (NodePointer Child : *node) {
    manglePureProtocol(Child);
    mangleListSeparator(FirstElem);
  }
  mangleEndOfList(FirstElem);
  Buffer << 'p';
}

void Remangler::mangleProtocolWitness(Node *node) {
  mangleChildNodes(node);
  Buffer << "TW";
}

void Remangler::mangleProtocolWitnessTable(Node *node) {
  mangleSingleChildNode(node);
  Buffer << "WP";
}

void Remangler::mangleProtocolWitnessTableAccessor(Node *node) {
  mangleSingleChildNode(node);
  Buffer << "Wa";
}

void Remangler::mangleQualifiedArchetype(Node *node) {
  mangleChildNode(node, 1);
  Buffer << "Qq";
  mangleNumber(node->getFirstChild());
}

void Remangler::mangleReabstractionThunk(Node *node) {
  if (node->getNumChildren() == 3) {
    mangleChildNode(node, 1); // type 1
    mangleChildNode(node, 2); // type 2
    mangleChildNode(node, 0); // generic signature
  } else {
    mangleChildNodes(node);
  }
  Buffer << "Tr";
}

void Remangler::mangleReabstractionThunkHelper(Node *node) {
  if (node->getNumChildren() == 3) {
    mangleChildNode(node, 1); // type 1
    mangleChildNode(node, 2); // type 2
    mangleChildNode(node, 0); // generic signature
  } else {
    mangleChildNodes(node);
  }
  Buffer << "TR";
}

void Remangler::mangleReturnType(Node *node) {
  mangleArgumentTuple(node);
}

void Remangler::mangleSILBoxType(Node *node) {
  mangleSingleChildNode(node);
  Buffer << "Xb";
}

void Remangler::mangleSetter(Node *node) {
  mangleChildNodes(node);
  Buffer << "fs";
}

void Remangler::mangleSpecializationPassID(Node *node) {
  Buffer << node->getIndex();
}

void Remangler::mangleSpecializationIsFragile(Node *node) {
  Buffer << 'q';
}

void Remangler::mangleStatic(Node *node) {
  mangleSingleChildNode(node);
  Buffer << 'Z';
}

void Remangler::mangleStructure(Node *node) {
  mangleAnyNominalType(node);
}

void Remangler::mangleSubscript(Node *node) {
  mangleChildNodes(node);
  Buffer << 'i';
}

void Remangler::mangleSuffix(Node *node) {
  // Just add the suffix back on.
  Buffer << node->getText();
}

void Remangler::mangleThinFunctionType(Node *node) {
  mangleFunctionSignature(node);
  Buffer << "Xf";
}

void Remangler::mangleTupleElement(Node *node) {
  mangleChildNodesReversed(node); // tuple type, element name?
}

void Remangler::mangleTupleElementName(Node *node) {
  mangleIdentifier(node);
}

void Remangler::mangleType(Node *node) {
  mangleSingleChildNode(node);
}

void Remangler::mangleTypeAlias(Node *node) {
  mangleChildNodes(node);
  Buffer << 'a';
}

void Remangler::mangleTypeList(Node *node) {
  bool FirstElem = true;
  for (size_t Idx = 0, Num = node->getNumChildren(); Idx < Num; ++Idx) {
    mangleChildNode(node, Idx);
    mangleListSeparator(FirstElem);
  }
  mangleEndOfList(FirstElem);
}

void Remangler::mangleTypeMangling(Node *node) {
  mangleSingleChildNode(node);
  Buffer << 'D';
}

void Remangler::mangleTypeMetadata(Node *node) {
  mangleSingleChildNode(node);
  Buffer << "N";
}

void Remangler::mangleTypeMetadataAccessFunction(Node *node) {
  mangleSingleChildNode(node);
  Buffer << "Ma";
}

void Remangler::mangleTypeMetadataLazyCache(Node *node) {
  mangleChildNodes(node);
  Buffer << "ML";
}

void Remangler::mangleUncurriedFunctionType(Node *node) {
  mangleFunctionSignature(node);
  // Mangle as regular function type (there is no "uncurried function type"
  // in the new mangling scheme).
  Buffer << 'c';
}

void Remangler::mangleUnmanaged(Node *node) {
  mangleSingleChildNode(node);
  Buffer << "Xu";
}

void Remangler::mangleUnowned(Node *node) {
  mangleSingleChildNode(node);
  Buffer << "Xo";
}

void Remangler::mangleUnsafeAddressor(Node *node) {
  mangleChildNodes(node);
  Buffer << "flu";
}

void Remangler::mangleUnsafeMutableAddressor(Node *node) {
  mangleChildNodes(node);
  Buffer << "fau";
}

void Remangler::mangleValueWitness(Node *node) {
  mangleSingleChildNode(node); // type
  const char *Code = nullptr;
  switch (ValueWitnessKind(node->getIndex())) {
#define VALUE_WITNESS(MANGLING, NAME) \
    case ValueWitnessKind::NAME: Code = #MANGLING; break;
#include "swift/Basic/ValueWitnessMangling.def"
  }
  Buffer << 'w' << Code;
}

void Remangler::mangleValueWitnessTable(Node *node) {
  mangleSingleChildNode(node);
  Buffer << "WV";
}

void Remangler::mangleVariable(Node *node) {
  mangleChildNodes(node);
  Buffer << 'v';
}

void Remangler::mangleVariadicTuple(Node *node) {
  mangleTypeList(node);
  Buffer << "dt";
}

void Remangler::mangleVTableAttribute(Node *node) {
  Buffer << "TV";
}

void Remangler::mangleWeak(Node *node) {
  mangleSingleChildNode(node);
  Buffer << "Xw";
}

void Remangler::mangleWillSet(Node *node) {
  mangleChildNodes(node);
  Buffer << "fw";
}

void Remangler::mangleWitnessTableOffset(Node *node) {
  mangleChildNodes(node);
  Buffer << "Wo";
}

void Remangler::mangleReflectionMetadataBuiltinDescriptor(Node *node) {
  mangleSingleChildNode(node);
  Buffer << "MB";
}

void Remangler::mangleReflectionMetadataFieldDescriptor(Node *node) {
  mangleSingleChildNode(node);
  Buffer << "MF";
}

void Remangler::mangleReflectionMetadataAssocTypeDescriptor(Node *node) {
  mangleSingleChildNode(node); // protocol-conformance
  Buffer << "MA";
}

void Remangler::mangleReflectionMetadataSuperclassDescriptor(Node *node) {
  mangleSingleChildNode(node); // protocol-conformance
  Buffer << "MC";
}

void Remangler::mangleCurryThunk(Node *node) {
  mangleSingleChildNode(node);
  Buffer << "Tc";
}

void Remangler::mangleThrowsAnnotation(Node *node) {
  Buffer << 'K';
}

void Remangler::mangleEmptyList(Node *node) {
  Buffer << 'y';
}

void Remangler::mangleFirstElementMarker(Node *node) {
  Buffer << '_';
}

void Remangler::mangleVariadicMarker(Node *node) {
  Buffer << 'd';
}

void Remangler::mangleOutlinedCopy(Node *node) {
  mangleSingleChildNode(node);
  Buffer << "Wy";
}

void Remangler::mangleOutlinedConsume(Node *node) {
  mangleSingleChildNode(node);
  Buffer << "We";
}

void Remangler::mangleSILBoxTypeWithLayout(Node *node) {
  assert(node->getNumChildren() == 1 || node->getNumChildren() == 3);
  assert(node->getChild(0)->getKind() == Node::Kind::SILBoxLayout);
  auto layout = node->getChild(0);
  auto layoutTypeList = Factory.createNode(Node::Kind::TypeList);
  for (unsigned i = 0, e = layout->getNumChildren(); i < e; ++i) {
    assert(layout->getChild(i)->getKind() == Node::Kind::SILBoxImmutableField
           || layout->getChild(i)->getKind() == Node::Kind::SILBoxMutableField);
    auto field = layout->getChild(i);
    assert(field->getNumChildren() == 1
           && field->getChild(0)->getKind() == Node::Kind::Type);
    auto fieldType = field->getChild(0);
    // 'inout' mangling is used to represent mutable fields.
    if (field->getKind() == Node::Kind::SILBoxMutableField) {
      auto inout = Factory.createNode(Node::Kind::InOut);
      inout->addChild(fieldType->getChild(0), Factory);
      fieldType = Factory.createNode(Node::Kind::Type);
      fieldType->addChild(inout, Factory);
    }
    layoutTypeList->addChild(fieldType, Factory);
  }
  mangleTypeList(layoutTypeList);
  
  if (node->getNumChildren() == 3) {
    auto signature = node->getChild(1);
    auto genericArgs = node->getChild(2);
    assert(signature->getKind() == Node::Kind::DependentGenericSignature);
    assert(genericArgs->getKind() == Node::Kind::TypeList);
    mangleTypeList(genericArgs);
    mangleDependentGenericSignature(signature);
    Buffer << "XX";
  } else {
    Buffer << "Xx";
  }
}

void Remangler::mangleSILBoxLayout(Node *node) {
  unreachable("should be part of SILBoxTypeWithLayout");
}

void Remangler::mangleSILBoxMutableField(Node *node) {
  unreachable("should be part of SILBoxTypeWithLayout");
}

void Remangler::mangleSILBoxImmutableField(Node *node) {
  unreachable("should be part of SILBoxTypeWithLayout");
}

} // anonymous namespace

/// The top-level interface to the remangler.
std::string Demangle::mangleNode(const NodePointer &node) {
  if (!node) return "";

  DemanglerPrinter printer;
  Remangler(printer).mangle(node);

  return std::move(printer).str();
}

static bool isInSwiftModule(Node *node) {
  Node *context = node->getFirstChild();
  return (context->getKind() == Node::Kind::Module &&
          context->getText() == STDLIB_NAME);
};

bool Demangle::mangleStandardSubstitution(Node *node, DemanglerPrinter &Out) {
  // Look for known substitutions.
  switch (node->getKind()) {
#define SUCCESS_IF_IS(VALUE, EXPECTED, SUBSTITUTION)            \
do {                                                        \
if ((VALUE) == (EXPECTED)) {                              \
Out << SUBSTITUTION;                                    \
return true;                                            \
}                                                         \
} while (0)
#define SUCCESS_IF_TEXT_IS(EXPECTED, SUBSTITUTION)              \
SUCCESS_IF_IS(node->getText(), EXPECTED, SUBSTITUTION)
#define SUCCESS_IF_DECLNAME_IS(EXPECTED, SUBSTITUTION)          \
SUCCESS_IF_IS(node->getChild(1)->getText(), EXPECTED, SUBSTITUTION)

    case Node::Kind::Module:
      SUCCESS_IF_TEXT_IS(STDLIB_NAME, "s");
      SUCCESS_IF_TEXT_IS(MANGLING_MODULE_OBJC, "So");
      SUCCESS_IF_TEXT_IS(MANGLING_MODULE_C, "SC");
      break;
    case Node::Kind::Structure:
      if (isInSwiftModule(node)) {
        SUCCESS_IF_DECLNAME_IS("Array", "Sa");
        SUCCESS_IF_DECLNAME_IS("Bool", "Sb");
        SUCCESS_IF_DECLNAME_IS("UnicodeScalar", "Sc");
        SUCCESS_IF_DECLNAME_IS("Double", "Sd");
        SUCCESS_IF_DECLNAME_IS("Float", "Sf");
        SUCCESS_IF_DECLNAME_IS("Int", "Si");
        SUCCESS_IF_DECLNAME_IS("UnsafeRawPointer", "SV");
        SUCCESS_IF_DECLNAME_IS("UnsafeMutableRawPointer", "Sv");
        SUCCESS_IF_DECLNAME_IS("UnsafePointer", "SP");
        SUCCESS_IF_DECLNAME_IS("UnsafeMutablePointer", "Sp");
        SUCCESS_IF_DECLNAME_IS("UnsafeBufferPointer", "SR");
        SUCCESS_IF_DECLNAME_IS("UnsafeMutableBufferPointer", "Sr");
        SUCCESS_IF_DECLNAME_IS("String", "SS");
        SUCCESS_IF_DECLNAME_IS("UInt", "Su");
      }
      break;
    case Node::Kind::Enum:
      if (isInSwiftModule(node)) {
        SUCCESS_IF_DECLNAME_IS("Optional", "Sq");
        SUCCESS_IF_DECLNAME_IS("ImplicitlyUnwrappedOptional", "SQ");
      }
      break;

    default:
      break;

#undef SUCCESS_IF_DECLNAME_IS
#undef SUCCESS_IF_TEXT_IS
#undef SUCCESS_IF_IS
  }
  return false;
}

bool Demangle::isSpecialized(Node *node) {
  switch (node->getKind()) {
    case Node::Kind::BoundGenericStructure:
    case Node::Kind::BoundGenericEnum:
    case Node::Kind::BoundGenericClass:
      return true;

    case Node::Kind::Structure:
    case Node::Kind::Enum:
    case Node::Kind::Class: {
      Node *parentOrModule = node->getChild(0);
      if (isSpecialized(parentOrModule))
        return true;

      return false;
    }

    default:
      return false;
  }
}

NodePointer Demangle::getUnspecialized(Node *node, NodeFactory &Factory) {
  switch (node->getKind()) {
    case Node::Kind::Structure:
    case Node::Kind::Enum:
    case Node::Kind::Class: {
      NodePointer result = Factory.createNode(node->getKind());
      NodePointer parentOrModule = node->getChild(0);
      if (isSpecialized(parentOrModule))
        result->addChild(getUnspecialized(parentOrModule, Factory), Factory);
      else
        result->addChild(parentOrModule, Factory);
      result->addChild(node->getChild(1), Factory);
      return result;
    }

    case Node::Kind::BoundGenericStructure:
    case Node::Kind::BoundGenericEnum:
    case Node::Kind::BoundGenericClass: {
      NodePointer unboundType = node->getChild(0);
      assert(unboundType->getKind() == Node::Kind::Type);
      NodePointer nominalType = unboundType->getChild(0);
      if (isSpecialized(nominalType))
        return getUnspecialized(nominalType, Factory);
      else
        return nominalType;
    }
      
    default:
      unreachable("bad nominal type kind");
  }
}
