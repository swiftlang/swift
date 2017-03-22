//===--- Demangler.cpp - String to Node-Tree Demangling -------------------===//
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
//  This file implements new Swift de-mangler.
//
//===----------------------------------------------------------------------===//

#include "swift/Demangling/Demangler.h"
#include "swift/Demangling/ManglingUtils.h"
#include "swift/Demangling/ManglingMacros.h"
#include "swift/Demangling/Punycode.h"
#include "swift/Strings.h"

using namespace swift;
using namespace Mangle;
using swift::Demangle::FunctionSigSpecializationParamKind;

//////////////////////////////////
// Private utility functions    //
//////////////////////////////////

[[noreturn]]
static void demangler_unreachable(const char *Message) {
  fprintf(stderr, "fatal error: %s\n", Message);
  std::abort();
}

namespace {

static bool isDeclName(Node::Kind kind) {
  switch (kind) {
    case Node::Kind::Identifier:
    case Node::Kind::LocalDeclName:
    case Node::Kind::PrivateDeclName:
    case Node::Kind::PrefixOperator:
    case Node::Kind::PostfixOperator:
    case Node::Kind::InfixOperator:
      return true;
    default:
      return false;
  }
}

static bool isContext(Node::Kind kind) {
  switch (kind) {
#define NODE(ID)
#define CONTEXT_NODE(ID)                                                \
    case Node::Kind::ID:
#include "swift/Demangling/DemangleNodes.def"
      return true;
    default:
      return false;
  }
}

static bool isNominal(Node::Kind kind) {
  switch (kind) {
    case Node::Kind::Structure:
    case Node::Kind::Class:
    case Node::Kind::Enum:
    case Node::Kind::Protocol:
      return true;
    default:
      return false;
  }
}

static bool isEntity(Node::Kind kind) {
  // Also accepts some kind which are not entities.
  if (kind == Node::Kind::Type)
    return true;
  return isContext(kind);
}

static bool isRequirement(Node::Kind kind) {
  switch (kind) {
    case Node::Kind::DependentGenericSameTypeRequirement:
    case Node::Kind::DependentGenericLayoutRequirement:
    case Node::Kind::DependentGenericConformanceRequirement:
      return true;
    default:
      return false;
  }
}

static bool isFunctionAttr(Node::Kind kind) {
  switch (kind) {
    case Node::Kind::FunctionSignatureSpecialization:
    case Node::Kind::GenericSpecialization:
    case Node::Kind::GenericSpecializationNotReAbstracted:
    case Node::Kind::GenericPartialSpecialization:
    case Node::Kind::GenericPartialSpecializationNotReAbstracted:
    case Node::Kind::ObjCAttribute:
    case Node::Kind::NonObjCAttribute:
    case Node::Kind::DynamicAttribute:
    case Node::Kind::DirectMethodReferenceAttribute:
    case Node::Kind::VTableAttribute:
    case Node::Kind::PartialApplyForwarder:
    case Node::Kind::PartialApplyObjCForwarder:
      return true;
    default:
      return false;
  }
}

} // anonymous namespace

namespace swift {
namespace Demangle {

//////////////////////////////////
// Node member functions        //
//////////////////////////////////

void Node::addChild(NodePointer Child, NodeFactory &Factory) {
  assert(Child && "adding null child!");
  if (NumChildren >= ReservedChildren)
    Factory.Reallocate(Children, ReservedChildren, 1);
  assert(NumChildren < ReservedChildren);
  Children[NumChildren++] = Child;
}

void Node::reverseChildren(size_t StartingAt) {
  assert(StartingAt <= NumChildren);
  std::reverse(Children + StartingAt, Children + NumChildren);
}

//////////////////////////////////
// NodeFactory member functions //
//////////////////////////////////

void NodeFactory::freeSlabs(Slab *slab) {
  while (slab) {
    Slab *prev = slab->Previous;
#ifdef NODE_FACTORY_DEBUGGING
    std::cerr << "  free slab = " << slab << "\n";
#endif
    free(slab);
    slab = prev;
  }
}
  
void NodeFactory::clear() {
  if (CurrentSlab) {
    freeSlabs(CurrentSlab->Previous);
    
    // Recycle the last allocated slab.
    // Note that the size of the last slab is at least as big as all previous
    // slabs combined. Therefore it's not worth the effort of reusing all slabs.
    // The slab size also stays the same. So at some point the demangling
    // process converges to a single large slab across repeated demangle-clear
    // cycles.
    CurrentSlab->Previous = nullptr;
    CurPtr = (char *)(CurrentSlab + 1);
    assert(End == CurPtr + SlabSize);
  }
}

NodePointer NodeFactory::createNode(Node::Kind K) {
  return new (Allocate<Node>()) Node(K);
}
NodePointer NodeFactory::createNode(Node::Kind K, Node::IndexType Index) {
  return new (Allocate<Node>()) Node(K, Index);
}
NodePointer NodeFactory::createNodeWithAllocatedText(Node::Kind K,
                                                     llvm::StringRef Text) {
  return new (Allocate<Node>()) Node(K, Text);
}
NodePointer NodeFactory::createNode(Node::Kind K, const CharVector &Text) {
  return createNodeWithAllocatedText(K, Text.str());
}
NodePointer NodeFactory::createNode(Node::Kind K, const char *Text) {
  return new (Allocate<Node>()) Node(K, llvm::StringRef(Text));
}

//////////////////////////////////
// CharVector member functions  //
//////////////////////////////////

void CharVector::append(StringRef Rhs, NodeFactory &Factory) {
  if (NumElems + Rhs.size() > Capacity)
    Factory.Reallocate(Elems, Capacity, /*Growth*/ Rhs.size());
  memcpy(Elems + NumElems, Rhs.data(), Rhs.size());
  NumElems += Rhs.size();
  assert(NumElems <= Capacity);
}

void CharVector::append(int Number, NodeFactory &Factory) {
  const int MaxIntPrintSize = 8;
  if (NumElems + MaxIntPrintSize > Capacity)
    Factory.Reallocate(Elems, Capacity, /*Growth*/ MaxIntPrintSize);
  int Length = snprintf(Elems + NumElems, MaxIntPrintSize, "%d", Number);
  assert(Length > 0 && Length < MaxIntPrintSize);
  NumElems += Length;
}

//////////////////////////////////
// Demangler member functions   //
//////////////////////////////////

void Demangler::clear() {
  NodeStack.free();
  Substitutions.free();
  PendingSubstitutions.free();
  NodeFactory::clear();
}

void Demangler::init(StringRef MangledName) {
  NodeStack.init(*this, 16);
  Substitutions.init(*this, 16);
  PendingSubstitutions.init(*this, 4);
  NumWords = 0;
  Text = MangledName;
  Pos = 0;
}
  
NodePointer Demangler::demangleSymbol(StringRef MangledName) {
  init(MangledName);

  // Demangle old-style class and protocol names, which are still used in the
  // ObjC metadata.
  if (nextIf("_Tt"))
    return demangleObjCTypeName();

  if (!nextIf(MANGLING_PREFIX_STR)
      // Also accept the future mangling prefix.
      // TODO: remove this line as soon as MANGLING_PREFIX_STR gets "_S".
      && !nextIf("_S"))
    return nullptr;

  NodePointer topLevel = createNode(Node::Kind::Global);

  parseAndPushNodes();

  // Let a trailing '_' be part of the not demangled suffix.
  popNode(Node::Kind::FirstElementMarker);

  size_t EndPos = (NodeStack.empty() ? 0 : NodeStack.back().Pos);

  NodePointer Parent = topLevel;
  while (NodePointer FuncAttr = popNode(isFunctionAttr)) {
    Parent->addChild(FuncAttr, *this);
    if (FuncAttr->getKind() == Node::Kind::PartialApplyForwarder ||
        FuncAttr->getKind() == Node::Kind::PartialApplyObjCForwarder)
      Parent = FuncAttr;
  }
  for (const NodeWithPos &NWP : NodeStack) {
    NodePointer Nd = NWP.Node;
    switch (Nd->getKind()) {
      case Node::Kind::Type:
        Parent->addChild(Nd->getFirstChild(), *this);
        break;
      default:
        Parent->addChild(Nd, *this);
        break;
    }
  }
  if (topLevel->getNumChildren() == 0)
    return nullptr;

  if (EndPos < Text.size()) {
    topLevel->addChild(createNode(Node::Kind::Suffix, Text.substr(EndPos)), *this);
  }

  return topLevel;
}

NodePointer Demangler::demangleType(StringRef MangledName) {
  init(MangledName);

  parseAndPushNodes();

  if (NodePointer Result = popNode())
    return Result;

  return createNode(Node::Kind::Suffix, Text);
}

void Demangler::parseAndPushNodes() {
  int Idx = 0;
  while (!Text.empty()) {
    NodePointer Node = demangleOperator();
    if (!Node)
      break;
    pushNode(Node);
    Idx++;
  }
}

NodePointer Demangler::addChild(NodePointer Parent, NodePointer Child) {
  if (!Parent || !Child)
    return nullptr;
  Parent->addChild(Child, *this);
  return Parent;
}

NodePointer Demangler::createWithChild(Node::Kind kind,
                                            NodePointer Child) {
  if (!Child)
    return nullptr;
  NodePointer Nd = createNode(kind);
  Nd->addChild(Child, *this);
  return Nd;
}

NodePointer Demangler::createType(NodePointer Child) {
  return createWithChild(Node::Kind::Type, Child);
}

NodePointer Demangler::Demangler::createWithChildren(Node::Kind kind,
                                       NodePointer Child1, NodePointer Child2) {
  if (!Child1 || !Child2)
    return nullptr;
  NodePointer Nd = createNode(kind);
  Nd->addChild(Child1, *this);
  Nd->addChild(Child2, *this);
  return Nd;
}

NodePointer Demangler::createWithChildren(Node::Kind kind,
                                               NodePointer Child1,
                                               NodePointer Child2,
                                               NodePointer Child3) {
  if (!Child1 || !Child2 || !Child3)
    return nullptr;
  NodePointer Nd = createNode(kind);
  Nd->addChild(Child1, *this);
  Nd->addChild(Child2, *this);
  Nd->addChild(Child3, *this);
  return Nd;
}

NodePointer Demangler::changeKind(NodePointer Node, Node::Kind NewKind) {
  if (!Node)
    return nullptr;
  NodePointer NewNode = nullptr;
  if (Node->hasText()) {
    NewNode = createNodeWithAllocatedText(NewKind, Node->getText());
  } else if (Node->hasIndex()) {
    NewNode = createNode(NewKind, Node->getIndex());
  } else {
    NewNode = createNode(NewKind);
  }
  for (NodePointer Child : *Node) {
    NewNode->addChild(Child, *this);
  }
  return NewNode;
}

NodePointer Demangler::demangleOperator() {
  switch (char c = nextChar()) {
    case 'A': return demangleMultiSubstitutions();
    case 'B': return demangleBuiltinType();
    case 'C': return demangleNominalType(Node::Kind::Class);
    case 'D': return createWithChild(Node::Kind::TypeMangling,
                                     popNode(Node::Kind::Type));
    case 'E': return demangleExtensionContext();
    case 'F': return demanglePlainFunction();
    case 'G': return demangleBoundGenericType();
    case 'I': return demangleImplFunctionType();
    case 'K': return createNode(Node::Kind::ThrowsAnnotation);
    case 'L': return demangleLocalIdentifier();
    case 'M': return demangleMetatype();
    case 'N': return createWithChild(Node::Kind::TypeMetadata,
                                     popNode(Node::Kind::Type));
    case 'O': return demangleNominalType(Node::Kind::Enum);
    case 'P': return demangleNominalType(Node::Kind::Protocol);
    case 'Q': return demangleArchetype();
    case 'R': return demangleGenericRequirement();
    case 'S': return demangleStandardSubstitution();
    case 'T': return demangleThunkOrSpecialization();
    case 'V': return demangleNominalType(Node::Kind::Structure);
    case 'W': return demangleWitness();
    case 'X': return demangleSpecialType();
    case 'Z': return createWithChild(Node::Kind::Static, popNode(isEntity));
    case 'a': return demangleTypeAlias();
    case 'c': return popFunctionType(Node::Kind::FunctionType);
    case 'd': return createNode(Node::Kind::VariadicMarker);
    case 'f': return demangleFunctionEntity();
    case 'i': return demangleEntity(Node::Kind::Subscript);
    case 'l': return demangleGenericSignature(/*hasParamCounts*/ false);
    case 'm': return createType(createWithChild(Node::Kind::Metatype,
                                                popNode(Node::Kind::Type)));
    case 'o': return demangleOperatorIdentifier();
    case 'p': return demangleProtocolListType();
    case 'q': return createType(demangleGenericParamIndex());
    case 'r': return demangleGenericSignature(/*hasParamCounts*/ true);
    case 's': return createNode(Node::Kind::Module, STDLIB_NAME);
    case 't': return popTuple();
    case 'u': return demangleGenericType();
    case 'v': return demangleEntity(Node::Kind::Variable);
    case 'w': return demangleValueWitness();
    case 'x': return createType(getDependentGenericParamType(0, 0));
    case 'y': return createNode(Node::Kind::EmptyList);
    case 'z': return createType(createWithChild(Node::Kind::InOut,
                                                popTypeAndGetChild()));
    case '_': return createNode(Node::Kind::FirstElementMarker);
    default:
      pushBack();
      return demangleIdentifier();
  }
}

int Demangler::demangleNatural() {
  if (!isDigit(peekChar()))
    return -1000;
  int num = 0;
  while (true) {
    char c = peekChar();
    if (!isDigit(c))
      return num;
    int newNum = (10 * num) + (c - '0');
    if (newNum < num)
      return -1000;
    num = newNum;
    nextChar();
  }
}

int Demangler::demangleIndex() {
  if (nextIf('_'))
    return 0;
  int num = demangleNatural();
  if (num >= 0 && nextIf('_'))
    return num + 1;
  return -1000;
}

NodePointer Demangler::demangleIndexAsNode() {
  int Idx = demangleIndex();
  if (Idx >= 0)
    return createNode(Node::Kind::Number, Idx);
  return nullptr;
}

NodePointer Demangler::demangleMultiSubstitutions() {
  int RepeatCount = -1;
  while (true) {
    char c = nextChar();
    if (isLowerLetter(c)) {
      // It's a substitution with an index < 26.
      NodePointer Nd = pushMultiSubstitutions(RepeatCount, c - 'a');
      if (!Nd)
        return nullptr;
      pushNode(Nd);
      RepeatCount = -1;
      // A lowercase letter indicates that there are more substitutions to
      // follow.
      continue;
    }
    if (isUpperLetter(c)) {
      // The last substitution.
      return pushMultiSubstitutions(RepeatCount, c - 'A');
    }
    if (c == '_') {
      // The previously demangled number is actually not a repeat count but
      // the large (> 26) index of a substitution. Because it's an index we
      // have to add 27 and not 26.
      unsigned Idx = RepeatCount + 27;
      if (Idx >= Substitutions.size())
        return nullptr;
      return Substitutions[Idx];
    }
    pushBack();
    // Not a letter? Then it can only be a natural number which might be the
    // repeat count or a large (> 26) substitution index.
    RepeatCount = demangleNatural();
    if (RepeatCount < 0)
      return nullptr;
  }
}

NodePointer Demangler::pushMultiSubstitutions(int RepeatCount,
                                              size_t SubstIdx) {
  if (SubstIdx >= Substitutions.size())
    return nullptr;
  if (RepeatCount > SubstitutionMerging::MaxRepeatCount)
    return nullptr;
  NodePointer Nd = Substitutions[SubstIdx];
  while (RepeatCount-- > 1) {
    pushNode(Nd);
  }
  return Nd;
}

NodePointer Demangler::createSwiftType(Node::Kind typeKind, const char *name) {
  return createType(createWithChildren(typeKind,
    createNode(Node::Kind::Module, STDLIB_NAME),
    createNode(Node::Kind::Identifier, name)));
}

NodePointer Demangler::demangleStandardSubstitution() {
  switch (char c = nextChar()) {
    case 'o':
      return createNode(Node::Kind::Module, MANGLING_MODULE_OBJC);
    case 'C':
      return createNode(Node::Kind::Module, MANGLING_MODULE_C);
    case 'g': {
      NodePointer OptionalTy =
        createType(createWithChildren(Node::Kind::BoundGenericEnum,
          createSwiftType(Node::Kind::Enum, "Optional"),
          createWithChild(Node::Kind::TypeList, popNode(Node::Kind::Type))));
      addSubstitution(OptionalTy);
      return OptionalTy;
    }
    default: {
      pushBack();
      int RepeatCount = demangleNatural();
      if (RepeatCount > SubstitutionMerging::MaxRepeatCount)
        return nullptr;
      if (NodePointer Nd = createStandardSubstitution(nextChar())) {
        while (RepeatCount-- > 1) {
          pushNode(Nd);
        }
        return Nd;
      }
      return nullptr;
    }
  }
}

NodePointer Demangler::createStandardSubstitution(char Subst) {
#define STANDARD_TYPE(KIND, MANGLING, TYPENAME)                   \
  if (Subst == #MANGLING[0]) {                                    \
    return createSwiftType(Node::Kind::KIND, #TYPENAME);      \
  }

#include "swift/Demangling/StandardTypesMangling.def"
  return nullptr;
}

NodePointer Demangler::demangleIdentifier() {
  bool hasWordSubsts = false;
  bool isPunycoded = false;
  char c = peekChar();
  if (!isDigit(c))
    return nullptr;
  if (c == '0') {
    nextChar();
    if (peekChar() == '0') {
      nextChar();
      isPunycoded = true;
    } else {
      hasWordSubsts = true;
    }
  }
  CharVector Identifier;
  do {
    while (hasWordSubsts && isLetter(peekChar())) {
      char c = nextChar();
      int WordIdx = 0;
      if (isLowerLetter(c)) {
        WordIdx = c - 'a';
      } else {
        assert(isUpperLetter(c));
        WordIdx = c - 'A';
        hasWordSubsts = false;
      }
      if (WordIdx >= NumWords)
        return nullptr;
      assert(WordIdx < MaxNumWords);
      StringRef Slice = Words[WordIdx];
      Identifier.append(Slice, *this);
    }
    if (nextIf('0'))
      break;
    int numChars = demangleNatural();
    if (numChars <= 0)
      return nullptr;
    if (isPunycoded)
      nextIf('_');
    if (Pos + numChars > Text.size())
      return nullptr;
    StringRef Slice = StringRef(Text.data() + Pos, numChars);
    if (isPunycoded) {
      std::string PunycodedString;
      if (!Punycode::decodePunycodeUTF8(Slice, PunycodedString))
        return nullptr;
      Identifier.append(StringRef(PunycodedString), *this);
    } else {
      Identifier.append(Slice, *this);
      int wordStartPos = -1;
      for (int Idx = 0, End = (int)Slice.size(); Idx <= End; ++Idx) {
        char c = (Idx < End ? Slice[Idx] : 0);
        if (wordStartPos >= 0 && isWordEnd(c, Slice[Idx - 1])) {
          if (Idx - wordStartPos >= 2 && NumWords < MaxNumWords) {
            StringRef word(Slice.begin() + wordStartPos, Idx - wordStartPos);
            Words[NumWords++] = word;
          }
          wordStartPos = -1;
        }
        if (wordStartPos < 0 && isWordStart(c)) {
          wordStartPos = Idx;
        }
      }
    }
    Pos += numChars;
  } while (hasWordSubsts);

  if (Identifier.empty())
    return nullptr;
  NodePointer Ident = createNode(Node::Kind::Identifier, Identifier);
  addSubstitution(Ident);
  return Ident;
}

NodePointer Demangler::demangleOperatorIdentifier() {
  NodePointer Ident = popNode(Node::Kind::Identifier);
  if (!Ident)
    return nullptr;

  static const char op_char_table[] = "& @/= >    <*!|+?%-~   ^ .";

  CharVector OpStr;
  for (signed char c : Ident->getText()) {
    if (c < 0) {
      // Pass through Unicode characters.
      OpStr.push_back(c, *this);
      continue;
    }
    if (!isLowerLetter(c))
      return nullptr;
    char o = op_char_table[c - 'a'];
    if (o == ' ')
      return nullptr;
    OpStr.push_back(o, *this);
  }
  switch (nextChar()) {
    case 'i': return createNode(Node::Kind::InfixOperator, OpStr);
    case 'p': return createNode(Node::Kind::PrefixOperator, OpStr);
    case 'P': return createNode(Node::Kind::PostfixOperator, OpStr);
    default: return nullptr;
  }
}

NodePointer Demangler::demangleLocalIdentifier() {
  if (nextIf('L')) {
    NodePointer discriminator = popNode(Node::Kind::Identifier);
    NodePointer name = popNode(isDeclName);
    return createWithChildren(Node::Kind::PrivateDeclName, discriminator, name);
  }
  NodePointer discriminator = demangleIndexAsNode();
  NodePointer name = popNode(isDeclName);
  return createWithChildren(Node::Kind::LocalDeclName, discriminator, name);
}

NodePointer Demangler::popModule() {
  if (NodePointer Ident = popNode(Node::Kind::Identifier))
    return changeKind(Ident, Node::Kind::Module);
  return popNode(Node::Kind::Module);
}

NodePointer Demangler::popContext() {
  if (NodePointer Mod = popModule())
    return Mod;

  if (NodePointer Ty = popNode(Node::Kind::Type)) {
    if (Ty->getNumChildren() != 1)
      return nullptr;
    NodePointer Child = Ty->getFirstChild();
    if (!isContext(Child->getKind()))
      return nullptr;
    return Child;
  }
  return popNode(isContext);
}

NodePointer Demangler::popTypeAndGetChild() {
  NodePointer Ty = popNode(Node::Kind::Type);
  if (!Ty || Ty->getNumChildren() != 1)
    return nullptr;
  return Ty->getFirstChild();
}

NodePointer Demangler::popTypeAndGetNominal() {
  NodePointer Child = popTypeAndGetChild();
  if (Child && isNominal(Child->getKind()))
    return Child;
  return nullptr;
}

NodePointer Demangler::demangleBuiltinType() {
  NodePointer Ty = nullptr;
  switch (nextChar()) {
    case 'b':
      Ty = createNode(Node::Kind::BuiltinTypeName,
                               "Builtin.BridgeObject");
      break;
    case 'B':
      Ty = createNode(Node::Kind::BuiltinTypeName,
                              "Builtin.UnsafeValueBuffer");
      break;
    case 'f': {
      int size = demangleIndex() - 1;
      if (size <= 0)
        return nullptr;
      CharVector name;
      name.append("Builtin.Float", *this);
      name.append(size, *this);
      Ty = createNode(Node::Kind::BuiltinTypeName, name);
      break;
    }
    case 'i': {
      int size = demangleIndex() - 1;
      if (size <= 0)
        return nullptr;
      CharVector name;
      name.append("Builtin.Int", *this);
      name.append(size, *this);
      Ty = createNode(Node::Kind::BuiltinTypeName, name);
      break;
    }
    case 'v': {
      int elts = demangleIndex() - 1;
      if (elts <= 0)
        return nullptr;
      NodePointer EltType = popTypeAndGetChild();
      if (!EltType || EltType->getKind() != Node::Kind::BuiltinTypeName ||
          !EltType->getText().startswith("Builtin."))
        return nullptr;
      CharVector name;
      name.append("Builtin.Vec", *this);
      name.append(elts, *this);
      name.push_back('x', *this);
      name.append(EltType->getText().substr(sizeof("Builtin.") - 1), *this);
      Ty = createNode(Node::Kind::BuiltinTypeName, name);
      break;
    }
    case 'O':
      Ty = createNode(Node::Kind::BuiltinTypeName,
                               "Builtin.UnknownObject");
      break;
    case 'o':
      Ty = createNode(Node::Kind::BuiltinTypeName,
                               "Builtin.NativeObject");
      break;
    case 'p':
      Ty = createNode(Node::Kind::BuiltinTypeName,
                               "Builtin.RawPointer");
      break;
    case 'w':
      Ty = createNode(Node::Kind::BuiltinTypeName,
                               "Builtin.Word");
      break;
    default:
      return nullptr;
  }
  return createType(Ty);
}

NodePointer Demangler::demangleNominalType(Node::Kind kind) {
  NodePointer Name = popNode(isDeclName);
  NodePointer Ctx = popContext();
  NodePointer NTy = createType(createWithChildren(kind, Ctx, Name));
  addSubstitution(NTy);
  return NTy;
}

NodePointer Demangler::demangleTypeAlias() {
  NodePointer Name = popNode(isDeclName);
  NodePointer Ctx = popContext();
  return createType(createWithChildren(Node::Kind::TypeAlias, Ctx, Name));
}

NodePointer Demangler::demangleExtensionContext() {
  NodePointer GenSig = popNode(Node::Kind::DependentGenericSignature);
  NodePointer Module = popModule();
  NodePointer Type = popTypeAndGetNominal();
  NodePointer Ext = createWithChildren(Node::Kind::Extension, Module, Type);
  if (GenSig)
    Ext = addChild(Ext, GenSig);
  return Ext;
}

NodePointer Demangler::demanglePlainFunction() {
  NodePointer GenSig = popNode(Node::Kind::DependentGenericSignature);
  NodePointer Type = popFunctionType(Node::Kind::FunctionType);
  if (GenSig) {
    Type = createType(createWithChildren(Node::Kind::DependentGenericType,
                                         GenSig, Type));
  }
  NodePointer Name = popNode(isDeclName);
  NodePointer Ctx = popContext();
  return createWithChildren(Node::Kind::Function, Ctx, Name, Type);
}

NodePointer Demangler::popFunctionType(Node::Kind kind) {
  NodePointer FuncType = createNode(kind);
  addChild(FuncType, popNode(Node::Kind::ThrowsAnnotation));

  FuncType = addChild(FuncType, popFunctionParams(Node::Kind::ArgumentTuple));
  FuncType = addChild(FuncType, popFunctionParams(Node::Kind::ReturnType));
  return createType(FuncType);
}

NodePointer Demangler::popFunctionParams(Node::Kind kind) {
  NodePointer ParamsType = nullptr;
  if (popNode(Node::Kind::EmptyList)) {
    ParamsType = createType(createNode(Node::Kind::NonVariadicTuple));
  } else {
    ParamsType = popNode(Node::Kind::Type);
  }
  return createWithChild(kind, ParamsType);
}

NodePointer Demangler::popTuple() {
  NodePointer Root = createNode(popNode(Node::Kind::VariadicMarker) ?
                                         Node::Kind::VariadicTuple :
                                         Node::Kind::NonVariadicTuple);

  if (!popNode(Node::Kind::EmptyList)) {
    bool firstElem = false;
    do {
      firstElem = (popNode(Node::Kind::FirstElementMarker) != nullptr);
      NodePointer TupleElmt = createNode(Node::Kind::TupleElement);
      if (NodePointer Ident = popNode(Node::Kind::Identifier)) {
        TupleElmt->addChild(createNodeWithAllocatedText(
                              Node::Kind::TupleElementName, Ident->getText()),
                            *this);
      }
      NodePointer Ty = popNode(Node::Kind::Type);
      if (!Ty)
        return nullptr;
      TupleElmt->addChild(Ty, *this);
      Root->addChild(TupleElmt, *this);
    } while (!firstElem);

    Root->reverseChildren();
  }
  return createType(Root);
}

NodePointer Demangler::popTypeList() {
  NodePointer Root = createNode(Node::Kind::TypeList);

  if (!popNode(Node::Kind::EmptyList)) {
    bool firstElem = false;
    do {
      firstElem = (popNode(Node::Kind::FirstElementMarker) != nullptr);
      NodePointer Ty = popNode(Node::Kind::Type);
      if (!Ty)
        return nullptr;
      Root->addChild(Ty, *this);
    } while (!firstElem);
    
    Root->reverseChildren();
  }
  return Root;
}

NodePointer Demangler::popProtocol() {
  NodePointer Name = popNode(isDeclName);
  NodePointer Ctx = popContext();
  NodePointer Proto = createWithChildren(Node::Kind::Protocol, Ctx, Name);
  return createType(Proto);
}

NodePointer Demangler::demangleBoundGenericType() {
  Vector<NodePointer> TypeListList(*this, 4);
  for (;;) {
    NodePointer TList = createNode(Node::Kind::TypeList);
    TypeListList.push_back(TList, *this);
    while (NodePointer Ty = popNode(Node::Kind::Type)) {
      TList->addChild(Ty, *this);
    }
    TList->reverseChildren();

    if (popNode(Node::Kind::EmptyList))
      break;
    if (!popNode(Node::Kind::FirstElementMarker))
      return nullptr;
  }
  NodePointer Nominal = popTypeAndGetNominal();
  NodePointer NTy = createType(demangleBoundGenericArgs(Nominal, TypeListList, 0));
  addSubstitution(NTy);
  return NTy;
}

NodePointer Demangler::demangleBoundGenericArgs(NodePointer Nominal,
                                    const Vector<NodePointer> &TypeLists,
                                    size_t TypeListIdx) {
  if (!Nominal || Nominal->getNumChildren() < 2)
    return nullptr;

  if (TypeListIdx >= TypeLists.size())
    return nullptr;
  NodePointer args = TypeLists[TypeListIdx++];

  // Generic arguments for the outermost type come first.
  NodePointer Context = Nominal->getFirstChild();

  if (TypeListIdx < TypeLists.size()) {
    NodePointer BoundParent = nullptr;
    if (Context->getKind() == Node::Kind::Extension) {
      BoundParent = demangleBoundGenericArgs(Context->getChild(1), TypeLists,
                                             TypeListIdx);
      BoundParent = createWithChildren(Node::Kind::Extension,
                                       Context->getFirstChild(),
                                       BoundParent);
    } else {
      BoundParent = demangleBoundGenericArgs(Context, TypeLists, TypeListIdx);
    }
    // Rebuild this type with the new parent type, which may have
    // had its generic arguments applied.
    Nominal = createWithChildren(Nominal->getKind(), BoundParent,
                                 Nominal->getChild(1));
    if (!Nominal)
      return nullptr;
  }

  // If there were no arguments at this level there is nothing left
  // to do.
  if (args->getNumChildren() == 0)
    return Nominal;

  Node::Kind kind;
  switch (Nominal->getKind()) {
    case Node::Kind::Class:
      kind = Node::Kind::BoundGenericClass;
      break;
    case Node::Kind::Structure:
      kind = Node::Kind::BoundGenericStructure;
      break;
    case Node::Kind::Enum:
      kind = Node::Kind::BoundGenericEnum;
      break;
    default:
      return nullptr;
  }
  return createWithChildren(kind, createType(Nominal), args);
}

NodePointer Demangler::demangleImplParamConvention() {
  const char *attr = nullptr;
  switch (nextChar()) {
    case 'i': attr = "@in"; break;
    case 'l': attr = "@inout"; break;
    case 'b': attr = "@inout_aliasable"; break;
    case 'n': attr = "@in_guaranteed"; break;
    case 'x': attr = "@owned"; break;
    case 'g': attr = "@guaranteed"; break;
    case 'e': attr = "@deallocating"; break;
    case 'y': attr = "@unowned"; break;
    default:
      pushBack();
      return nullptr;
  }
  return createWithChild(Node::Kind::ImplParameter,
                         createNode(Node::Kind::ImplConvention, attr));
}

NodePointer Demangler::demangleImplResultConvention(Node::Kind ConvKind) {
  const char *attr = nullptr;
  switch (nextChar()) {
    case 'r': attr = "@out"; break;
    case 'o': attr = "@owned"; break;
    case 'd': attr = "@unowned"; break;
    case 'u': attr = "@unowned_inner_pointer"; break;
    case 'a': attr = "@autoreleased"; break;
    default:
      pushBack();
      return nullptr;
  }
  return createWithChild(ConvKind,
                         createNode(Node::Kind::ImplConvention, attr));
}

NodePointer Demangler::demangleImplFunctionType() {
  NodePointer type = createNode(Node::Kind::ImplFunctionType);

  NodePointer GenSig = popNode(Node::Kind::DependentGenericSignature);
  if (GenSig && nextIf('P'))
    GenSig = changeKind(GenSig, Node::Kind::DependentPseudogenericSignature);

  const char *CAttr = nullptr;
  switch (nextChar()) {
    case 'y': CAttr = "@callee_unowned"; break;
    case 'g': CAttr = "@callee_guaranteed"; break;
    case 'x': CAttr = "@callee_owned"; break;
    case 't': CAttr = "@convention(thin)"; break;
    default: return nullptr;
  }
  type->addChild(createNode(Node::Kind::ImplConvention, CAttr), *this);

  const char *FAttr = nullptr;
  switch (nextChar()) {
    case 'B': FAttr = "@convention(block)"; break;
    case 'C': FAttr = "@convention(c)"; break;
    case 'M': FAttr = "@convention(method)"; break;
    case 'O': FAttr = "@convention(objc_method)"; break;
    case 'K': FAttr = "@convention(closure)"; break;
    case 'W': FAttr = "@convention(witness_method)"; break;
    default:
      pushBack();
      break;
  }
  if (FAttr)
    type->addChild(createNode(Node::Kind::ImplFunctionAttribute, FAttr), *this);

  addChild(type, GenSig);

  int NumTypesToAdd = 0;
  while (NodePointer Param = demangleImplParamConvention()) {
    type = addChild(type, Param);
    NumTypesToAdd++;
  }
  while (NodePointer Result = demangleImplResultConvention(
                                                    Node::Kind::ImplResult)) {
    type = addChild(type, Result);
    NumTypesToAdd++;
  }
  if (nextIf('z')) {
    NodePointer ErrorResult = demangleImplResultConvention(
                                                  Node::Kind::ImplErrorResult);
    if (!ErrorResult)
      return nullptr;
    type = addChild(type, ErrorResult);
    NumTypesToAdd++;
  }
  if (!nextIf('_'))
    return nullptr;

  for (int Idx = 0; Idx < NumTypesToAdd; ++Idx) {
    NodePointer ConvTy = popNode(Node::Kind::Type);
    if (!ConvTy)
      return nullptr;
    type->getChild(type->getNumChildren() - Idx - 1)->addChild(ConvTy, *this);
  }
  return createType(type);
}

NodePointer Demangler::demangleMetatype() {
  switch (nextChar()) {
    case 'f':
      return createWithPoppedType(Node::Kind::FullTypeMetadata);
    case 'P':
      return createWithPoppedType(Node::Kind::GenericTypeMetadataPattern);
    case 'a':
      return createWithPoppedType(Node::Kind::TypeMetadataAccessFunction);
    case 'L':
      return createWithPoppedType(Node::Kind::TypeMetadataLazyCache);
    case 'm':
      return createWithPoppedType(Node::Kind::Metaclass);
    case 'n':
      return createWithPoppedType(Node::Kind::NominalTypeDescriptor);
    case 'p':
      return createWithChild(Node::Kind::ProtocolDescriptor, popProtocol());
    case 'B':
      return createWithChild(Node::Kind::ReflectionMetadataBuiltinDescriptor,
                             popNode(Node::Kind::Type));
    case 'F':
      return createWithChild(Node::Kind::ReflectionMetadataFieldDescriptor,
                             popNode(Node::Kind::Type));
    case 'A':
      return createWithChild(Node::Kind::ReflectionMetadataAssocTypeDescriptor,
                             popProtocolConformance());
    case 'C': {
      NodePointer Ty = popNode(Node::Kind::Type);
      if (!Ty || !isNominal(Ty->getChild(0)->getKind()))
        return nullptr;
      return createWithChild(Node::Kind::ReflectionMetadataSuperclassDescriptor,
                             Ty->getChild(0));
    }
    default:
      return nullptr;
  }
}

NodePointer Demangler::demangleArchetype() {
  switch (nextChar()) {
    case 'a': {
      NodePointer Ident = popNode(Node::Kind::Identifier);
      NodePointer ArcheTy = popTypeAndGetChild();
      NodePointer AssocTy = createType(
            createWithChildren(Node::Kind::AssociatedTypeRef, ArcheTy, Ident));
      addSubstitution(AssocTy);
      return AssocTy;
    }
    case 'q': {
      NodePointer Idx = demangleIndexAsNode();
      NodePointer Ctx = popContext();
      NodePointer DeclCtx = createWithChild(Node::Kind::DeclContext, Ctx);
      return createType(createWithChildren(Node::Kind::QualifiedArchetype,
                                           Idx, DeclCtx));
    }
    case 'y': {
      NodePointer T = demangleAssociatedTypeSimple(demangleGenericParamIndex());
      addSubstitution(T);
      return T;
    }
    case 'z': {
      NodePointer T = demangleAssociatedTypeSimple(getDependentGenericParamType(0, 0));
      addSubstitution(T);
      return T;
    }
    case 'Y': {
      NodePointer T = demangleAssociatedTypeCompound(demangleGenericParamIndex());
      addSubstitution(T);
      return T;
    }
    case 'Z': {
      NodePointer T = demangleAssociatedTypeCompound(getDependentGenericParamType(0, 0));
      addSubstitution(T);
      return T;
    }
    default:
      return nullptr;
  }
}

NodePointer Demangler::demangleAssociatedTypeSimple(
                                                  NodePointer GenericParamIdx) {
  NodePointer GPI = createType(GenericParamIdx);
  NodePointer ATName = popAssocTypeName();
  return createType(createWithChildren(Node::Kind::DependentMemberType,
                                       GPI, ATName));
}

NodePointer Demangler::demangleAssociatedTypeCompound(
                                                  NodePointer GenericParamIdx) {
  Vector<NodePointer> AssocTyNames(*this, 4);
  bool firstElem = false;
  do {
    firstElem = (popNode(Node::Kind::FirstElementMarker) != nullptr);
    NodePointer AssocTyName = popAssocTypeName();
    if (!AssocTyName)
      return nullptr;
    AssocTyNames.push_back(AssocTyName, *this);
  } while (!firstElem);

  NodePointer Base = GenericParamIdx;

  while (NodePointer AssocTy = AssocTyNames.pop_back_val()) {
    NodePointer depTy = createNode(Node::Kind::DependentMemberType);
    depTy = addChild(depTy, createType(Base));
    Base = addChild(depTy, AssocTy);
  }
  return createType(Base);
}

NodePointer Demangler::popAssocTypeName() {
  NodePointer Proto = popNode(Node::Kind::Type);
  if (Proto && Proto->getFirstChild()->getKind() != Node::Kind::Protocol)
    return nullptr;

  NodePointer Id = popNode(Node::Kind::Identifier);
  NodePointer AssocTy = changeKind(Id, Node::Kind::DependentAssociatedTypeRef);
  addChild(AssocTy, Proto);
  return AssocTy;
}

NodePointer Demangler::getDependentGenericParamType(int depth, int index) {
  if (depth < 0 || index < 0)
    return nullptr;

  CharVector name;
  int idxChar = index;
  do {
    name.push_back((char)('A' + (idxChar % 26)), *this);
    idxChar /= 26;
  } while (idxChar);
  if (depth != 0)
    name.append(depth, *this);

  auto paramTy = createNode(Node::Kind::DependentGenericParamType, name);
  paramTy->addChild(createNode(Node::Kind::Index, depth), *this);
  paramTy->addChild(createNode(Node::Kind::Index, index), *this);
  return paramTy;
}

NodePointer Demangler::demangleGenericParamIndex() {
  if (nextIf('d')) {
    int depth = demangleIndex() + 1;
    int index = demangleIndex();
    return getDependentGenericParamType(depth, index);
  }
  if (nextIf('z')) {
    return getDependentGenericParamType(0, 0);
  }
  return getDependentGenericParamType(0, demangleIndex() + 1);
}

NodePointer Demangler::popProtocolConformance() {
  NodePointer GenSig = popNode(Node::Kind::DependentGenericSignature);
  NodePointer Module = popModule();
  NodePointer Proto = popProtocol();
  NodePointer Type = popNode(Node::Kind::Type);
  NodePointer Ident = nullptr;
  if (!Type) {
    // Property behavior conformance
    Ident = popNode(Node::Kind::Identifier);
    Type = popNode(Node::Kind::Type);
  }
  if (GenSig) {
    Type = createType(createWithChildren(Node::Kind::DependentGenericType,
                                         GenSig, Type));
  }
  NodePointer Conf = createWithChildren(Node::Kind::ProtocolConformance,
                                        Type, Proto, Module);
  addChild(Conf, Ident);
  return Conf;
}

NodePointer Demangler::demangleThunkOrSpecialization() {
  switch (char c = nextChar()) {
    case 'c': return createWithChild(Node::Kind::CurryThunk, popNode(isEntity));
    case 'o': return createNode(Node::Kind::ObjCAttribute);
    case 'O': return createNode(Node::Kind::NonObjCAttribute);
    case 'D': return createNode(Node::Kind::DynamicAttribute);
    case 'd': return createNode(Node::Kind::DirectMethodReferenceAttribute);
    case 'V': return createNode(Node::Kind::VTableAttribute);
    case 'a': return createNode(Node::Kind::PartialApplyObjCForwarder);
    case 'A': return createNode(Node::Kind::PartialApplyForwarder);
    case 'W': {
      NodePointer Entity = popNode(isEntity);
      NodePointer Conf = popProtocolConformance();
      return createWithChildren(Node::Kind::ProtocolWitness, Conf, Entity);
    }
    case 'R':
    case 'r': {
      NodePointer Thunk = createNode(c == 'R' ?
                                        Node::Kind::ReabstractionThunkHelper :
                                        Node::Kind::ReabstractionThunk);
      if (NodePointer GenSig = popNode(Node::Kind::DependentGenericSignature))
        addChild(Thunk, GenSig);
      NodePointer Ty2 = popNode(Node::Kind::Type);
      Thunk = addChild(Thunk, popNode(Node::Kind::Type));
      return addChild(Thunk, Ty2);
    }
    case'g':
      return demangleGenericSpecialization(Node::Kind::GenericSpecialization);
    case'G':
      return demangleGenericSpecialization(Node::Kind::
                                          GenericSpecializationNotReAbstracted);
    case'p': {
      NodePointer Spec = demangleSpecAttributes(Node::Kind::
                                                GenericPartialSpecialization);
      NodePointer Param = createWithChild(Node::Kind::GenericSpecializationParam,
                                          popNode(Node::Kind::Type));
      return addChild(Spec, Param);
    }
    case'P': {
      NodePointer Spec = demangleSpecAttributes(Node::Kind::
                                  GenericPartialSpecializationNotReAbstracted);
      NodePointer Param = createWithChild(Node::Kind::GenericSpecializationParam,
                                          popNode(Node::Kind::Type));
      return addChild(Spec, Param);
    }
    case'f':
      return demangleFunctionSpecialization();
    default:
      return nullptr;
  }
}

NodePointer Demangler::demangleGenericSpecialization(Node::Kind SpecKind) {
  NodePointer Spec = demangleSpecAttributes(SpecKind);
  NodePointer TyList = popTypeList();
  if (!TyList)
    return nullptr;
  for (NodePointer Ty : *TyList) {
    Spec->addChild(createWithChild(Node::Kind::GenericSpecializationParam, Ty), *this);
  }
  return Spec;
}

NodePointer Demangler::demangleFunctionSpecialization() {
  NodePointer Spec = demangleSpecAttributes(
        Node::Kind::FunctionSignatureSpecialization, /*demangleUniqueID*/ true);
  unsigned ParamIdx = 0;
  while (Spec && !nextIf('_')) {
    Spec = addChild(Spec, demangleFuncSpecParam(ParamIdx));
    ParamIdx++;
  }
  if (!nextIf('n'))
    Spec = addChild(Spec, demangleFuncSpecParam(Node::IndexType(~0)));

  if (!Spec)
    return nullptr;

  // Add the required parameters in reverse order.
  for (size_t Idx = 0, Num = Spec->getNumChildren(); Idx < Num; ++Idx) {
    NodePointer Param = Spec->getChild(Num - Idx - 1);
    if (Param->getKind() != Node::Kind::FunctionSignatureSpecializationParam)
      continue;

    if (Param->getNumChildren() == 0)
      continue;
    NodePointer KindNd = Param->getFirstChild();
    assert(KindNd->getKind() ==
             Node::Kind::FunctionSignatureSpecializationParamKind);
    auto ParamKind = (FunctionSigSpecializationParamKind)KindNd->getIndex();
    switch (ParamKind) {
      case FunctionSigSpecializationParamKind::ConstantPropFunction:
      case FunctionSigSpecializationParamKind::ConstantPropGlobal:
      case FunctionSigSpecializationParamKind::ConstantPropString:
      case FunctionSigSpecializationParamKind::ClosureProp: {
        size_t FixedChildren = Param->getNumChildren();
        while (NodePointer Ty = popNode(Node::Kind::Type)) {
          assert(ParamKind == FunctionSigSpecializationParamKind::ClosureProp);
          Param = addChild(Param, Ty);
        }
        NodePointer Name = popNode(Node::Kind::Identifier);
        if (!Name)
          return nullptr;
        StringRef Text = Name->getText();
        if (ParamKind ==
              FunctionSigSpecializationParamKind::ConstantPropString &&
            Text.size() > 0 && Text[0] == '_') {
          // A '_' escapes a leading digit or '_' of a string constant.
          Text = Text.drop_front(1);
        }
        addChild(Param, createNodeWithAllocatedText(
          Node::Kind::FunctionSignatureSpecializationParamPayload, Text));
        Param->reverseChildren(FixedChildren);
        break;
      }
      default:
        break;
    }
  }
  return Spec;
}

NodePointer Demangler::demangleFuncSpecParam(Node::IndexType ParamIdx) {
  NodePointer Param = createNode(
            Node::Kind::FunctionSignatureSpecializationParam, ParamIdx);
  switch (nextChar()) {
    case 'n':
      return Param;
    case 'c':
      // Consumes an identifier and multiple type parameters.
      // The parameters will be added later.
      return addChild(Param, createNode(
        Node::Kind::FunctionSignatureSpecializationParamKind,
        uint64_t(FunctionSigSpecializationParamKind::ClosureProp)));
    case 'p': {
      switch (nextChar()) {
        case 'f':
          // Consumes an identifier parameter, which will be added later.
          return addChild(
              Param,
              createNode(Node::Kind::FunctionSignatureSpecializationParamKind,
                         Node::IndexType(FunctionSigSpecializationParamKind::
                                             ConstantPropFunction)));
        case 'g':
          // Consumes an identifier parameter, which will be added later.
          return addChild(
              Param,
              createNode(
                  Node::Kind::FunctionSignatureSpecializationParamKind,
                  Node::IndexType(
                      FunctionSigSpecializationParamKind::ConstantPropGlobal)));
        case 'i':
          return addFuncSpecParamNumber(Param,
                    FunctionSigSpecializationParamKind::ConstantPropInteger);
        case 'd':
          return addFuncSpecParamNumber(Param,
                      FunctionSigSpecializationParamKind::ConstantPropFloat);
        case 's': {
          // Consumes an identifier parameter (the string constant),
          // which will be added later.
          const char *Encoding = nullptr;
          switch (nextChar()) {
            case 'b': Encoding = "u8"; break;
            case 'w': Encoding = "u16"; break;
            case 'c': Encoding = "objc"; break;
            default: return nullptr;
          }
          addChild(Param,
                   createNode(
                       Node::Kind::FunctionSignatureSpecializationParamKind,
                       Node::IndexType(
                           swift::Demangle::FunctionSigSpecializationParamKind::
                               ConstantPropString)));
          return addChild(Param, createNode(
                  Node::Kind::FunctionSignatureSpecializationParamPayload,
                  Encoding));
        }
        default:
          return nullptr;
      }
    }
    case 'd': {
      unsigned Value = unsigned(FunctionSigSpecializationParamKind::Dead);
      if (nextIf('G'))
        Value |= unsigned(FunctionSigSpecializationParamKind::OwnedToGuaranteed);
      if (nextIf('X'))
        Value |= unsigned(FunctionSigSpecializationParamKind::SROA);
      return addChild(Param, createNode(
                  Node::Kind::FunctionSignatureSpecializationParamKind, Value));
    }
    case 'g': {
      unsigned Value = unsigned(FunctionSigSpecializationParamKind::
                                OwnedToGuaranteed);
      if (nextIf('X'))
        Value |= unsigned(FunctionSigSpecializationParamKind::SROA);
      return addChild(Param, createNode(
                  Node::Kind::FunctionSignatureSpecializationParamKind, Value));
    }
    case 'x':
      return addChild(Param, createNode(
                Node::Kind::FunctionSignatureSpecializationParamKind,
                unsigned(FunctionSigSpecializationParamKind::SROA)));
    case 'i':
      return addChild(Param, createNode(
                Node::Kind::FunctionSignatureSpecializationParamKind,
                unsigned(FunctionSigSpecializationParamKind::BoxToValue)));
    case 's':
      return addChild(Param, createNode(
                Node::Kind::FunctionSignatureSpecializationParamKind,
                unsigned(FunctionSigSpecializationParamKind::BoxToStack)));
    default:
      return nullptr;
  }
}

NodePointer Demangler::addFuncSpecParamNumber(NodePointer Param,
                                    FunctionSigSpecializationParamKind Kind) {
  Param->addChild(createNode(
        Node::Kind::FunctionSignatureSpecializationParamKind, unsigned(Kind)),
        *this);
  CharVector Str;
  while (isDigit(peekChar())) {
    Str.push_back(nextChar(), *this);
  }
  if (Str.empty())
    return nullptr;
  return addChild(Param, createNode(
     Node::Kind::FunctionSignatureSpecializationParamPayload, Str));
}

NodePointer Demangler::demangleSpecAttributes(Node::Kind SpecKind,
                                              bool demangleUniqueID) {
  bool isFragile = nextIf('q');

  int PassID = (int)nextChar() - '0';
  if (PassID < 0 || PassID > 9)
    return nullptr;

  int Idx = -1;
  if (demangleUniqueID)
    Idx = demangleNatural();

  NodePointer SpecNd = nullptr;
  if (Idx >= 0) {
    SpecNd = createNode(SpecKind, Idx);
  } else {
    SpecNd = createNode(SpecKind);
  }
  if (isFragile)
    SpecNd->addChild(createNode(Node::Kind::SpecializationIsFragile),
                     *this);

  SpecNd->addChild(createNode(Node::Kind::SpecializationPassID, PassID),
                   *this);
  return SpecNd;
}

NodePointer Demangler::demangleWitness() {
  switch (nextChar()) {
    case 'V':
      return createWithChild(Node::Kind::ValueWitnessTable,
                             popNode(Node::Kind::Type));
    case 'o':
      return createWithChild(Node::Kind::WitnessTableOffset, popNode(isEntity));
    case 'v': {
      unsigned Directness;
      switch (nextChar()) {
        case 'd': Directness = unsigned(Directness::Direct); break;
        case 'i': Directness = unsigned(Directness::Indirect); break;
        default: return nullptr;
      }
      return createWithChildren(Node::Kind::FieldOffset,
                        createNode(Node::Kind::Directness, Directness),
                        popNode(isEntity));
    }
    case 'P':
      return createWithChild(Node::Kind::ProtocolWitnessTable,
                             popProtocolConformance());
    case 'G':
      return createWithChild(Node::Kind::GenericProtocolWitnessTable,
                             popProtocolConformance());
    case 'I':
      return createWithChild(
                  Node::Kind::GenericProtocolWitnessTableInstantiationFunction,
                  popProtocolConformance());
    case 'l': {
      NodePointer Conf = popProtocolConformance();
      NodePointer Type = popNode(Node::Kind::Type);
      return createWithChildren(Node::Kind::LazyProtocolWitnessTableAccessor,
                                Type, Conf);
    }
    case 'L': {
      NodePointer Conf = popProtocolConformance();
      NodePointer Type = popNode(Node::Kind::Type);
      return createWithChildren(
               Node::Kind::LazyProtocolWitnessTableCacheVariable, Type, Conf);
    }
    case 'a':
      return createWithChild(Node::Kind::ProtocolWitnessTableAccessor,
                             popProtocolConformance());
    case 't': {
      NodePointer Name = popNode(isDeclName);
      NodePointer Conf = popProtocolConformance();
      return createWithChildren(Node::Kind::AssociatedTypeMetadataAccessor,
                                Conf, Name);
    }
    case 'T': {
      NodePointer ProtoTy = popNode(Node::Kind::Type);
      NodePointer Name = popNode(isDeclName);
      NodePointer Conf = popProtocolConformance();
      return createWithChildren(Node::Kind::AssociatedTypeWitnessTableAccessor,
                                Conf, Name, ProtoTy);
    }
    case 'y': {
      return createWithChild(Node::Kind::OutlinedCopy,
                             popNode(Node::Kind::Type));
    }
    case 'e': {
      return createWithChild(Node::Kind::OutlinedConsume,
                             popNode(Node::Kind::Type));
    }
    default:
      return nullptr;
  }
}

NodePointer Demangler::demangleSpecialType() {
  switch (auto specialChar = nextChar()) {
    case 'f':
      return popFunctionType(Node::Kind::ThinFunctionType);
    case 'K':
      return popFunctionType(Node::Kind::AutoClosureType);
    case 'U':
      return popFunctionType(Node::Kind::UncurriedFunctionType);
    case 'B':
      return popFunctionType(Node::Kind::ObjCBlock);
    case 'C':
      return popFunctionType(Node::Kind::CFunctionPointer);
    case 'o':
      return createType(createWithChild(Node::Kind::Unowned,
                                        popNode(Node::Kind::Type)));
    case 'u':
      return createType(createWithChild(Node::Kind::Unmanaged,
                                        popNode(Node::Kind::Type)));
    case 'w':
      return createType(createWithChild(Node::Kind::Weak,
                                        popNode(Node::Kind::Type)));
    case 'b':
      return createType(createWithChild(Node::Kind::SILBoxType,
                                        popNode(Node::Kind::Type)));
    case 'D':
      return createType(createWithChild(Node::Kind::DynamicSelf,
                                        popNode(Node::Kind::Type)));
    case 'M': {
      NodePointer MTR = demangleMetatypeRepresentation();
      NodePointer Type = popNode(Node::Kind::Type);
      return createType(createWithChildren(Node::Kind::Metatype, MTR, Type));
    }
    case 'm': {
      NodePointer MTR = demangleMetatypeRepresentation();
      NodePointer Type = popNode(Node::Kind::Type);
      return createType(createWithChildren(Node::Kind::ExistentialMetatype,
                                           MTR, Type));
    }
    case 'p':
      return createType(createWithChild(Node::Kind::ExistentialMetatype,
                                        popNode(Node::Kind::Type)));
    case 'X':
    case 'x': {
      // SIL box types.
      NodePointer signature = nullptr, genericArgs = nullptr;
      if (specialChar == 'X') {
        signature = popNode(Node::Kind::DependentGenericSignature);
        if (!signature)
          return nullptr;
        genericArgs = popTypeList();
        if (!genericArgs)
          return nullptr;
      }
      
      auto fieldTypes = popTypeList();
      if (!fieldTypes)
        return nullptr;
      // Build layout.
      auto layout = createNode(Node::Kind::SILBoxLayout);
      for (unsigned i = 0, e = fieldTypes->getNumChildren(); i < e; ++i) {
        auto fieldType = fieldTypes->getChild(i);
        assert(fieldType->getKind() == Node::Kind::Type);
        bool isMutable = false;
        // 'inout' typelist mangling is used to represent mutable fields.
        if (fieldType->getChild(0)->getKind() == Node::Kind::InOut) {
          isMutable = true;
          fieldType = createType(fieldType->getChild(0)->getChild(0));
        }
        auto field = createNode(isMutable
                                         ? Node::Kind::SILBoxMutableField
                                         : Node::Kind::SILBoxImmutableField);
        field->addChild(fieldType, *this);
        layout->addChild(field, *this);
      }
      auto boxTy = createNode(Node::Kind::SILBoxTypeWithLayout);
      boxTy->addChild(layout, *this);
      if (signature) {
        boxTy->addChild(signature, *this);
        assert(genericArgs);
        boxTy->addChild(genericArgs, *this);
      }
      return createType(boxTy);
    }
    case 'e':
      return createType(createNode(Node::Kind::ErrorType, StringRef()));
    default:
      return nullptr;
  }
}

NodePointer Demangler::demangleMetatypeRepresentation() {
  switch (nextChar()) {
    case 't':
      return createNode(Node::Kind::MetatypeRepresentation, "@thin");
    case 'T':
      return createNode(Node::Kind::MetatypeRepresentation, "@thick");
    case 'o':
      return createNode(Node::Kind::MetatypeRepresentation,
                                 "@objc_metatype");
    default:
      return nullptr;
  }
}

NodePointer Demangler::demangleFunctionEntity() {
  enum { None, Type, TypeAndName, TypeAndIndex, Index } Args;

  Node::Kind Kind = Node::Kind::EmptyList;
  switch (nextChar()) {
    case 'D': Args = None; Kind = Node::Kind::Deallocator; break;
    case 'd': Args = None; Kind = Node::Kind::Destructor; break;
    case 'E': Args = None; Kind = Node::Kind::IVarDestroyer; break;
    case 'e': Args = None; Kind = Node::Kind::IVarInitializer; break;
    case 'i': Args = None; Kind = Node::Kind::Initializer; break;
    case 'C': Args = Type; Kind = Node::Kind::Allocator; break;
    case 'c': Args = Type; Kind = Node::Kind::Constructor; break;
    case 'g': Args = TypeAndName; Kind = Node::Kind::Getter; break;
    case 'G': Args = TypeAndName; Kind = Node::Kind::GlobalGetter; break;
    case 's': Args = TypeAndName; Kind = Node::Kind::Setter; break;
    case 'm': Args = TypeAndName; Kind = Node::Kind::MaterializeForSet; break;
    case 'w': Args = TypeAndName; Kind = Node::Kind::WillSet; break;
    case 'W': Args = TypeAndName; Kind = Node::Kind::DidSet; break;
    case 'a':
      Args = TypeAndName;
      switch (nextChar()) {
        case 'O': Kind = Node::Kind::OwningMutableAddressor; break;
        case 'o': Kind = Node::Kind::NativeOwningMutableAddressor; break;
        case 'P': Kind = Node::Kind::NativePinningMutableAddressor; break;
        case 'u': Kind = Node::Kind::UnsafeMutableAddressor; break;
        default: return nullptr;
      }
      break;
    case 'l':
      Args = TypeAndName;
      switch (nextChar()) {
        case 'O': Kind = Node::Kind::OwningAddressor; break;
        case 'o': Kind = Node::Kind::NativeOwningAddressor; break;
        case 'p': Kind = Node::Kind::NativePinningAddressor; break;
        case 'u': Kind = Node::Kind::UnsafeAddressor; break;
        default: return nullptr;
      }
      break;
    case 'U': Args = TypeAndIndex; Kind = Node::Kind::ExplicitClosure; break;
    case 'u': Args = TypeAndIndex; Kind = Node::Kind::ImplicitClosure; break;
    case 'A': Args = Index; Kind = Node::Kind::DefaultArgumentInitializer; break;
    case 'p': return demangleEntity(Node::Kind::GenericTypeParamDecl);
    default: return nullptr;
  }

  NodePointer Child1 = nullptr, Child2 = nullptr;
  switch (Args) {
    case None:
      break;
    case Type:
      Child1 = popNode(Node::Kind::Type);
      break;
    case TypeAndName:
      Child2 = popNode(Node::Kind::Type);
      Child1 = popNode(isDeclName);
      break;
    case TypeAndIndex:
      Child1 = demangleIndexAsNode();
      Child2 = popNode(Node::Kind::Type);
      break;
    case Index:
      Child1 = demangleIndexAsNode();
      break;
  }
  NodePointer Entity = createWithChild(Kind, popContext());
  switch (Args) {
    case None:
      break;
    case Type:
    case Index:
      Entity = addChild(Entity, Child1);
      break;
    case TypeAndName:
    case TypeAndIndex:
      Entity = addChild(Entity, Child1);
      Entity = addChild(Entity, Child2);
      break;
  }
  return Entity;
}

NodePointer Demangler::demangleEntity(Node::Kind Kind) {
  NodePointer Type = popNode(Node::Kind::Type);
  NodePointer Name = popNode(isDeclName);
  NodePointer Context = popContext();
  return createWithChildren(Kind, Context, Name, Type);
}

NodePointer Demangler::demangleProtocolListType() {
  NodePointer TypeList = createNode(Node::Kind::TypeList);
  NodePointer ProtoList = createWithChild(Node::Kind::ProtocolList, TypeList);
  if (!popNode(Node::Kind::EmptyList)) {
    bool firstElem = false;
    do {
      firstElem = (popNode(Node::Kind::FirstElementMarker) != nullptr);
      NodePointer Proto = popProtocol();
      if (!Proto)
        return nullptr;
      TypeList->addChild(Proto, *this);
    } while (!firstElem);

    TypeList->reverseChildren();
  }
  return createType(ProtoList);
}

NodePointer Demangler::demangleGenericSignature(bool hasParamCounts) {
  NodePointer Sig = createNode(Node::Kind::DependentGenericSignature);
  if (hasParamCounts) {
    while (!nextIf('l')) {
      int count = 0;
      if (!nextIf('z'))
        count = demangleIndex() + 1;
      if (count < 0)
        return nullptr;
      Sig->addChild(createNode(Node::Kind::DependentGenericParamCount,
                                        count), *this);
    }
  } else {
    Sig->addChild(createNode(Node::Kind::DependentGenericParamCount, 1),
                  *this);
  }
  if (Sig->getNumChildren() == 0)
    return nullptr;
  size_t NumCounts = Sig->getNumChildren();
  while (NodePointer Req = popNode(isRequirement)) {
    Sig->addChild(Req, *this);
  }
  Sig->reverseChildren(NumCounts);
  return Sig;
}

NodePointer Demangler::demangleGenericRequirement() {
  
  enum { Generic, Assoc, CompoundAssoc, Substitution } TypeKind;
  enum { Protocol, BaseClass, SameType, Layout } ConstraintKind;
  
  switch (nextChar()) {
    case 'c': ConstraintKind = BaseClass; TypeKind = Assoc; break;
    case 'C': ConstraintKind = BaseClass; TypeKind = CompoundAssoc; break;
    case 'b': ConstraintKind = BaseClass; TypeKind = Generic; break;
    case 'B': ConstraintKind = BaseClass; TypeKind = Substitution; break;
    case 't': ConstraintKind = SameType; TypeKind = Assoc; break;
    case 'T': ConstraintKind = SameType; TypeKind = CompoundAssoc; break;
    case 's': ConstraintKind = SameType; TypeKind = Generic; break;
    case 'S': ConstraintKind = SameType; TypeKind = Substitution; break;
    case 'm': ConstraintKind = Layout; TypeKind = Assoc; break;
    case 'M': ConstraintKind = Layout; TypeKind = CompoundAssoc; break;
    case 'l': ConstraintKind = Layout; TypeKind = Generic; break;
    case 'L': ConstraintKind = Layout; TypeKind = Substitution; break;
    case 'p': ConstraintKind = Protocol; TypeKind = Assoc; break;
    case 'P': ConstraintKind = Protocol; TypeKind = CompoundAssoc; break;
    case 'Q': ConstraintKind = Protocol; TypeKind = Substitution; break;
    default:  ConstraintKind = Protocol; TypeKind = Generic; pushBack(); break;
  }

  NodePointer ConstrTy = nullptr;

  switch (TypeKind) {
  case Generic:
    ConstrTy = createType(demangleGenericParamIndex());
    break;
  case Assoc:
    ConstrTy = demangleAssociatedTypeSimple(demangleGenericParamIndex());
    addSubstitution(ConstrTy);
    break;
  case CompoundAssoc:
    ConstrTy = demangleAssociatedTypeCompound(demangleGenericParamIndex());
    addSubstitution(ConstrTy);
    break;
  case Substitution:
    ConstrTy = popNode(Node::Kind::Type);
    break;
  }

  switch (ConstraintKind) {
  case Protocol:
    return createWithChildren(
        Node::Kind::DependentGenericConformanceRequirement, ConstrTy,
        popProtocol());
  case BaseClass:
    return createWithChildren(
        Node::Kind::DependentGenericConformanceRequirement, ConstrTy,
        popNode(Node::Kind::Type));
  case SameType:
    return createWithChildren(Node::Kind::DependentGenericSameTypeRequirement,
                              ConstrTy, popNode(Node::Kind::Type));
  case Layout: {
    auto c = nextChar();
    NodePointer size = nullptr;
    NodePointer alignment = nullptr;
    const char *name = nullptr;
    if (c == 'U') {
      name = "U";
    } else if (c == 'R') {
      name = "R";
    } else if (c == 'N') {
      name = "N";
    } else if (c == 'T') {
      name = "T";
    } else if (c == 'E') {
      size = demangleIndexAsNode();
      if (!size)
        return nullptr;
      alignment = demangleIndexAsNode();
      name = "E";
    } else if (c == 'e') {
      size = demangleIndexAsNode();
      if (!size)
        return nullptr;
      name = "e";
    } else if (c == 'M') {
      size = demangleIndexAsNode();
      if (!size)
        return nullptr;
      alignment = demangleIndexAsNode();
      name = "M";
    } else if (c == 'm') {
      size = demangleIndexAsNode();
      if (!size)
        return nullptr;
      name = "m";
    } else {
      demangler_unreachable("Unknown layout constraint");
    }

    auto NameNode = createNode(Node::Kind::Identifier, name);
    auto LayoutRequirement = createWithChildren(
        Node::Kind::DependentGenericLayoutRequirement, ConstrTy, NameNode);
    if (size)
      LayoutRequirement->addChild(size, *this);
    if (alignment)
      LayoutRequirement->addChild(alignment, *this);
    return LayoutRequirement;
  }
  }

  demangler_unreachable("Unhandled TypeKind in switch.");
}

NodePointer Demangler::demangleGenericType() {
  NodePointer GenSig = popNode(Node::Kind::DependentGenericSignature);
  NodePointer Ty = popNode(Node::Kind::Type);
  return createType(createWithChildren(Node::Kind::DependentGenericType,
                                       GenSig, Ty));
}

static int decodeValueWitnessKind(StringRef CodeStr) {
#define VALUE_WITNESS(MANGLING, NAME) \
  if (CodeStr == #MANGLING) return (int)ValueWitnessKind::NAME;
#include "swift/Demangling/ValueWitnessMangling.def"
  return -1;
}

NodePointer Demangler::demangleValueWitness() {
  char Code[2];
  Code[0] = nextChar();
  Code[1] = nextChar();
  int Kind = decodeValueWitnessKind(StringRef(Code, 2));
  if (Kind < 0)
    return nullptr;
  NodePointer VW = createNode(Node::Kind::ValueWitness, unsigned(Kind));
  return addChild(VW, popNode(Node::Kind::Type));
}

NodePointer Demangler::demangleObjCTypeName() {
  NodePointer Ty = createNode(Node::Kind::Type);
  NodePointer Global = addChild(createNode(Node::Kind::Global),
                         addChild(createNode(Node::Kind::TypeMangling), Ty));
  NodePointer Nominal = nullptr;
  bool isProto = false;
  if (nextIf('C')) {
    Nominal = createNode(Node::Kind::Class);
    addChild(Ty, Nominal);
  } else if (nextIf('P')) {
    isProto = true;
    Nominal = createNode(Node::Kind::Protocol);
    addChild(Ty, addChild(createNode(Node::Kind::ProtocolList),
                    addChild(createNode(Node::Kind::TypeList),
                      addChild(createNode(Node::Kind::Type), Nominal))));
  } else {
    return nullptr;
  }

  if (nextIf('s')) {
    Nominal->addChild(createNode(Node::Kind::Module, "Swift"), *this);
  } else {
    NodePointer Module = demangleIdentifier();
    if (!Module)
      return nullptr;
    Nominal->addChild(changeKind(Module, Node::Kind::Module), *this);
  }

  NodePointer Ident = demangleIdentifier();
  if (!Ident)
    return nullptr;
  Nominal->addChild(Ident, *this);

  if (isProto && !nextIf('_'))
    return nullptr;

  if (Pos < Text.size())
    return nullptr;

  return Global;
}

} // end namespace Mangle
} // end namespace swift

