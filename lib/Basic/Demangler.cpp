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

#include "swift/Basic/Demangler.h"
#include "swift/Basic/ManglingUtils.h"
#include "swift/Basic/ManglingMacros.h"
#include "swift/Basic/Punycode.h"
#include "swift/Basic/Range.h"
#include "swift/Strings.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Compiler.h"

using namespace swift;
using swift::Demangle::FunctionSigSpecializationParamKind;

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
#include "swift/Basic/DemangleNodes.def"
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
// Context member functions     //
//////////////////////////////////

// TODO: these functions are just dummy implementations. After lldb has switched
// to the new Context API, implement the Context (bumpptr allocation based
// demangling).

Context::Context() {
}

Context::~Context() {
}

void Context::clear() {
}

NodePointer Context::demangleSymbolAsNode(llvm::StringRef MangledName) {
  return ::demangleSymbolAsNode(MangledName.data(), MangledName.size());
}

NodePointer Context::demangleTypeAsNode(llvm::StringRef MangledName) {
  return ::demangleTypeAsNode(MangledName.data(), MangledName.size());
}

std::string Context::demangleSymbolAsString(llvm::StringRef MangledName,
                                            const DemangleOptions &Options) {
  return ::demangleSymbolAsString(MangledName.data(), MangledName.size(),
                                  Options);
}

std::string Context::demangleTypeAsString(llvm::StringRef MangledName,
                                          const DemangleOptions &Options) {
  return ::demangleTypeAsString(MangledName.data(), MangledName.size(),
                                Options);
}

bool Context::isThunkSymbol(llvm::StringRef MangledName) {
  return ::isThunkSymbol(MangledName.data(), MangledName.size());
}
  
NodePointer Context::createNode(Node::Kind K) {
  return NodeFactory::create(K);
}

NodePointer Context::createNode(Node::Kind K, Node::IndexType Index) {
  return NodeFactory::create(K, Index);
}

NodePointer Context::createNode(Node::Kind K, llvm::StringRef Text) {
  return NodeFactory::create(K, Text);
}

//////////////////////////////////
// Node member functions        //
//////////////////////////////////

void Node::addChild(NodePointer Child, NodeFactory &Factory) {
  addChild(Child);
}

void Node::addChild(NodePointer Child, Context &Ctx) {
  addChild(Child);
}

} // namespace Demangle

namespace NewMangling {

//////////////////////////////////
// Demangler member functions   //
//////////////////////////////////

NodePointer Demangler::demangleTopLevel() {
  if (!nextIf(MANGLING_PREFIX_STR))
    return nullptr;

  NodePointer topLevel = NodeFactory::create(Node::Kind::Global);

  parseAndPushNodes();

  // Let a trailing '_' be part of the not demangled suffix.
  popNode(Node::Kind::FirstElementMarker);

  size_t EndPos = (NodeStack.empty() ? 0 : NodeStack.back().Pos);

  NodePointer Parent = topLevel;
  while (NodePointer FuncAttr = popNode(isFunctionAttr)) {
    Parent->addChild(FuncAttr);
    if (FuncAttr->getKind() == Node::Kind::PartialApplyForwarder ||
        FuncAttr->getKind() == Node::Kind::PartialApplyObjCForwarder)
      Parent = FuncAttr;
  }
  for (const NodeWithPos &NWP : NodeStack) {
    NodePointer Nd = NWP.Node;
    switch (Nd->getKind()) {
      case Node::Kind::Type:
        Parent->addChild(Nd->getFirstChild());
        break;
      case Node::Kind::Identifier:
        if (StringRef(Nd->getText()).startswith("_T")) {
          NodePointer Global = demangleSymbolAsNode(Nd->getText());
          if (Global && Global->getKind() == Node::Kind::Global) {
            for (NodePointer Child : *Global) {
              Parent->addChild(Child);
            }
            break;
          }
        }
        LLVM_FALLTHROUGH;
      default:
        Parent->addChild(Nd);
        break;
    }
  }
  if (EndPos < Text.size()) {
    topLevel->addChild(NodeFactory::create(Node::Kind::Suffix,
                                           Text.substr(EndPos)));
  }

  return topLevel;
}

NodePointer Demangler::demangleType() {
  parseAndPushNodes();

  if (NodePointer Result = popNode())
    return Result;

  return NodeFactory::create(Node::Kind::Suffix, Text);
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

NodePointer Demangler::changeKind(NodePointer Node, Node::Kind NewKind) {
  if (!Node)
    return nullptr;
  NodePointer NewNode;
  if (Node->hasText()) {
    NewNode = NodeFactory::create(NewKind, Node->getText());
  } else if (Node->hasIndex()) {
    NewNode = NodeFactory::create(NewKind, Node->getIndex());
  } else {
    NewNode = NodeFactory::create(NewKind);
  }
  for (NodePointer Child : *Node) {
    NewNode->addChild(Child);
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
    case 'K': return NodeFactory::create(Node::Kind::ThrowsAnnotation);
    case 'L': return demangleLocalIdentifier();
    case 'M': return demangleMetatype();
    case 'N': return createWithChild(Node::Kind::TypeMetadata,
                                     popNode(Node::Kind::Type));
    case 'O': return demangleNominalType(Node::Kind::Enum);
    case 'P': return demangleNominalType(Node::Kind::Protocol);
    case 'Q': return demangleArchetype();
    case 'R': return demangleGenericRequirement();
    case 'S': return demangleKnownType();
    case 'T': return demangleThunkOrSpecialization();
    case 'V': return demangleNominalType(Node::Kind::Structure);
    case 'W': return demangleWitness();
    case 'X': return demangleSpecialType();
    case 'Z': return createWithChild(Node::Kind::Static, popNode(isEntity));
    case 'a': return demangleTypeAlias();
    case 'c': return popFunctionType(Node::Kind::FunctionType);
    case 'd': return NodeFactory::create(Node::Kind::VariadicMarker);
    case 'f': return demangleFunctionEntity();
    case 'i': return demangleEntity(Node::Kind::Subscript);
    case 'l': return demangleGenericSignature(/*hasParamCounts*/ false);
    case 'm': return createType(createWithChild(Node::Kind::Metatype,
                                                popNode(Node::Kind::Type)));
    case 'o': return demangleOperatorIdentifier();
    case 'p': return demangleProtocolListType();
    case 'q': return createType(demangleGenericParamIndex());
    case 'r': return demangleGenericSignature(/*hasParamCounts*/ true);
    case 's': return NodeFactory::create(Node::Kind::Module, STDLIB_NAME);
    case 't': return popTuple();
    case 'u': return demangleGenericType();
    case 'v': return demangleEntity(Node::Kind::Variable);
    case 'w': return demangleValueWitness();
    case 'x': return createType(getDependentGenericParamType(0, 0));
    case 'y': return NodeFactory::create(Node::Kind::EmptyList);
    case 'z': return createType(createWithChild(Node::Kind::InOut,
                                                popTypeAndGetChild()));
    case '_': return NodeFactory::create(Node::Kind::FirstElementMarker);
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
    return NodeFactory::create(Node::Kind::Number, Idx);
  return nullptr;
}

NodePointer Demangler::demangleMultiSubstitutions() {
  while (true) {
    char c = nextChar();
    if (isLowerLetter(c)) {
      unsigned Idx = c - 'a';
      if (Idx >= Substitutions.size())
        return nullptr;
      pushNode(Substitutions[Idx]);
      continue;
    }
    if (isUpperLetter(c)) {
      unsigned Idx = c - 'A';
      if (Idx >= Substitutions.size())
        return nullptr;
      return Substitutions[Idx];
    }
    pushBack();
    unsigned Idx = demangleIndex() + 26;
    if (Idx >= Substitutions.size())
      return nullptr;
    return Substitutions[Idx];
  }
}

NodePointer Demangler::createSwiftType(Node::Kind typeKind, StringRef name) {
  return createType(createWithChildren(typeKind,
    NodeFactory::create(Node::Kind::Module, STDLIB_NAME),
    NodeFactory::create(Node::Kind::Identifier, name)));
}

NodePointer Demangler::demangleKnownType() {
  switch (nextChar()) {
    case 'o':
      return NodeFactory::create(Node::Kind::Module, MANGLING_MODULE_OBJC);
    case 'C':
      return NodeFactory::create(Node::Kind::Module, MANGLING_MODULE_C);
    case 'a':
      return createSwiftType(Node::Kind::Structure, "Array");
    case 'b':
      return createSwiftType(Node::Kind::Structure, "Bool");
    case 'c':
      return createSwiftType(Node::Kind::Structure, "UnicodeScalar");
    case 'd':
      return createSwiftType(Node::Kind::Structure, "Double");
    case 'f':
      return createSwiftType(Node::Kind::Structure, "Float");
    case 'i':
      return createSwiftType(Node::Kind::Structure, "Int");
    case 'V':
      return createSwiftType(Node::Kind::Structure, "UnsafeRawPointer");
    case 'v':
      return createSwiftType(Node::Kind::Structure, "UnsafeMutableRawPointer");
    case 'P':
      return createSwiftType(Node::Kind::Structure, "UnsafePointer");
    case 'p':
      return createSwiftType(Node::Kind::Structure, "UnsafeMutablePointer");
    case 'q':
      return createSwiftType(Node::Kind::Enum, "Optional");
    case 'Q':
      return createSwiftType(Node::Kind::Enum, "ImplicitlyUnwrappedOptional");
    case 'R':
      return createSwiftType(Node::Kind::Structure, "UnsafeBufferPointer");
    case 'r':
      return createSwiftType(Node::Kind::Structure, "UnsafeMutableBufferPointer");
    case 'S':
      return createSwiftType(Node::Kind::Structure, "String");
    case 'u':
      return createSwiftType(Node::Kind::Structure, "UInt");
    case 'g':
      return createType(createWithChildren(Node::Kind::BoundGenericEnum,
              createSwiftType(Node::Kind::Enum, "Optional"),
              createWithChild(Node::Kind::TypeList, popNode(Node::Kind::Type))));
    default:
      return nullptr;
  }
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
  std::string Identifier;
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
      if (WordIdx >= (int)Words.size())
        return nullptr;
      StringRef Slice = Words[WordIdx];
      Identifier.append(Slice.data(), Slice.size());
    }
    if (nextIf('0'))
      break;
    int numChars = demangleNatural();
    if (numChars <= 0)
      return nullptr;
    if (isPunycoded)
      nextIf('_');
    if (Pos + numChars >= Text.size())
      return nullptr;
    StringRef Slice = StringRef(Text.data() + Pos, numChars);
    if (isPunycoded) {
      if (!Punycode::decodePunycodeUTF8(Slice, Identifier))
        return nullptr;
    } else {
      Identifier.append(Slice.data(), Slice.size());
      int wordStartPos = -1;
      for (int Idx = 0, End = (int)Slice.size(); Idx <= End; ++Idx) {
        char c = (Idx < End ? Slice[Idx] : 0);
        if (wordStartPos >= 0 && isWordEnd(c, Slice[Idx - 1])) {
          if (Idx - wordStartPos >= 2) {
            StringRef word(Slice.begin() + wordStartPos, Idx - wordStartPos);
            Words.push_back(word);
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
  NodePointer Ident = NodeFactory::create(Node::Kind::Identifier, Identifier);
  addSubstitution(Ident);
  return Ident;
}

NodePointer Demangler::demangleOperatorIdentifier() {
  NodePointer Ident = popNode(Node::Kind::Identifier);

  static const char op_char_table[] = "& @/= >    <*!|+?%-~   ^ .";

  std::string OpStr;
  OpStr.reserve(Ident->getText().size());
  for (signed char c : Ident->getText()) {
    if (c < 0) {
      // Pass through Unicode characters.
      OpStr.push_back(c);
      continue;
    }
    if (!isLowerLetter(c))
      return nullptr;
    char o = op_char_table[c - 'a'];
    if (o == ' ')
      return nullptr;
    OpStr.push_back(o);
  }
  switch (nextChar()) {
    case 'i': return NodeFactory::create(Node::Kind::InfixOperator, OpStr);
    case 'p': return NodeFactory::create(Node::Kind::PrefixOperator, OpStr);
    case 'P': return NodeFactory::create(Node::Kind::PostfixOperator, OpStr);
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
  NodePointer Ty;
  switch (nextChar()) {
    case 'b':
      Ty = NodeFactory::create(Node::Kind::BuiltinTypeName,
                               "Builtin.BridgeObject");
      break;
    case 'B':
      Ty = NodeFactory::create(Node::Kind::BuiltinTypeName,
                              "Builtin.UnsafeValueBuffer");
      break;
    case 'f': {
      int size = demangleIndex() - 1;
      if (size <= 0)
        return nullptr;
      Ty = NodeFactory::create(Node::Kind::BuiltinTypeName,
               std::move(DemanglerPrinter() << "Builtin.Float" << size).str());
      break;
    }
    case 'i': {
      int size = demangleIndex() - 1;
      if (size <= 0)
        return nullptr;
      Ty = NodeFactory::create(Node::Kind::BuiltinTypeName,
                          (DemanglerPrinter() << "Builtin.Int" << size).str());
      break;
    }
    case 'v': {
      int elts = demangleIndex() - 1;
      if (elts <= 0)
        return nullptr;
      NodePointer EltType = popTypeAndGetChild();
      if (!EltType || EltType->getKind() != Node::Kind::BuiltinTypeName ||
          EltType->getText().find("Builtin.") != 0)
        return nullptr;
      Ty = NodeFactory::create(Node::Kind::BuiltinTypeName,
                      (DemanglerPrinter() << "Builtin.Vec" << elts << "x" <<
                      EltType->getText().substr(sizeof("Builtin.") - 1)).str());
      break;
    }
    case 'O':
      Ty = NodeFactory::create(Node::Kind::BuiltinTypeName,
                               "Builtin.UnknownObject");
      break;
    case 'o':
      Ty = NodeFactory::create(Node::Kind::BuiltinTypeName,
                               "Builtin.NativeObject");
      break;
    case 'p':
      Ty = NodeFactory::create(Node::Kind::BuiltinTypeName,
                               "Builtin.RawPointer");
      break;
    case 'w':
      Ty = NodeFactory::create(Node::Kind::BuiltinTypeName,
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
  NodePointer Func = NodeFactory::create(Node::Kind::Function);
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
  NodePointer FuncType = NodeFactory::create(kind);
  addChild(FuncType, popNode(Node::Kind::ThrowsAnnotation));

  FuncType = addChild(FuncType, popFunctionParams(Node::Kind::ArgumentTuple));
  FuncType = addChild(FuncType, popFunctionParams(Node::Kind::ReturnType));
  return createType(FuncType);
}

NodePointer Demangler::popFunctionParams(Node::Kind kind) {
  NodePointer ParamsType;
  if (popNode(Node::Kind::EmptyList)) {
    ParamsType = createType(NodeFactory::create(Node::Kind::NonVariadicTuple));
  } else {
    ParamsType = popNode(Node::Kind::Type);
  }
  return createWithChild(kind, ParamsType);
}

NodePointer Demangler::popTuple() {
  NodePointer Root = NodeFactory::create(popNode(Node::Kind::VariadicMarker) ?
                                         Node::Kind::VariadicTuple :
                                         Node::Kind::NonVariadicTuple);

  if (!popNode(Node::Kind::EmptyList)) {
    std::vector<NodePointer> Nodes;
    bool firstElem = false;
    do {
      firstElem = (popNode(Node::Kind::FirstElementMarker) != nullptr);
      NodePointer TupleElmt = NodeFactory::create(Node::Kind::TupleElement);
      if (NodePointer Ident = popNode(Node::Kind::Identifier)) {
        TupleElmt->addChild(NodeFactory::create(Node::Kind::TupleElementName,
                                                Ident->getText()));
      }
      NodePointer Ty = popNode(Node::Kind::Type);
      if (!Ty)
        return nullptr;
      TupleElmt->addChild(Ty);
      Nodes.push_back(TupleElmt);
    } while (!firstElem);

    while (NodePointer TupleElmt = pop_back_val(Nodes)) {
      Root->addChild(TupleElmt);
    }
  }
  return createType(Root);
}

NodePointer Demangler::popTypeList() {
  NodePointer Root = NodeFactory::create(Node::Kind::TypeList);

  if (!popNode(Node::Kind::EmptyList)) {
    std::vector<NodePointer> Nodes;
    bool firstElem = false;
    do {
      firstElem = (popNode(Node::Kind::FirstElementMarker) != nullptr);
      NodePointer Ty = popNode(Node::Kind::Type);
      if (!Ty)
        return nullptr;
      Nodes.push_back(Ty);
    } while (!firstElem);
    while (NodePointer Ty = pop_back_val(Nodes)) {
      Root->addChild(Ty);
    }
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
  std::vector<NodePointer> TypeListList;
  std::vector<NodePointer> Types;
  for (;;) {
    NodePointer TList = NodeFactory::create(Node::Kind::TypeList);
    TypeListList.push_back(TList);
    while (NodePointer Ty = popNode(Node::Kind::Type)) {
      Types.push_back(Ty);
    }
    while (NodePointer Ty = pop_back_val(Types)) {
      TList->addChild(Ty);
    }
    if (popNode(Node::Kind::EmptyList))
      break;
    if (!popNode(Node::Kind::FirstElementMarker))
      return nullptr;
  }
  NodePointer Nominal = popTypeAndGetNominal();
  return createType(demangleBoundGenericArgs(Nominal, TypeListList, 0));
}

NodePointer Demangler::demangleBoundGenericArgs(NodePointer Nominal,
                                    const std::vector<NodePointer> &TypeLists,
                                    size_t TypeListIdx) {
  if (!Nominal || Nominal->getNumChildren() < 2)
    return nullptr;

  if (TypeListIdx >= TypeLists.size())
    return nullptr;
  NodePointer args = TypeLists[TypeListIdx];

  // Generic arguments for the outermost type come first.
  NodePointer Context = Nominal->getFirstChild();

  if (Context->getKind() != Node::Kind::Module &&
      Context->getKind() != Node::Kind::Function &&
      Context->getKind() != Node::Kind::Extension) {
    NodePointer BoundParent = demangleBoundGenericArgs(Context, TypeLists,
                                                       TypeListIdx + 1);

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
  StringRef attr;
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
                         NodeFactory::create(Node::Kind::ImplConvention, attr));
}

NodePointer Demangler::demangleImplResultConvention(Node::Kind ConvKind) {
  StringRef attr;
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
                         NodeFactory::create(Node::Kind::ImplConvention, attr));
}

NodePointer Demangler::demangleImplFunctionType() {
  NodePointer type = NodeFactory::create(Node::Kind::ImplFunctionType);

  NodePointer GenSig = popNode(Node::Kind::DependentGenericSignature);
  if (GenSig && nextIf('P'))
    GenSig = changeKind(GenSig, Node::Kind::DependentPseudogenericSignature);

  StringRef CAttr;
  switch (nextChar()) {
    case 'y': CAttr = "@callee_unowned"; break;
    case 'g': CAttr = "@callee_guaranteed"; break;
    case 'x': CAttr = "@callee_owned"; break;
    case 't': CAttr = "@convention(thin)"; break;
    default: return nullptr;
  }
  type->addChild(NodeFactory::create(Node::Kind::ImplConvention, CAttr));

  StringRef FAttr;
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
  if (!FAttr.empty())
    type->addChild(NodeFactory::create(Node::Kind::ImplFunctionAttribute, FAttr));

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
    type->getChild(type->getNumChildren() - Idx - 1)->addChild(ConvTy);
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

static std::string getArchetypeName(Node::IndexType index,
                                    Node::IndexType depth) {
  DemanglerPrinter name;
  do {
    name << (char)('A' + (index % 26));
    index /= 26;
  } while (index);
  if (depth != 0)
    name << depth;
  return std::move(name).str();
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
  std::vector<NodePointer> AssocTyNames;
  bool firstElem = false;
  do {
    firstElem = (popNode(Node::Kind::FirstElementMarker) != nullptr);
    NodePointer AssocTyName = popAssocTypeName();
    if (!AssocTyName)
      return nullptr;
    AssocTyNames.push_back(AssocTyName);
  } while (!firstElem);

  NodePointer Base = GenericParamIdx;

  while (NodePointer AssocTy = pop_back_val(AssocTyNames)) {
    NodePointer depTy = NodeFactory::create(Node::Kind::DependentMemberType);
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
  DemanglerPrinter PrintName;
  PrintName << getArchetypeName(index, depth);
  
  auto paramTy = NodeFactory::create(Node::Kind::DependentGenericParamType,
                                     std::move(PrintName).str());
  paramTy->addChild(NodeFactory::create(Node::Kind::Index, depth));
  paramTy->addChild(NodeFactory::create(Node::Kind::Index, index));
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
  NodePointer Ident;
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
    case 'o': return NodeFactory::create(Node::Kind::ObjCAttribute);
    case 'O': return NodeFactory::create(Node::Kind::NonObjCAttribute);
    case 'D': return NodeFactory::create(Node::Kind::DynamicAttribute);
    case 'd': return NodeFactory::create(Node::Kind::DirectMethodReferenceAttribute);
    case 'V': return NodeFactory::create(Node::Kind::VTableAttribute);
    case 'a': return NodeFactory::create(Node::Kind::PartialApplyObjCForwarder);
    case 'A': return NodeFactory::create(Node::Kind::PartialApplyForwarder);
    case 'W': {
      NodePointer Entity = popNode(isEntity);
      NodePointer Conf = popProtocolConformance();
      return createWithChildren(Node::Kind::ProtocolWitness, Conf, Entity);
    }
    case 'R':
    case 'r': {
      NodePointer Thunk = NodeFactory::create(c == 'R' ?
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
    Spec->addChild(createWithChild(Node::Kind::GenericSpecializationParam, Ty));
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
  return Spec;
}

NodePointer Demangler::demangleFuncSpecParam(Node::IndexType ParamIdx) {
  NodePointer Param = NodeFactory::create(
            Node::Kind::FunctionSignatureSpecializationParam, ParamIdx);
  switch (nextChar()) {
    case 'n':
      return Param;
    case 'c': {
      std::vector<NodePointer> Types;
      while (NodePointer Ty = popNode(Node::Kind::Type)) {
        Types.push_back(Ty);
      }
      Param = addFuncSpecParamIdentifier(Param,
            swift::Demangle::FunctionSigSpecializationParamKind::ClosureProp);
      while (NodePointer Ty = pop_back_val(Types)) {
        Param = addChild(Param, Ty);
      }
      return Param;
    }
    case 'p': {
      switch (nextChar()) {
        case 'f':
          return addFuncSpecParamIdentifier(Param,
                    FunctionSigSpecializationParamKind::ConstantPropFunction);
        case 'g':
          return addFuncSpecParamIdentifier(Param,
                      FunctionSigSpecializationParamKind::ConstantPropGlobal);
        case 'i':
          return addFuncSpecParamNumber(Param,
                    FunctionSigSpecializationParamKind::ConstantPropInteger);
        case 'd':
          return addFuncSpecParamNumber(Param,
                      FunctionSigSpecializationParamKind::ConstantPropFloat);
        case 's': {
          StringRef Encoding;
          switch (nextChar()) {
            case 'b': Encoding = "u8"; break;
            case 'w': Encoding = "u16"; break;
            case 'c': Encoding = "objc"; break;
            default: return nullptr;
          }
          NodePointer Str = popNode(Node::Kind::Identifier);
          if (!Str)
            return nullptr;
          StringRef Text = Str->getText();
          if (Text.size() > 0 && Text[0] == '_')
            Text = Text.drop_front(1);

          Param->addChild(NodeFactory::create(
                  Node::Kind::FunctionSignatureSpecializationParamKind,
                  unsigned(swift::Demangle::FunctionSigSpecializationParamKind::
                           ConstantPropString)));
          Param->addChild(NodeFactory::create(
                  Node::Kind::FunctionSignatureSpecializationParamPayload,
                  Encoding));
          return addChild(Param, NodeFactory::create(
                  Node::Kind::FunctionSignatureSpecializationParamPayload,
                  Text));
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
      return addChild(Param, NodeFactory::create(
                  Node::Kind::FunctionSignatureSpecializationParamKind, Value));
    }
    case 'g': {
      unsigned Value = unsigned(FunctionSigSpecializationParamKind::
                                OwnedToGuaranteed);
      if (nextIf('X'))
        Value |= unsigned(FunctionSigSpecializationParamKind::SROA);
      return addChild(Param, NodeFactory::create(
                  Node::Kind::FunctionSignatureSpecializationParamKind, Value));
    }
    case 'x':
      return addChild(Param, NodeFactory::create(
                Node::Kind::FunctionSignatureSpecializationParamKind,
                unsigned(FunctionSigSpecializationParamKind::SROA)));
    case 'i':
      return addChild(Param, NodeFactory::create(
                Node::Kind::FunctionSignatureSpecializationParamKind,
                unsigned(FunctionSigSpecializationParamKind::BoxToValue)));
    case 's':
      return addChild(Param, NodeFactory::create(
                Node::Kind::FunctionSignatureSpecializationParamKind,
                unsigned(FunctionSigSpecializationParamKind::BoxToStack)));
    default:
      return nullptr;
  }
}

NodePointer Demangler::addFuncSpecParamIdentifier(NodePointer Param,
                                    FunctionSigSpecializationParamKind Kind,
                                    StringRef FirstParam) {
  NodePointer Name = popNode(Node::Kind::Identifier);
  if (!Name)
    return nullptr;
  Param->addChild(NodeFactory::create(
        Node::Kind::FunctionSignatureSpecializationParamKind, unsigned(Kind)));
  if (!FirstParam.empty()) {
    Param->addChild(NodeFactory::create(
          Node::Kind::FunctionSignatureSpecializationParamPayload, FirstParam));
  }
  return addChild(Param, NodeFactory::create(
     Node::Kind::FunctionSignatureSpecializationParamPayload, Name->getText()));
}

NodePointer Demangler::addFuncSpecParamNumber(NodePointer Param,
                                    FunctionSigSpecializationParamKind Kind) {
  Param->addChild(NodeFactory::create(
        Node::Kind::FunctionSignatureSpecializationParamKind, unsigned(Kind)));
  std::string Str;
  while (isDigit(peekChar())) {
    Str += nextChar();
  }
  if (Str.empty())
    return nullptr;
  return addChild(Param, NodeFactory::create(
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

  NodePointer SpecNd;
  if (Idx >= 0) {
    SpecNd = NodeFactory::create(SpecKind, Idx);
  } else {
    SpecNd = NodeFactory::create(SpecKind);
  }
  if (isFragile)
    SpecNd->addChild(NodeFactory::create(Node::Kind::SpecializationIsFragile));

  SpecNd->addChild(NodeFactory::create(Node::Kind::SpecializationPassID,
                                       PassID));
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
                        NodeFactory::create(Node::Kind::Directness, Directness),
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
      NodePointer signature, genericArgs;
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
      auto layout = NodeFactory::create(Node::Kind::SILBoxLayout);
      for (unsigned i = 0, e = fieldTypes->getNumChildren(); i < e; ++i) {
        auto fieldType = fieldTypes->getChild(i);
        assert(fieldType->getKind() == Node::Kind::Type);
        bool isMutable = false;
        // 'inout' typelist mangling is used to represent mutable fields.
        if (fieldType->getChild(0)->getKind() == Node::Kind::InOut) {
          isMutable = true;
          fieldType = createType(fieldType->getChild(0)->getChild(0));
        }
        auto field = NodeFactory::create(isMutable
                                         ? Node::Kind::SILBoxMutableField
                                         : Node::Kind::SILBoxImmutableField);
        field->addChild(fieldType);
        layout->addChild(field);
      }
      auto boxTy = NodeFactory::create(Node::Kind::SILBoxTypeWithLayout);
      boxTy->addChild(layout);
      if (signature) {
        boxTy->addChild(signature);
        assert(genericArgs);
        boxTy->addChild(genericArgs);
      }
      return createType(boxTy);
    }
    case 'e':
      return createType(NodeFactory::create(Node::Kind::ErrorType, std::string()));
    default:
      return nullptr;
  }
}

NodePointer Demangler::demangleMetatypeRepresentation() {
  switch (nextChar()) {
    case 't':
      return NodeFactory::create(Node::Kind::MetatypeRepresentation, "@thin");
    case 'T':
      return NodeFactory::create(Node::Kind::MetatypeRepresentation, "@thick");
    case 'o':
      return NodeFactory::create(Node::Kind::MetatypeRepresentation,
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

  NodePointer Child1, Child2;
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
  NodePointer TypeList = NodeFactory::create(Node::Kind::TypeList);
  NodePointer ProtoList = createWithChild(Node::Kind::ProtocolList, TypeList);
  if (!popNode(Node::Kind::EmptyList)) {
    std::vector<NodePointer> ProtoNames;
    bool firstElem = false;
    do {
      firstElem = (popNode(Node::Kind::FirstElementMarker) != nullptr);
      NodePointer Proto = popProtocol();
      if (!Proto)
        return nullptr;
      ProtoNames.push_back(Proto);
    } while (!firstElem);

    while (NodePointer Proto = pop_back_val(ProtoNames)) {
      TypeList->addChild(Proto);
    }
  }
  return createType(ProtoList);
}

NodePointer Demangler::demangleGenericSignature(bool hasParamCounts) {
  std::vector<NodePointer> Requirements;
  while (NodePointer Req = popNode(isRequirement)) {
    Requirements.push_back(Req);
  }
  NodePointer Sig = NodeFactory::create(Node::Kind::DependentGenericSignature);
  if (hasParamCounts) {
    while (!nextIf('l')) {
      int count = 0;
      if (!nextIf('z'))
        count = demangleIndex() + 1;
      if (count < 0)
        return nullptr;
      Sig->addChild(NodeFactory::create(Node::Kind::DependentGenericParamCount,
                                        count));
    }
  } else {
    Sig->addChild(NodeFactory::create(Node::Kind::DependentGenericParamCount,
                                      1));
  }
  if (Sig->getNumChildren() == 0)
    return nullptr;
  while (NodePointer Req = pop_back_val(Requirements)) {
    Sig->addChild(Req);
  }
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

  NodePointer ConstrTy;

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
    StringRef name;
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

    auto NameNode = NodeFactory::create(Node::Kind::Identifier, name);
    auto LayoutRequirement = createWithChildren(
        Node::Kind::DependentGenericLayoutRequirement, ConstrTy, NameNode);
    if (size)
      LayoutRequirement->addChild(size);
    if (alignment)
      LayoutRequirement->addChild(alignment);
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
#include "swift/Basic/ValueWitnessMangling.def"
  return -1;
}

NodePointer Demangler::demangleValueWitness() {
  char Code[2];
  Code[0] = nextChar();
  Code[1] = nextChar();
  int Kind = decodeValueWitnessKind(StringRef(Code, 2));
  if (Kind < 0)
    return nullptr;
  NodePointer VW = NodeFactory::create(Node::Kind::ValueWitness, unsigned(Kind));
  return addChild(VW, popNode(Node::Kind::Type));
}

} // end namespace NewMangling
} // end namespace swift

