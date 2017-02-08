//===--- Demangler.h - String to Node-Tree Demangling -----------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_DEMANGLER_H
#define SWIFT_BASIC_DEMANGLER_H

#include "swift/Basic/Demangle.h"
#include <vector>

using namespace swift::Demangle;
using llvm::StringRef;

namespace swift {
namespace NewMangling {

class Demangler {
  StringRef Text;
  size_t Pos;

  struct NodeWithPos {
    NodePointer Node;
    size_t Pos;
  };

  std::vector<NodeWithPos> NodeStack;
  std::vector<NodePointer> Substitutions;
  std::vector<unsigned> PendingSubstitutions;
  std::vector<StringRef> Words;

  static NodePointer pop_back_val(std::vector<NodePointer> &NodeVector) {
    if (NodeVector.empty())
      return nullptr;
    NodePointer Val = NodeVector.back();
    NodeVector.pop_back();
    return Val;
  }

  bool nextIf(StringRef str) {
    if (!Text.substr(Pos).startswith(str)) return false;
    Pos += str.size();
    return true;
  }

  char peekChar() {
    if (Pos >= Text.size())
      return 0;
    return Text[Pos];
  }

  char nextChar() {
    if (Pos >= Text.size())
      return 0;
    return Text[Pos++];
  }

  bool nextIf(char c) {
    if (peekChar() != c)
      return false;
    Pos++;
    return true;
  }

  void pushBack() {
    assert(Pos > 0);
    Pos--;
  }

  void pushNode(NodePointer Nd) {
    NodeStack.push_back({ Nd, Pos });
  }

  NodePointer popNode() {
    if (!NodeStack.empty()) {
      NodePointer Val = NodeStack.back().Node;
      NodeStack.pop_back();
      return Val;
    }
    return nullptr;
  }

  NodePointer popNode(Node::Kind kind) {
    if (NodeStack.empty())
      return nullptr;

    Node::Kind NdKind = NodeStack.back().Node->getKind();
    if (NdKind != kind)
      return nullptr;

    return popNode();
  }

  template <typename Pred> NodePointer popNode(Pred pred) {
    if (NodeStack.empty())
      return nullptr;

    Node::Kind NdKind = NodeStack.back().Node->getKind();
    if (!pred(NdKind))
      return nullptr;
    
    return popNode();
  }

public:
  Demangler(llvm::StringRef mangled) : Text(mangled), Pos(0) {}

  NodePointer demangleTopLevel();

  NodePointer demangleType();

private:

  void addSubstitution(NodePointer Nd) {
    if (Nd)
      Substitutions.push_back(Nd);
  }

  static NodePointer addChild(NodePointer Parent, NodePointer Child) {
    if (!Parent || !Child)
      return nullptr;
    Parent->addChild(Child);
    return Parent;
  }

  static NodePointer createWithChild(Node::Kind kind, NodePointer Child) {
    if (!Child)
      return nullptr;
    NodePointer Nd = NodeFactory::create(kind);
    Nd->addChild(Child);
    return Nd;
  }

  static NodePointer createType(NodePointer Child) {
    return createWithChild(Node::Kind::Type, Child);
  }
  
  static NodePointer createWithChildren(Node::Kind kind, NodePointer Child1,
                                        NodePointer Child2) {
    if (!Child1 || !Child2)
      return nullptr;
    NodePointer Nd = NodeFactory::create(kind);
    Nd->addChild(Child1);
    Nd->addChild(Child2);
    return Nd;
  }

  static NodePointer createWithChildren(Node::Kind kind, NodePointer Child1,
                                        NodePointer Child2,
                                        NodePointer Child3) {
    if (!Child1 || !Child2 || !Child3)
      return nullptr;
    NodePointer Nd = NodeFactory::create(kind);
    Nd->addChild(Child1);
    Nd->addChild(Child2);
    Nd->addChild(Child3);
    return Nd;
  }

  NodePointer createWithPoppedType(Node::Kind kind) {
    return createWithChild(kind, popNode(Node::Kind::Type));
  }

  void parseAndPushNodes();

  NodePointer changeKind(NodePointer Node, Node::Kind NewKind);

  NodePointer demangleOperator();

  int demangleNatural();
  int demangleIndex();
  NodePointer demangleIndexAsNode();
  NodePointer demangleIdentifier();
  NodePointer demangleOperatorIdentifier();

  NodePointer demangleMultiSubstitutions();
  static NodePointer createSwiftType(Node::Kind typeKind, StringRef name);
  NodePointer demangleKnownType();
  NodePointer demangleLocalIdentifier();

  NodePointer popModule();
  NodePointer popContext();
  NodePointer popTypeAndGetChild();
  NodePointer popTypeAndGetNominal();
  NodePointer demangleBuiltinType();
  NodePointer demangleNominalType(Node::Kind kind);
  NodePointer demangleTypeAlias();
  NodePointer demangleExtensionContext();
  NodePointer demanglePlainFunction();
  NodePointer popFunctionType(Node::Kind kind);
  NodePointer popFunctionParams(Node::Kind kind);
  NodePointer popTuple();
  NodePointer popTypeList();
  NodePointer popProtocol();
  NodePointer demangleBoundGenericType();
  NodePointer demangleBoundGenericArgs(NodePointer nominalType,
                                    const std::vector<NodePointer> &TypeLists,
                                    size_t TypeListIdx);
  NodePointer demangleInitializer();
  NodePointer demangleImplParamConvention();
  NodePointer demangleImplResultConvention(Node::Kind ConvKind);
  NodePointer demangleImplFunctionType();
  NodePointer demangleMetatype();
  static NodePointer createArchetypeRef(int depth, int i);
  NodePointer demangleArchetype();
  NodePointer demangleAssociatedTypeSimple(NodePointer GenericParamIdx);
  NodePointer demangleAssociatedTypeCompound(NodePointer GenericParamIdx);

  NodePointer popAssocTypeName();
  static NodePointer getDependentGenericParamType(int depth, int index);
  NodePointer demangleGenericParamIndex();
  NodePointer popProtocolConformance();
  NodePointer demangleThunkOrSpecialization();
  NodePointer demangleGenericSpecialization(Node::Kind SpecKind);
  NodePointer demangleFunctionSpecialization();
  NodePointer demangleFuncSpecParam(Node::IndexType ParamIdx);
  NodePointer addFuncSpecParamIdentifier(NodePointer Param,
                                  FunctionSigSpecializationParamKind Kind,
                                  StringRef FirstParam = StringRef());
  NodePointer addFuncSpecParamNumber(NodePointer Param,
                              FunctionSigSpecializationParamKind Kind);

  NodePointer demangleSpecAttributes(Node::Kind SpecKind,
                                     bool demangleUniqueID = false);

  NodePointer demangleWitness();
  NodePointer demangleSpecialType();
  NodePointer demangleMetatypeRepresentation();
  NodePointer demangleFunctionEntity();
  NodePointer demangleEntity(Node::Kind Kind);
  NodePointer demangleProtocolListType();
  NodePointer demangleGenericSignature(bool hasParamCounts);
  NodePointer demangleGenericRequirement();
  NodePointer demangleGenericType();
  NodePointer demangleValueWitness();
};

} // end namespace NewMangling
} // end namespace swift

#endif // SWIFT_BASIC_DEMANGLER_H
