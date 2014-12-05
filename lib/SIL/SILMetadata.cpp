//===--- SILMetadata.cpp - Metadata for SIL ---------------------*- C++ -*-===//
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

#include "swift/SIL/SILMetadata.h"
#include "swift/SIL/SILModule.h"

using namespace swift;

SILMetadata::SILMetadata(MDKind kind)
  : Kind(kind) {}

void SILMetadata::Profile(llvm::FoldingSetNodeID &ID) {
  switch(Kind) {
  case MDKind::BranchKind:
    cast<SILBranchNode>(this)->Profile(ID);
    break;
  }
}

SILMetadata::MDKind SILMetadata::getMDKind(StringRef Name) {
  if (Name == "prof")
    return MDKind::BranchKind;
  llvm_unreachable("unsupported metadata kind");
}

void SILBranchNode::Profile(llvm::FoldingSetNodeID &ID,
                            llvm::ArrayRef<uint32_t> weights) {
  ID.AddInteger(unsigned(MDKind::BranchKind));
  for (auto p : weights)
    ID.AddInteger(p);
}

SILBranchNode::SILBranchNode(unsigned numOps)
  : SILMetadata(MDKind::BranchKind), NumOperands(numOps) {}

SILBranchNode::SILBranchNode(llvm::ArrayRef<uint32_t> weights)
  : SILMetadata(MDKind::BranchKind), NumOperands(weights.size()) {
  auto nodeWeights = getMutableWeights();
  for (unsigned i = 0; i < NumOperands; ++i)
    nodeWeights[i] = weights[i];
}

/// Find or create a SILMetadata with the given kind.
SILMetadata *SILModule::getSILMetadata(const llvm::FoldingSetNodeID &ID,
                                       unsigned numOps, size_t bytes,
                                       size_t alignment,
                                       SILMetadata::MDKind kind,
                                       bool &newlyCreated) {
  newlyCreated = false;
  void *insertPos;
  if (auto *node = Metadatas.FindNodeOrInsertPos(ID, insertPos))
    return node;

  // Allocate and construct the new SILBranchNode.
  void *mem = BPA.Allocate(bytes, alignment);
  SILMetadata *newNode;
  switch(kind) {
  case SILMetadata::MDKind::BranchKind:
    newNode = new (mem) SILBranchNode(numOps);
    break;
  }
  Metadatas.InsertNode(newNode, insertPos);
  newlyCreated = true;
  return newNode;
}

SILMetadata *SILModule::getInstMetadata(SILMetadata::MDKind KindID,
                                        SILInstruction *Inst) const {
  auto iter = MetadataStore.find(Inst);
  if(iter == MetadataStore.end())
    return nullptr;
  return iter->second;
}

void SILModule::setInstMetadata(SILMetadata::MDKind KindID,
                                SILInstruction *Inst, SILMetadata *Node) {
  MetadataStore[Inst] = Node;
}

SILMetadata *SILModule::findDefinedMetadata(unsigned NodeID) const {
  auto iter = NumberedMetadata.find(NodeID);
  if (iter == NumberedMetadata.end())
    return nullptr;
  return iter->second;
}

void SILModule::defineMetadata(unsigned NodeID, SILMetadata *node) {
  NumberedMetadata[NodeID] = node;
}

SILBranchNode *SILBranchNode::get(SILModule &M,
                                  llvm::ArrayRef<uint32_t> weights) {
  if (weights.empty())
    return nullptr;

  llvm::FoldingSetNodeID ID;
  SILBranchNode::Profile(ID, weights);

  // Allocate and construct the new SILBranchNode.
  size_t bytes = sizeof(SILBranchNode)
               + sizeof(uint32_t) * weights.size();
  bool newlyCreated;
  auto *node = M.getSILMetadata(ID, weights.size(), bytes,
                                alignof(SILBranchNode), MDKind::BranchKind,
                                newlyCreated);

  assert(isa<SILBranchNode>(node) && "should return SILBranchNode");
  auto *branch = cast<SILBranchNode>(node);
  if (!newlyCreated)
    return branch;

  auto nodeWeights = branch->getMutableWeights();
  for (unsigned i = 0; i < weights.size(); ++i)
    nodeWeights[i] = weights[i];
  return branch;
}

void SILInstruction::setMetadata(SILMetadata::MDKind KindID,
                                 SILMetadata *Node) {
  if (!Node) return;

  // Call into SILModule to update MetadataStore.
  getModule().setInstMetadata(KindID, this, Node);
}
