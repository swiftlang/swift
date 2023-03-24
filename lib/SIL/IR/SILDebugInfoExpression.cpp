//===-------- SILDebugInfoExpression.cpp - DIExpression for SIL -----------===//
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
///
/// \file
/// This file contains the table used by SILDIExprInfo to map from
/// SILDIExprOperator to supplement information like the operator string.
///
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILDebugInfoExpression.h"
#include "swift/SIL/SILAllocated.h"
#include "swift/SIL/SILModule.h"
#include <unordered_map>

using namespace swift;

namespace std {
// This is, unfortunately, a workaround for an ancient bug in libstdc++:
// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=60970
// Which prevented std::hash<T> users from having drop-in support for
// enum class types.
template <> struct hash<SILDIExprOperator> {
  size_t operator()(SILDIExprOperator V) const noexcept {
    return std::hash<unsigned>{}(static_cast<unsigned>(V));
  }
};
} // end namespace std

const SILDIExprInfo *SILDIExprInfo::get(SILDIExprOperator Op) {
  static const std::unordered_map<SILDIExprOperator, SILDIExprInfo> Infos = {
      {SILDIExprOperator::Fragment,
       {"op_fragment", {SILDIExprElement::DeclKind}}},
      {SILDIExprOperator::Dereference, {"op_deref", {}}},
      {SILDIExprOperator::Plus, {"op_plus", {}}},
      {SILDIExprOperator::Minus, {"op_minus", {}}},
      {SILDIExprOperator::ConstUInt,
       {"op_constu", {SILDIExprElement::ConstIntKind}}},
      {SILDIExprOperator::ConstSInt,
       {"op_consts", {SILDIExprElement::ConstIntKind}}}};

  return Infos.count(Op) ? &Infos.at(Op) : nullptr;
}

SILDebugInfoExpression *
SILDebugInfoExpression::get(SILModule &mod, ArrayRef<SILDIExprElement *> start,
                            ArrayRef<SILDIExprElement *> end) {
  llvm::FoldingSetNodeID id;
  Profile(id, start);
  Profile(id, end);

  void *insertPos;
  auto existing = mod.diExprs.FindNodeOrInsertPos(id, insertPos);
  if (existing)
    return existing;

  unsigned numTrailingElts = start.size() + end.size();
  unsigned numBytes = totalSizeToAlloc<SILDIExprElement *>(numTrailingElts);
  void *mem = mod.allocate(numBytes, alignof(SILDebugInfoExpression));
  auto *diExpr = new (mem) SILDebugInfoExpression();
  diExpr->numElts = numTrailingElts;
  auto elements = llvm::makeMutableArrayRef(
      diExpr->getTrailingObjects<SILDIExprElement *>(), numTrailingElts);
  unsigned index = 0;
  for (auto &elt : start)
    elements[index++] = elt;
  for (auto &elt : end)
    elements[index++] = elt;
  assert(diExpr->getElements().size() == numTrailingElts);
  for (auto &elt : diExpr->getElements())
    assert(elt);
  mod.diExprs.InsertNode(diExpr, insertPos);
  return diExpr;
}

SILDebugInfoExpression *SILDebugInfoExpression::createFragment(SILModule &mod,
                                                               VarDecl *field) {
  assert(field);
  std::array<SILDIExprElement *, 2> inputs = {
      SILDIExprElement::createOperator(mod, SILDIExprOperator::Fragment),
      SILDIExprElement::createDecl(mod, field)};
  return SILDebugInfoExpression::get(mod, inputs);
}

void SILDebugInfoExpression::Profile(llvm::FoldingSetNodeID &id,
                                     ArrayRef<SILDIExprElement *> elements) {
  for (auto &e : elements) {
    e->Profile(id);
  }
}

SILDIExprElement *SILDIExprElement::createOperator(SILModule &mod,
                                                   SILDIExprOperator op) {
  llvm::FoldingSetNodeID id;
  Profile(id, op, nullptr, None);

  void *insertPos;
  auto existing = mod.diExprElements.FindNodeOrInsertPos(id, insertPos);
  if (existing)
    return existing;

  void *mem = mod.allocate(sizeof(SILDIExprElement), alignof(SILDIExprElement));
  SILDIExprElement *diOp = new (mem) SILDIExprElement(OperatorKind);
  diOp->Operator = op;
  mod.diExprElements.InsertNode(diOp, insertPos);
  return diOp;
}

SILDIExprElement *SILDIExprElement::createDecl(SILModule &mod, Decl *decl) {
  llvm::FoldingSetNodeID id;
  Profile(id, SILDIExprOperator::Invalid, decl, None);

  void *insertPos;
  auto existing = mod.diExprElements.FindNodeOrInsertPos(id, insertPos);
  if (existing)
    return existing;

  void *mem = mod.allocate(sizeof(SILDIExprElement), alignof(SILDIExprElement));
  SILDIExprElement *diOp = new (mem) SILDIExprElement(DeclKind);
  diOp->Declaration = decl;
  mod.diExprElements.InsertNode(diOp, insertPos);
  return diOp;
}

SILDIExprElement *SILDIExprElement::createConstInt(SILModule &mod,
                                                   uint64_t value) {
  llvm::FoldingSetNodeID id;
  Profile(id, SILDIExprOperator::Invalid, nullptr, value);

  void *insertPos;
  auto existing = mod.diExprElements.FindNodeOrInsertPos(id, insertPos);
  if (existing)
    return existing;

  void *mem = mod.allocate(sizeof(SILDIExprElement), alignof(SILDIExprElement));
  SILDIExprElement *diOp = new (mem) SILDIExprElement(ConstIntKind);
  diOp->ConstantInt = value;
  mod.diExprElements.InsertNode(diOp, insertPos);
  return diOp;
}

void SILDIExprElement::Profile(llvm::FoldingSetNodeID &id, SILDIExprOperator op,
                               Decl *decl, Optional<uint64_t> intValue) {
  id.AddInteger(unsigned(op));
  id.AddPointer(decl);
  id.AddBoolean(intValue.hasValue());
  id.AddInteger(intValue.getValueOr(0));
}
