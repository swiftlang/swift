//===--- BodyASTSerializer.cpp - Bitstream body AST serialization --------===//
//
// This source code is part of the Swift.org open source project
//
// Copyright (c) 2024-2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "BodyASTSerializer.h"
#include "Serialization.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Type.h"
#include "swift/AST/FunctionRefInfo.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/ProtocolConformanceRef.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/GenericSignature.h"
#include "llvm/Bitcode/BitcodeConvenience.h"

using namespace swift;
using namespace swift::serialization;

uint32_t BodyASTSerializer::assignExprID(Expr *E) {
  if (!E)
    return 0;
  auto it = ExprIDs.find(E);
  if (it != ExprIDs.end())
    return it->second;
  uint32_t id = NextExprID++;
  ExprIDs[E] = id;
  return id;
}

uint32_t BodyASTSerializer::assignStmtID(Stmt *stmt) {
  if (!stmt)
    return 0;
  auto it = StmtIDs.find(stmt);
  if (it != StmtIDs.end())
    return it->second;
  uint32_t id = NextStmtID++;
  StmtIDs[stmt] = id;
  return id;
}

void BodyASTSerializer::serializeExpr(Expr *E) {
  if (!E)
    return;

  uint32_t exprID = assignExprID(E);

  TypeID typeID = 0;
  if (E->getType())
    typeID = S.addTypeRef(E->getType());

  bool implicit = E->isImplicit();

  auto &Out = S.getWriter();
  auto &Scratch = S.getScratchRecord();
  Scratch.clear();

  using namespace astcache_body_block;

  switch (E->getKind()) {
  case ExprKind::DeclRef: {
    auto *DRE = cast<DeclRefExpr>(E);
    DeclID declID = S.addDeclRef(DRE->getDecl());
    uint8_t fri = DRE->getFunctionRefInfo().getOpaqueValue();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_DeclRef);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(declID);
    Scratch.push_back(fri);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::IntegerLiteral: {
    auto *ILE = cast<IntegerLiteralExpr>(E);
    IdentifierID iid = S.addDeclBaseNameRef(
        DeclBaseName(S.getASTContext().getIdentifier(ILE->getDigitsText())));
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_IntegerLiteral);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(iid);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::MemberRef: {
    auto *MRE = cast<MemberRefExpr>(E);
    serializeExpr(MRE->getBase());
    uint32_t baseExprID = assignExprID(MRE->getBase());
    DeclID memberDeclID = S.addDeclRef(MRE->getMember().getDecl());
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_MemberRef);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(baseExprID);
    Scratch.push_back(memberDeclID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::Binary: {
    auto *BE = cast<BinaryExpr>(E);
    serializeExpr(BE->getLHS());
    serializeExpr(BE->getRHS());
    Expr *fnExpr = BE->getFn();
    if (fnExpr) serializeExpr(fnExpr);
    uint32_t lhsID = assignExprID(BE->getLHS());
    uint32_t rhsID = assignExprID(BE->getRHS());
    uint32_t fnID = fnExpr ? assignExprID(fnExpr) : 0;
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_Binary);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(lhsID);
    Scratch.push_back(rhsID);
    Scratch.push_back(fnID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::Call: {
    auto *CE = cast<CallExpr>(E);
    serializeExpr(CE->getFn());
    uint32_t fnID = assignExprID(CE->getFn());
    auto *args = CE->getArgs();
    uint32_t numArgs = args ? args->size() : 0;
    SmallVector<uint32_t, 4> argIDs;
    for (unsigned i = 0; i < numArgs; i++) {
      Expr *argExpr = args->getExpr(i);
      serializeExpr(argExpr);
      argIDs.push_back(assignExprID(argExpr));
    }
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_Call);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(fnID);
    Scratch.push_back(numArgs);
    for (auto aid : argIDs)
      Scratch.push_back(aid);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::Assign: {
    auto *AE = cast<AssignExpr>(E);
    serializeExpr(AE->getDest());
    serializeExpr(AE->getSrc());
    uint32_t destID = assignExprID(AE->getDest());
    uint32_t srcID = assignExprID(AE->getSrc());
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_Assign);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(destID);
    Scratch.push_back(srcID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::InOut: {
    auto *IO = cast<InOutExpr>(E);
    serializeExpr(IO->getSubExpr());
    uint32_t subID = assignExprID(IO->getSubExpr());
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_InOut);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(subID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::DotSyntaxCall: {
    auto *DSC = cast<DotSyntaxCallExpr>(E);
    serializeExpr(DSC->getFn());
    serializeExpr(DSC->getBase());
    uint32_t fnID = assignExprID(DSC->getFn());
    uint32_t baseID = assignExprID(DSC->getBase());
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_DotSyntaxCall);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(fnID);
    Scratch.push_back(baseID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  // === Literals ===
  case ExprKind::NilLiteral: {
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_NilLiteral);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::FloatLiteral: {
    auto *FLE = cast<FloatLiteralExpr>(E);
    IdentifierID iid = S.addDeclBaseNameRef(
        DeclBaseName(S.getASTContext().getIdentifier(FLE->getDigitsText())));
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_FloatLiteral);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(iid);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::BooleanLiteral: {
    auto *BLE = cast<BooleanLiteralExpr>(E);
    uint8_t value = BLE->getValue() ? 1u : 0u;
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_BooleanLiteral);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(value);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::StringLiteral: {
    auto *SLE = cast<StringLiteralExpr>(E);
    IdentifierID iid = S.addDeclBaseNameRef(
        DeclBaseName(S.getASTContext().getIdentifier(SLE->getValue())));
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_StringLiteral);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(iid);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::MagicIdentifierLiteral: {
    auto *MILE = cast<MagicIdentifierLiteralExpr>(E);
    uint8_t kind = static_cast<uint8_t>(MILE->getKind());
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_MagicIdentifierLiteral);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(kind);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  // === Identity wrappers ===
  case ExprKind::Paren: {
    auto *PE = cast<ParenExpr>(E);
    serializeExpr(PE->getSubExpr());
    uint32_t subID = assignExprID(PE->getSubExpr());
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_Paren);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(subID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::DotSelf: {
    auto *DSE = cast<DotSelfExpr>(E);
    serializeExpr(DSE->getSubExpr());
    uint32_t subID = assignExprID(DSE->getSubExpr());
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_DotSelf);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(subID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::Await: {
    auto *AE = cast<AwaitExpr>(E);
    serializeExpr(AE->getSubExpr());
    uint32_t subID = assignExprID(AE->getSubExpr());
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_Await);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(subID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::Unsafe: {
    auto *UE = cast<UnsafeExpr>(E);
    serializeExpr(UE->getSubExpr());
    uint32_t subID = assignExprID(UE->getSubExpr());
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_Unsafe);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(subID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::Borrow: {
    auto *BE = cast<BorrowExpr>(E);
    serializeExpr(BE->getSubExpr());
    uint32_t subID = assignExprID(BE->getSubExpr());
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_Borrow);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(subID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  // === Other simple ===
  case ExprKind::Copy: {
    auto *CE = cast<CopyExpr>(E);
    serializeExpr(CE->getSubExpr());
    uint32_t subID = assignExprID(CE->getSubExpr());
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_Copy);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(subID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::Consume: {
    auto *CE = cast<ConsumeExpr>(E);
    serializeExpr(CE->getSubExpr());
    uint32_t subID = assignExprID(CE->getSubExpr());
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_Consume);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(subID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::ForceValue: {
    auto *FVE = cast<ForceValueExpr>(E);
    serializeExpr(FVE->getSubExpr());
    uint32_t subID = assignExprID(FVE->getSubExpr());
    uint8_t forcedIUO = FVE->isForceOfImplicitlyUnwrappedOptional() ? 1u : 0u;
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_ForceValue);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(subID);
    Scratch.push_back(forcedIUO);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::BindOptional: {
    auto *BOE = cast<BindOptionalExpr>(E);
    serializeExpr(BOE->getSubExpr());
    uint32_t subID = assignExprID(BOE->getSubExpr());
    uint32_t depth = BOE->getDepth();
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_BindOptional);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(subID);
    Scratch.push_back(depth);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::OptionalEvaluation: {
    auto *OEE = cast<OptionalEvaluationExpr>(E);
    serializeExpr(OEE->getSubExpr());
    uint32_t subID = assignExprID(OEE->getSubExpr());
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_OptionalEvaluation);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(subID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::TupleElement: {
    auto *TEE = cast<TupleElementExpr>(E);
    serializeExpr(TEE->getBase());
    uint32_t subID = assignExprID(TEE->getBase());
    uint32_t fieldNo = TEE->getFieldNumber();
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_TupleElement);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(subID);
    Scratch.push_back(fieldNo);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  // === Ternary ===
  case ExprKind::Ternary: {
    auto *TE = cast<TernaryExpr>(E);
    serializeExpr(TE->getCondExpr());
    serializeExpr(TE->getThenExpr());
    serializeExpr(TE->getElseExpr());
    uint32_t condID = assignExprID(TE->getCondExpr());
    uint32_t trueID = assignExprID(TE->getThenExpr());
    uint32_t falseID = assignExprID(TE->getElseExpr());
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_Ternary);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(condID);
    Scratch.push_back(trueID);
    Scratch.push_back(falseID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  // === ImplicitConversionExpr subclasses ===
  // Most have the same shape: {subExprID}. The kind discriminates which
  // subclass to create on deserialization.
  case ExprKind::Load:
  case ExprKind::ABISafeConversion:
  case ExprKind::FunctionConversion:
  case ExprKind::CovariantFunctionConversion:
  case ExprKind::CovariantReturnConversion:
  case ExprKind::MetatypeConversion:
  case ExprKind::CollectionUpcastConversion:
  case ExprKind::AnyHashableErasure:
  case ExprKind::BridgeToObjC:
  case ExprKind::BridgeFromObjC:
  case ExprKind::ConditionalBridgeFromObjC:
  case ExprKind::DerivedToBase:
  case ExprKind::ArchetypeToSuper:
  case ExprKind::InjectIntoOptional:
  case ExprKind::ClassMetatypeToObject:
  case ExprKind::ExistentialMetatypeToObject:
  case ExprKind::ProtocolMetatypeToObject:
  case ExprKind::InOutToPointer:
  case ExprKind::ArrayToPointer:
  case ExprKind::StringToPointer:
  case ExprKind::PointerToPointer:
  case ExprKind::ForeignObjectConversion:
  case ExprKind::UnevaluatedInstance:
  case ExprKind::Unreachable:
  case ExprKind::DifferentiableFunction:
  case ExprKind::LinearFunction:
  case ExprKind::DifferentiableFunctionExtractOriginal:
  case ExprKind::LinearFunctionExtractOriginal:
  case ExprKind::LinearToDifferentiableFunction:
  case ExprKind::ActorIsolationErasure:
  case ExprKind::UnsafeCast: {
    auto *ICE = cast<ImplicitConversionExpr>(E);
    serializeExpr(ICE->getSubExpr());
    uint32_t subID = assignExprID(ICE->getSubExpr());
    Scratch.clear();
    Scratch.push_back(exprID);
    // Map ExprKind → astcache_body_block ExprKind.
    auto bodyKind = [](ExprKind k) -> uint8_t {
      switch (k) {
      case ExprKind::Load: return Expr_Load;
      case ExprKind::ABISafeConversion: return Expr_ABISafeConversion;
      case ExprKind::FunctionConversion: return Expr_FunctionConversion;
      case ExprKind::CovariantFunctionConversion:
        return Expr_CovariantFunctionConversion;
      case ExprKind::CovariantReturnConversion:
        return Expr_CovariantReturnConversion;
      case ExprKind::MetatypeConversion: return Expr_MetatypeConversion;
      case ExprKind::CollectionUpcastConversion:
        return Expr_CollectionUpcastConversion;
      case ExprKind::AnyHashableErasure: return Expr_AnyHashableErasure;
      case ExprKind::BridgeToObjC: return Expr_BridgeToObjC;
      case ExprKind::BridgeFromObjC: return Expr_BridgeFromObjC;
      case ExprKind::ConditionalBridgeFromObjC:
        return Expr_ConditionalBridgeFromObjC;
      case ExprKind::DerivedToBase: return Expr_DerivedToBase;
      case ExprKind::ArchetypeToSuper: return Expr_ArchetypeToSuper;
      case ExprKind::InjectIntoOptional: return Expr_InjectIntoOptional;
      case ExprKind::ClassMetatypeToObject: return Expr_ClassMetatypeToObject;
      case ExprKind::ExistentialMetatypeToObject:
        return Expr_ExistentialMetatypeToObject;
      case ExprKind::ProtocolMetatypeToObject:
        return Expr_ProtocolMetatypeToObject;
      case ExprKind::InOutToPointer: return Expr_InOutToPointer;
      case ExprKind::ArrayToPointer: return Expr_ArrayToPointer;
      case ExprKind::StringToPointer: return Expr_StringToPointer;
      case ExprKind::PointerToPointer: return Expr_PointerToPointer;
      case ExprKind::ForeignObjectConversion:
        return Expr_ForeignObjectConversion;
      case ExprKind::UnevaluatedInstance: return Expr_UnevaluatedInstance;
      case ExprKind::Unreachable: return Expr_Unreachable;
      case ExprKind::DifferentiableFunction: return Expr_DifferentiableFunction;
      case ExprKind::LinearFunction: return Expr_LinearFunction;
      case ExprKind::DifferentiableFunctionExtractOriginal:
        return Expr_DifferentiableFunctionExtractOriginal;
      case ExprKind::LinearFunctionExtractOriginal:
        return Expr_LinearFunctionExtractOriginal;
      case ExprKind::LinearToDifferentiableFunction:
        return Expr_LinearToDifferentiableFunction;
      case ExprKind::ActorIsolationErasure: return Expr_ActorIsolationErasure;
      case ExprKind::UnsafeCast: return Expr_UnsafeCast;
      default: llvm_unreachable("not a simple ImplicitConversionExpr");
      }
    };
    Scratch.push_back(bodyKind(E->getKind()));
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(subID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  // ErasureExpr: {subExprID, numConformances, [conformanceIDs...]}
  case ExprKind::Erasure: {
    auto *EE = cast<ErasureExpr>(E);
    serializeExpr(EE->getSubExpr());
    uint32_t subID = assignExprID(EE->getSubExpr());
    auto conformances = EE->getConformances();
    SmallVector<ProtocolConformanceID, 4> confIDs;
    for (auto &conf : conformances)
      confIDs.push_back(S.addConformanceRef(conf));
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_Erasure);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(subID);
    Scratch.push_back(static_cast<uint64_t>(confIDs.size()));
    for (auto cid : confIDs)
      Scratch.push_back(cid);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  // UnderlyingToOpaqueExpr: {subExprID, substitutionMapID}
  case ExprKind::UnderlyingToOpaque: {
    auto *UTO = cast<UnderlyingToOpaqueExpr>(E);
    serializeExpr(UTO->getSubExpr());
    uint32_t subID = assignExprID(UTO->getSubExpr());
    SubstitutionMapID subsID = S.addSubstitutionMapRef(UTO->substitutions);
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_UnderlyingToOpaque);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(subID);
    Scratch.push_back(subsID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  // DestructureTupleExpr: {subExprID, dstExprID, numElements,
  //                         [opaqueValueExprIDs...]}
  case ExprKind::DestructureTuple: {
    auto *DTE = cast<DestructureTupleExpr>(E);
    serializeExpr(DTE->getSubExpr());
    serializeExpr(DTE->getResultExpr());
    uint32_t subID = assignExprID(DTE->getSubExpr());
    uint32_t dstID = assignExprID(DTE->getResultExpr());
    auto elements = DTE->getDestructuredElements();
    SmallVector<uint32_t, 4> elemIDs;
    for (auto *OVE : elements) {
      serializeExpr(OVE);
      elemIDs.push_back(assignExprID(OVE));
    }
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_DestructureTuple);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(subID);
    Scratch.push_back(dstID);
    Scratch.push_back(static_cast<uint64_t>(elemIDs.size()));
    for (auto eid : elemIDs)
      Scratch.push_back(eid);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  // === ExplicitCastExpr subclasses ===
  // ForcedCheckedCast, ConditionalCheckedCast, Is, Coerce:
  //   {subExprID, targetTypeID, castKind}
  case ExprKind::ForcedCheckedCast:
  case ExprKind::ConditionalCheckedCast:
  case ExprKind::Is:
  case ExprKind::Coerce: {
    auto *ECE = cast<ExplicitCastExpr>(E);
    serializeExpr(ECE->getSubExpr());
    uint32_t subID = assignExprID(ECE->getSubExpr());
    TypeID targetTyID = 0;
    if (ECE->getCastType())
      targetTyID = S.addTypeRef(ECE->getCastType());
    // CastKind is only meaningful for CheckedCastExpr subclasses.
    uint8_t castKind = 0;
    if (auto *CCE = dyn_cast<CheckedCastExpr>(E))
      castKind = static_cast<uint8_t>(CCE->getCastKind());
    auto bodyKind = [](ExprKind k) -> uint8_t {
      switch (k) {
      case ExprKind::ForcedCheckedCast: return Expr_ForcedCheckedCast;
      case ExprKind::ConditionalCheckedCast: return Expr_ConditionalCheckedCast;
      case ExprKind::Is: return Expr_Is;
      case ExprKind::Coerce: return Expr_Coerce;
      default: llvm_unreachable("not an ExplicitCastExpr");
      }
    };
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(bodyKind(E->getKind()));
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(subID);
    Scratch.push_back(targetTyID);
    Scratch.push_back(castKind);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  // === Collections (ArrayExpr, DictionaryExpr, KeyPathApplicationExpr) ===
  case ExprKind::Array: {
    auto *AE = cast<ArrayExpr>(E);
    auto elements = AE->getElements();
    SmallVector<uint32_t, 4> elemIDs;
    for (auto *elt : elements) {
      serializeExpr(elt);
      elemIDs.push_back(assignExprID(elt));
    }
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_Array);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(static_cast<uint32_t>(elemIDs.size()));
    for (auto eid : elemIDs)
      Scratch.push_back(eid);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::Dictionary: {
    auto *DE = cast<DictionaryExpr>(E);
    auto elements = DE->getElements();
    SmallVector<uint32_t, 4> elemIDs;
    for (auto *elt : elements) {
      serializeExpr(elt);
      elemIDs.push_back(assignExprID(elt));
    }
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_Dictionary);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(static_cast<uint32_t>(elemIDs.size()));
    for (auto eid : elemIDs)
      Scratch.push_back(eid);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::KeyPathApplication: {
    auto *KPA = cast<KeyPathApplicationExpr>(E);
    serializeExpr(KPA->getBase());
    serializeExpr(KPA->getKeyPath());
    uint32_t baseID = assignExprID(KPA->getBase());
    uint32_t kpID = assignExprID(KPA->getKeyPath());
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_KeyPathApplication);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(baseID);
    Scratch.push_back(kpID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  // === Closures (ClosureExpr, AutoClosureExpr, CaptureListExpr) ===
  case ExprKind::Closure: {
    auto *CE = cast<ClosureExpr>(E);
    // Serialize the body BraceStmt if present.
    uint32_t bodyStmtID = 0;
    if (auto *body = CE->getBody()) {
      serializeStmt(body);
      bodyStmtID = assignStmtID(body);
    }
    // Serialize captures. Use getCachedCaptureInfo to avoid asserting
    // if captures haven't been computed (e.g., in unit tests).
    SmallVector<DeclID, 4> captureDeclIDs;
    if (auto captureInfo = CE->getCachedCaptureInfo()) {
      for (auto &cap : captureInfo->getCaptures()) {
        if (auto *VD = dyn_cast_or_null<ValueDecl>(cap.getDecl()))
          captureDeclIDs.push_back(S.addDeclRef(VD));
        else
          captureDeclIDs.push_back(0);
      }
    }
    // Serialize parameter list.
    auto *params = CE->getParameters();
    uint32_t numParams = params ? params->size() : 0;
    SmallVector<DeclID, 4> paramDeclIDs;
    for (unsigned i = 0; i < numParams; i++) {
      auto *PD = params->get(i);
      paramDeclIDs.push_back(S.addDeclRef(PD));
    }
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_Closure);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(bodyStmtID);
    Scratch.push_back(static_cast<uint32_t>(captureDeclIDs.size()));
    for (auto cid : captureDeclIDs)
      Scratch.push_back(cid);
    Scratch.push_back(numParams);
    for (auto pid : paramDeclIDs)
      Scratch.push_back(pid);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::AutoClosure: {
    auto *ACE = cast<AutoClosureExpr>(E);
    // Autoclosure wraps a single expression body.
    Expr *bodyExpr = ACE->getSingleExpressionBody();
    uint32_t bodyExprID = 0;
    if (bodyExpr) {
      serializeExpr(bodyExpr);
      bodyExprID = assignExprID(bodyExpr);
    }
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_AutoClosure);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(bodyExprID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::CaptureList: {
    auto *CLE = cast<CaptureListExpr>(E);
    auto captureList = CLE->getCaptureList();
    SmallVector<DeclID, 4> captureDeclIDs;
    for (auto &entry : captureList) {
      if (auto *VD = entry.getVar())
        captureDeclIDs.push_back(S.addDeclRef(VD));
      else
        captureDeclIDs.push_back(0);
    }
    // Serialize the closure body.
    auto *closure = CLE->getClosureBody();
    uint32_t bodyExprID = 0;
    if (closure) {
      serializeExpr(closure);
      bodyExprID = assignExprID(closure);
    }
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_CaptureList);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(bodyExprID);
    Scratch.push_back(static_cast<uint32_t>(captureDeclIDs.size()));
    for (auto cid : captureDeclIDs)
      Scratch.push_back(cid);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  // === Member access (SubscriptExpr, DynamicMemberRefExpr, DynamicSubscriptExpr) ===
  case ExprKind::Subscript: {
    auto *SE = cast<SubscriptExpr>(E);
    serializeExpr(SE->getBase());
    uint32_t baseID = assignExprID(SE->getBase());
    auto *args = SE->getArgs();
    uint32_t numArgs = args ? args->size() : 0;
    SmallVector<uint32_t, 4> argIDs;
    for (unsigned i = 0; i < numArgs; i++) {
      Expr *argExpr = args->getExpr(i);
      serializeExpr(argExpr);
      argIDs.push_back(assignExprID(argExpr));
    }
    DeclID fnID = 0;
    if (SE->hasDecl() && SE->getDecl().getDecl())
      fnID = S.addDeclRef(SE->getDecl().getDecl());
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_Subscript);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(baseID);
    Scratch.push_back(numArgs);
    for (auto aid : argIDs)
      Scratch.push_back(aid);
    Scratch.push_back(fnID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::DynamicMemberRef: {
    auto *DMR = cast<DynamicMemberRefExpr>(E);
    serializeExpr(DMR->getBase());
    uint32_t baseID = assignExprID(DMR->getBase());
    DeclID memberID = 0;
    if (DMR->hasDecl() && DMR->getDecl().getDecl())
      memberID = S.addDeclRef(DMR->getDecl().getDecl());
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_DynamicMemberRef);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(baseID);
    Scratch.push_back(memberID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::DynamicSubscript: {
    auto *DSS = cast<DynamicSubscriptExpr>(E);
    serializeExpr(DSS->getBase());
    uint32_t baseID = assignExprID(DSS->getBase());
    DeclID memberID = 0;
    if (DSS->hasDecl() && DSS->getDecl().getDecl())
      memberID = S.addDeclRef(DSS->getDecl().getDecl());
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_DynamicSubscript);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(baseID);
    Scratch.push_back(memberID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  // === Misc complex expressions ===
  case ExprKind::PrefixUnary: {
    auto *PUE = cast<PrefixUnaryExpr>(E);
    serializeExpr(PUE->getFn());
    serializeExpr(PUE->getOperand());
    uint32_t fnID = assignExprID(PUE->getFn());
    uint32_t operandID = assignExprID(PUE->getOperand());
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_PrefixUnary);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(fnID);
    Scratch.push_back(operandID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::PostfixUnary: {
    auto *PUE = cast<PostfixUnaryExpr>(E);
    serializeExpr(PUE->getFn());
    serializeExpr(PUE->getOperand());
    uint32_t fnID = assignExprID(PUE->getFn());
    uint32_t operandID = assignExprID(PUE->getOperand());
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_PostfixUnary);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(fnID);
    Scratch.push_back(operandID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::EnumIsCase: {
    auto *EIC = cast<EnumIsCaseExpr>(E);
    serializeExpr(EIC->getSubExpr());
    uint32_t subID = assignExprID(EIC->getSubExpr());
    DeclID caseItemID = 0;
    if (auto *EED = EIC->getEnumElement())
      caseItemID = S.addDeclRef(EED);
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_EnumIsCase);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(subID);
    Scratch.push_back(caseItemID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::DiscardAssignment: {
    // No extra fields beyond the standard {exprID, kind, typeID, implicit}.
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_DiscardAssignment);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::VarargExpansion: {
    auto *VE = cast<VarargExpansionExpr>(E);
    serializeExpr(VE->getSubExpr());
    uint32_t subID = assignExprID(VE->getSubExpr());
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_VarargExpansion);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(subID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::OpenExistential: {
    auto *OE = cast<OpenExistentialExpr>(E);
    serializeExpr(OE->getExistentialValue());
    serializeExpr(OE->getOpaqueValue());
    serializeExpr(OE->getSubExpr());
    uint32_t existentialID = assignExprID(OE->getExistentialValue());
    uint32_t opaqueID = assignExprID(OE->getOpaqueValue());
    uint32_t subID = assignExprID(OE->getSubExpr());
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_OpenExistential);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(existentialID);
    Scratch.push_back(opaqueID);
    Scratch.push_back(subID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::OpaqueValue: {
    // No extra fields beyond the standard {exprID, kind, typeID, implicit}.
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_OpaqueValue);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::DefaultArgument: {
    auto *DAE = cast<DefaultArgumentExpr>(E);
    DeclID paramDeclID = 0;
    if (auto *PD = DAE->getParamDecl())
      paramDeclID = S.addDeclRef(PD);
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_DefaultArgument);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(paramDeclID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::MakeTemporarilyEscapable: {
    auto *MTE = cast<MakeTemporarilyEscapableExpr>(E);
    serializeExpr(MTE->getNonescapingClosureValue());
    serializeExpr(MTE->getOpaqueValue());
    serializeExpr(MTE->getSubExpr());
    uint32_t nonEscapingID = assignExprID(MTE->getNonescapingClosureValue());
    uint32_t escapingID = assignExprID(MTE->getOpaqueValue());
    uint32_t subID = assignExprID(MTE->getSubExpr());
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_MakeTemporarilyEscapable);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(nonEscapingID);
    Scratch.push_back(escapingID);
    Scratch.push_back(subID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::DynamicType: {
    auto *DTE = cast<DynamicTypeExpr>(E);
    serializeExpr(DTE->getBase());
    uint32_t subID = assignExprID(DTE->getBase());
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_DynamicType);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(subID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  default:
    Scratch.clear();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_Error);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
}

void BodyASTSerializer::serializeStmt(Stmt *stmt) {
  if (!stmt)
    return;

  uint32_t stmtID = assignStmtID(stmt);

  auto &Out = S.getWriter();
  auto &Scratch = S.getScratchRecord();
  Scratch.clear();

  using namespace astcache_body_block;

  switch (stmt->getKind()) {
  case StmtKind::Brace: {
    auto *BS = cast<BraceStmt>(stmt);
    auto elements = BS->getElements();
    // Serialize sub-expressions/sub-statements FIRST, collecting their IDs.
    // We can't interleave serializeExpr() calls with Scratch building because
    // serializeExpr() clears and overwrites Scratch.
    SmallVector<std::pair<uint32_t, uint32_t>, 8> elementIDs;
    for (auto &elt : elements) {
      if (isa<Expr *>(elt)) {
        Expr *E = cast<Expr *>(elt);
        uint32_t eid = assignExprID(E);
        serializeExpr(E);
        elementIDs.push_back({0u, eid}); // elementKind = expr
      } else if (isa<Stmt *>(elt)) {
        Stmt *sub = cast<Stmt *>(elt);
        uint32_t sid = assignStmtID(sub);
        serializeStmt(sub);
        elementIDs.push_back({1u, sid}); // elementKind = stmt
      } else {
        elementIDs.push_back({2u, 0u}); // elementKind = decl/other
      }
    }
    // NOW build the BraceStmt record.
    Scratch.clear();
    Scratch.push_back(stmtID);
    Scratch.push_back(Stmt_Brace);
    Scratch.push_back(stmt->isImplicit() ? 1u : 0u);
    Scratch.push_back(static_cast<uint32_t>(elementIDs.size()));
    for (auto &pair : elementIDs) {
      Scratch.push_back(pair.first);
      Scratch.push_back(pair.second);
    }
    Out.EmitRecord(STMT_NODE, Scratch);
    break;
  }
  case StmtKind::Return: {
    auto *RS = cast<ReturnStmt>(stmt);
    bool hasResult = RS->hasResult();
    uint32_t resultExprID = 0;
    if (hasResult) {
      resultExprID = assignExprID(RS->getResult());
      serializeExpr(RS->getResult());
    }
    Scratch.clear(); // Clear after serializeExpr which may have used Scratch
    Scratch.push_back(stmtID);
    Scratch.push_back(Stmt_Return);
    Scratch.push_back(stmt->isImplicit() ? 1u : 0u);
    Scratch.push_back(hasResult ? 1u : 0u);
    if (hasResult)
      Scratch.push_back(resultExprID);
    Out.EmitRecord(STMT_NODE, Scratch);
    break;
  }
  case StmtKind::Yield: {
    auto *YS = cast<YieldStmt>(stmt);
    auto yields = YS->getYields();
    SmallVector<uint32_t, 4> yieldIDs;
    for (auto *yieldExpr : yields) {
      uint32_t eid = assignExprID(yieldExpr);
      serializeExpr(yieldExpr);
      yieldIDs.push_back(eid);
    }
    Scratch.clear();
    Scratch.push_back(stmtID);
    Scratch.push_back(Stmt_Yield);
    Scratch.push_back(stmt->isImplicit() ? 1u : 0u);
    Scratch.push_back(static_cast<uint32_t>(yieldIDs.size()));
    for (auto yid : yieldIDs)
      Scratch.push_back(yid);
    Out.EmitRecord(STMT_NODE, Scratch);
    break;
  }
  case StmtKind::Switch: {
    auto *SS = cast<SwitchStmt>(stmt);
    Expr *subject = SS->getSubjectExpr();
    uint32_t subjectID = 0;
    if (subject) {
      subjectID = assignExprID(subject);
      serializeExpr(subject);
    }
    auto cases = SS->getCases();
    SmallVector<uint32_t, 4> caseIDs;
    for (auto *CS : cases) {
      uint32_t sid = assignStmtID(CS);
      serializeStmt(CS);
      caseIDs.push_back(sid);
    }
    Scratch.clear();
    Scratch.push_back(stmtID);
    Scratch.push_back(Stmt_Switch);
    Scratch.push_back(stmt->isImplicit() ? 1u : 0u);
    Scratch.push_back(subjectID);
    Scratch.push_back(static_cast<uint32_t>(caseIDs.size()));
    for (auto cid : caseIDs)
      Scratch.push_back(cid);
    Out.EmitRecord(STMT_NODE, Scratch);
    break;
  }
  case StmtKind::Case: {
    auto *CS = cast<CaseStmt>(stmt);
    BraceStmt *caseBody = CS->getBody();
    uint32_t bodyID = 0;
    if (caseBody) {
      bodyID = assignStmtID(caseBody);
      serializeStmt(caseBody);
    }
    Scratch.clear();
    Scratch.push_back(stmtID);
    Scratch.push_back(Stmt_Case);
    Scratch.push_back(stmt->isImplicit() ? 1u : 0u);
    Scratch.push_back(bodyID);
    Out.EmitRecord(STMT_NODE, Scratch);
    break;
  }
  // === Simple statements ===
  case StmtKind::Break: {
    Scratch.clear();
    Scratch.push_back(stmtID);
    Scratch.push_back(Stmt_Break);
    Scratch.push_back(stmt->isImplicit() ? 1u : 0u);
    Out.EmitRecord(STMT_NODE, Scratch);
    break;
  }
  case StmtKind::Continue: {
    Scratch.clear();
    Scratch.push_back(stmtID);
    Scratch.push_back(Stmt_Continue);
    Scratch.push_back(stmt->isImplicit() ? 1u : 0u);
    Out.EmitRecord(STMT_NODE, Scratch);
    break;
  }
  case StmtKind::Fallthrough: {
    Scratch.clear();
    Scratch.push_back(stmtID);
    Scratch.push_back(Stmt_Fallthrough);
    Scratch.push_back(stmt->isImplicit() ? 1u : 0u);
    Out.EmitRecord(STMT_NODE, Scratch);
    break;
  }
  case StmtKind::Fail: {
    Scratch.clear();
    Scratch.push_back(stmtID);
    Scratch.push_back(Stmt_Fail);
    Scratch.push_back(stmt->isImplicit() ? 1u : 0u);
    Out.EmitRecord(STMT_NODE, Scratch);
    break;
  }
  case StmtKind::Throw: {
    auto *TS = cast<ThrowStmt>(stmt);
    Expr *subExpr = TS->getSubExpr();
    uint32_t subExprID = 0;
    if (subExpr) {
      subExprID = assignExprID(subExpr);
      serializeExpr(subExpr);
    }
    Scratch.clear();
    Scratch.push_back(stmtID);
    Scratch.push_back(Stmt_Throw);
    Scratch.push_back(stmt->isImplicit() ? 1u : 0u);
    Scratch.push_back(subExprID);
    Out.EmitRecord(STMT_NODE, Scratch);
    break;
  }
  case StmtKind::Discard: {
    // DiscardStmt has a SubExpr but for cache purposes we serialize {} —
    // the subExpr is always "discard self" and doesn't need round-tripping.
    Scratch.clear();
    Scratch.push_back(stmtID);
    Scratch.push_back(Stmt_Discard);
    Scratch.push_back(stmt->isImplicit() ? 1u : 0u);
    Out.EmitRecord(STMT_NODE, Scratch);
    break;
  }
  // === Control flow ===
  case StmtKind::Then: {
    auto *TS = cast<ThenStmt>(stmt);
    // ThenStmt wraps an Expr, not a BraceStmt. Serialize the sub-expression.
    Expr *result = TS->getResult();
    uint32_t resultExprID = 0;
    if (result) {
      resultExprID = assignExprID(result);
      serializeExpr(result);
    }
    Scratch.clear();
    Scratch.push_back(stmtID);
    Scratch.push_back(Stmt_Then);
    Scratch.push_back(stmt->isImplicit() ? 1u : 0u);
    Scratch.push_back(resultExprID);
    Out.EmitRecord(STMT_NODE, Scratch);
    break;
  }
  case StmtKind::Defer: {
    auto *DS = cast<DeferStmt>(stmt);
    BraceStmt *body = DS->getBodyAsWritten();
    uint32_t bodyID = 0;
    if (body) {
      bodyID = assignStmtID(body);
      serializeStmt(body);
    }
    Scratch.clear();
    Scratch.push_back(stmtID);
    Scratch.push_back(Stmt_Defer);
    Scratch.push_back(stmt->isImplicit() ? 1u : 0u);
    Scratch.push_back(bodyID);
    Out.EmitRecord(STMT_NODE, Scratch);
    break;
  }
  case StmtKind::If: {
    auto *IS = cast<IfStmt>(stmt);
    // Serialize the condition. IfStmt is a LabeledConditionalStmt;
    // for simple boolean conditions, extract the expr. For pattern bindings,
    // serialize just the boolean expr part (if any).
    uint32_t condExprID = 0;
    auto cond = IS->getCond();
    for (auto &elt : cond) {
      if (elt.getKind() == StmtConditionElement::CK_Boolean) {
        Expr *condExpr = elt.getBoolean();
        if (condExpr) {
          condExprID = assignExprID(condExpr);
          serializeExpr(condExpr);
        }
        break;
      }
      // For pattern binding conditions, serialize the initializer expr
      // as the condition (simplified reconstruction).
      if (elt.getKind() == StmtConditionElement::CK_PatternBinding) {
        Expr *init = elt.getInitializerOrNull();
        if (init) {
          condExprID = assignExprID(init);
          serializeExpr(init);
        }
        break;
      }
    }
    // Serialize then branch
    BraceStmt *thenStmt = IS->getThenStmt();
    uint32_t thenID = 0;
    if (thenStmt) {
      thenID = assignStmtID(thenStmt);
      serializeStmt(thenStmt);
    }
    // Serialize else branch (may be null or IfStmt for else-if)
    Stmt *elseStmt = IS->getElseStmt();
    uint32_t elseID = 0;
    if (elseStmt) {
      elseID = assignStmtID(elseStmt);
      serializeStmt(elseStmt);
    }
    Scratch.clear();
    Scratch.push_back(stmtID);
    Scratch.push_back(Stmt_If);
    Scratch.push_back(stmt->isImplicit() ? 1u : 0u);
    Scratch.push_back(condExprID);
    Scratch.push_back(thenID);
    Scratch.push_back(elseID);
    Out.EmitRecord(STMT_NODE, Scratch);
    break;
  }
  case StmtKind::Guard: {
    auto *GS = cast<GuardStmt>(stmt);
    // Serialize the condition (same approach as IfStmt).
    uint32_t condExprID = 0;
    auto cond = GS->getCond();
    for (auto &elt : cond) {
      if (elt.getKind() == StmtConditionElement::CK_Boolean) {
        Expr *condExpr = elt.getBoolean();
        if (condExpr) {
          condExprID = assignExprID(condExpr);
          serializeExpr(condExpr);
        }
        break;
      }
      if (elt.getKind() == StmtConditionElement::CK_PatternBinding) {
        Expr *init = elt.getInitializerOrNull();
        if (init) {
          condExprID = assignExprID(init);
          serializeExpr(init);
        }
        break;
      }
    }
    // Serialize body (the else block)
    BraceStmt *body = GS->getBody();
    uint32_t bodyID = 0;
    if (body) {
      bodyID = assignStmtID(body);
      serializeStmt(body);
    }
    Scratch.clear();
    Scratch.push_back(stmtID);
    Scratch.push_back(Stmt_Guard);
    Scratch.push_back(stmt->isImplicit() ? 1u : 0u);
    Scratch.push_back(condExprID);
    Scratch.push_back(bodyID);
    Out.EmitRecord(STMT_NODE, Scratch);
    break;
  }
  case StmtKind::While: {
    auto *WS = cast<WhileStmt>(stmt);
    // Serialize the condition (same approach as IfStmt).
    uint32_t condExprID = 0;
    auto cond = WS->getCond();
    for (auto &elt : cond) {
      if (elt.getKind() == StmtConditionElement::CK_Boolean) {
        Expr *condExpr = elt.getBoolean();
        if (condExpr) {
          condExprID = assignExprID(condExpr);
          serializeExpr(condExpr);
        }
        break;
      }
      if (elt.getKind() == StmtConditionElement::CK_PatternBinding) {
        Expr *init = elt.getInitializerOrNull();
        if (init) {
          condExprID = assignExprID(init);
          serializeExpr(init);
        }
        break;
      }
    }
    // Serialize body
    Stmt *body = WS->getBody();
    uint32_t bodyID = 0;
    if (body) {
      bodyID = assignStmtID(body);
      serializeStmt(body);
    }
    Scratch.clear();
    Scratch.push_back(stmtID);
    Scratch.push_back(Stmt_While);
    Scratch.push_back(stmt->isImplicit() ? 1u : 0u);
    Scratch.push_back(condExprID);
    Scratch.push_back(bodyID);
    Out.EmitRecord(STMT_NODE, Scratch);
    break;
  }
  case StmtKind::RepeatWhile: {
    auto *RWS = cast<RepeatWhileStmt>(stmt);
    Expr *cond = RWS->getCond();
    uint32_t condExprID = 0;
    if (cond) {
      condExprID = assignExprID(cond);
      serializeExpr(cond);
    }
    Stmt *body = RWS->getBody();
    uint32_t bodyID = 0;
    if (body) {
      bodyID = assignStmtID(body);
      serializeStmt(body);
    }
    Scratch.clear();
    Scratch.push_back(stmtID);
    Scratch.push_back(Stmt_RepeatWhile);
    Scratch.push_back(stmt->isImplicit() ? 1u : 0u);
    Scratch.push_back(condExprID);
    Scratch.push_back(bodyID);
    Out.EmitRecord(STMT_NODE, Scratch);
    break;
  }
  case StmtKind::ForEach: {
    auto *FES = cast<ForEachStmt>(stmt);
    // Serialize pattern via its VarDecl. For now, serialize just the
    // sequence, body, and where expr. Pattern serialization is complex;
    // we store a flag indicating whether there's a pattern binding.
    Pattern *pat = FES->getPattern();
    uint32_t patDeclID = 0;
    if (pat) {
      // Extract a VarDecl from the pattern. Common shape: BindingPattern
      // wrapping a NamedPattern (or a TuplePattern of NamedPatterns).
      // We use getSemanticsProvidingPattern to skip TypedPattern wrappers.
      if (auto *bindingPat = dyn_cast<BindingPattern>(pat))
        pat = bindingPat->getSubPattern();
      if (auto *namedPat = dyn_cast<NamedPattern>(pat))
        patDeclID = S.addDeclRef(namedPat->getDecl());
    }
    Expr *seq = FES->getSequence();
    uint32_t seqExprID = 0;
    if (seq) {
      seqExprID = assignExprID(seq);
      serializeExpr(seq);
    }
    BraceStmt *body = FES->getBody();
    uint32_t bodyID = 0;
    if (body) {
      bodyID = assignStmtID(body);
      serializeStmt(body);
    }
    Expr *whereExpr = FES->getWhere();
    uint32_t whereExprID = 0;
    if (whereExpr) {
      whereExprID = assignExprID(whereExpr);
      serializeExpr(whereExpr);
    }
    Scratch.clear();
    Scratch.push_back(stmtID);
    Scratch.push_back(Stmt_ForEach);
    Scratch.push_back(stmt->isImplicit() ? 1u : 0u);
    Scratch.push_back(patDeclID);
    Scratch.push_back(seqExprID);
    Scratch.push_back(bodyID);
    Scratch.push_back(whereExprID);
    Out.EmitRecord(STMT_NODE, Scratch);
    break;
  }
  // === Do/catch ===
  case StmtKind::Do: {
    auto *DS = cast<DoStmt>(stmt);
    BraceStmt *body = DS->getBody();
    uint32_t bodyID = 0;
    if (body) {
      bodyID = assignStmtID(body);
      serializeStmt(body);
    }
    Scratch.clear();
    Scratch.push_back(stmtID);
    Scratch.push_back(Stmt_Do);
    Scratch.push_back(stmt->isImplicit() ? 1u : 0u);
    Scratch.push_back(bodyID);
    Out.EmitRecord(STMT_NODE, Scratch);
    break;
  }
  case StmtKind::DoCatch: {
    auto *DCS = cast<DoCatchStmt>(stmt);
    Stmt *body = DCS->getBody();
    uint32_t bodyID = 0;
    if (body) {
      bodyID = assignStmtID(body);
      serializeStmt(body);
    }
    auto catches = DCS->getCatches();
    SmallVector<uint32_t, 4> caseIDs;
    for (auto *CS : catches) {
      uint32_t sid = assignStmtID(CS);
      serializeStmt(CS);
      caseIDs.push_back(sid);
    }
    Scratch.clear();
    Scratch.push_back(stmtID);
    Scratch.push_back(Stmt_DoCatch);
    Scratch.push_back(stmt->isImplicit() ? 1u : 0u);
    Scratch.push_back(bodyID);
    Scratch.push_back(static_cast<uint32_t>(caseIDs.size()));
    for (auto cid : caseIDs)
      Scratch.push_back(cid);
    Out.EmitRecord(STMT_NODE, Scratch);
    break;
  }
  // === Other ===
  case StmtKind::PoundAssert: {
    auto *PAS = cast<PoundAssertStmt>(stmt);
    Expr *cond = PAS->getCondition();
    uint32_t condExprID = 0;
    if (cond) {
      condExprID = assignExprID(cond);
      serializeExpr(cond);
    }
    // Serialize the message string via addUniquedStringRef.
    IdentifierID msgID = 0;
    StringRef msg = PAS->getMessage();
    if (!msg.empty())
      msgID = S.addUniquedStringRef(msg);
    Scratch.clear();
    Scratch.push_back(stmtID);
    Scratch.push_back(Stmt_PoundAssert);
    Scratch.push_back(stmt->isImplicit() ? 1u : 0u);
    Scratch.push_back(condExprID);
    Scratch.push_back(msgID);
    Out.EmitRecord(STMT_NODE, Scratch);
    break;
  }
  case StmtKind::Opaque: {
    Scratch.clear();
    Scratch.push_back(stmtID);
    Scratch.push_back(Stmt_Opaque);
    Scratch.push_back(stmt->isImplicit() ? 1u : 0u);
    Out.EmitRecord(STMT_NODE, Scratch);
    break;
  }
  default:
    Scratch.clear();
    Scratch.push_back(stmtID);
    Scratch.push_back(Stmt_Error);
    Scratch.push_back(stmt->isImplicit() ? 1u : 0u);
    Out.EmitRecord(STMT_NODE, Scratch);
    break;
  }
}

void BodyASTSerializer::serializeBody(DeclID funcDeclID, BraceStmt *body) {
  auto &Out = S.getWriter();

  // Enter the body block.
  llvm::BCBlockRAII block(Out, ASTCACHE_BODY_BLOCK_ID, 4);

  uint32_t rootStmtID = 0;
  if (body) {
    rootStmtID = assignStmtID(body);
    serializeStmt(body);
  }

  // Write the BODY record: {DeclID, rootStmtID}
  auto &Scratch = S.getScratchRecord();
  Scratch.clear();
  Scratch.push_back(funcDeclID);
  Scratch.push_back(rootStmtID);
  Out.EmitRecord(astcache_body_block::BODY, Scratch);
}
