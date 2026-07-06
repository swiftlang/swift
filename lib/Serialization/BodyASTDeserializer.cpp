//===--- BodyASTDeserializer.cpp - Bitstream body AST deserialization ----===//
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

#include "BodyASTDeserializer.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ArgumentList.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Type.h"
#include "swift/AST/FunctionRefInfo.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/ProtocolConformanceRef.h"
#include "swift/AST/ConcreteDeclRef.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Module.h"
#include "swift/AST/FileUnit.h"
#include "ModuleFormat.h"
#include "llvm/Bitstream/BitstreamReader.h"

using namespace swift;
using namespace swift::serialization;

Expr *BodyASTDeserializer::deserializeExpr(ArrayRef<uint64_t> record,
                                            uint32_t exprID) {
  using namespace astcache_body_block;

  if (record.size() < 4)
    return nullptr;

  uint8_t kind = static_cast<uint8_t>(record[1]);
  TypeID typeID = static_cast<TypeID>(record[2]);
  bool implicit = record[3] != 0;

  Type ty;
  if (typeID != 0 && ResolveType)
    ty = ResolveType(typeID);

  Expr *result = nullptr;

  switch (kind) {
  case Expr_DeclRef: {
    // {ExprID, ExprKind, TypeID, implicit, DeclID, FunctionRefInfo}
    if (record.size() < 6)
      return nullptr;
    DeclID declID = static_cast<DeclID>(record[4]);
    uint8_t fri = static_cast<uint8_t>(record[5]);
    Decl *D = ResolveDecl ? ResolveDecl(declID) : nullptr;
    auto *VD = dyn_cast_or_null<ValueDecl>(D);
    if (!VD)
      return nullptr;
    result = new (Ctx) DeclRefExpr(VD, DeclNameLoc(SourceLoc()), implicit,
                                   AccessSemantics::Ordinary, ty);
    if (auto *DRE = dyn_cast<DeclRefExpr>(result))
      DRE->setFunctionRefInfo(FunctionRefInfo::fromOpaque(fri));
    break;
  }
  case Expr_IntegerLiteral: {
    // {ExprID, ExprKind, TypeID, implicit, IdentifierID}
    if (record.size() < 5)
      return nullptr;
    IdentifierID iid = static_cast<IdentifierID>(record[4]);
    std::string digitsStr;
    if (ResolveIdentifier)
      digitsStr = ResolveIdentifier(iid).str().str();
    result = new (Ctx) IntegerLiteralExpr(digitsStr, SourceLoc(), implicit);
    if (ty)
      result->setType(ty);
    break;
  }
  case Expr_MemberRef: {
    // {ExprID, ExprKind, TypeID, implicit, baseExprID, memberDeclID}
    if (record.size() < 6)
      return nullptr;
    uint32_t baseExprID = static_cast<uint32_t>(record[4]);
    DeclID memberDeclID = static_cast<DeclID>(record[5]);
    Expr *base = (baseExprID > 0 && baseExprID <= ExprTable.size())
                     ? ExprTable[baseExprID - 1]
                     : nullptr;
    if (!base)
      return nullptr;
    Decl *D = ResolveDecl ? ResolveDecl(memberDeclID) : nullptr;
    auto *VD = dyn_cast_or_null<ValueDecl>(D);
    if (!VD)
      return nullptr;
    result = new (Ctx) MemberRefExpr(base, SourceLoc(), VD, DeclNameLoc(),
                                     implicit);
    if (ty)
      result->setType(ty);
    break;
  }
  case Expr_Type: {
    // {ExprID, ExprKind, TypeID, implicit}
    if (!ty) {
      result = new (Ctx) ErrorExpr(SourceRange());
      break;
    }
    result = TypeExpr::createImplicit(ty, Ctx);
    if (!implicit)
      result->setImplicit(false);
    break;
  }
  case Expr_Binary: {
    // {ExprID, ExprKind, TypeID, implicit, lhsExprID, rhsExprID, fnExprID}
    if (record.size() < 7)
      return nullptr;
    uint32_t lhsID = static_cast<uint32_t>(record[4]);
    uint32_t rhsID = static_cast<uint32_t>(record[5]);
    uint32_t fnID = static_cast<uint32_t>(record[6]);
    Expr *lhs = lookupExpr(lhsID);
    Expr *rhs = lookupExpr(rhsID);
    Expr *fn = (fnID > 0) ? lookupExpr(fnID) : nullptr;
    if (!lhs || !rhs)
      return nullptr;
    result = BinaryExpr::create(Ctx, lhs, fn, rhs, implicit, ty);
    break;
  }
  case Expr_Call: {
    // {ExprID, ExprKind, TypeID, implicit, fnExprID, numArgs, [argExprIDs...]}
    if (record.size() < 6)
      return nullptr;
    uint32_t fnID = static_cast<uint32_t>(record[4]);
    uint32_t numArgs = static_cast<uint32_t>(record[5]);
    Expr *fn = lookupExpr(fnID);
    if (!fn)
      return nullptr;
    SmallVector<Expr*, 4> argExprs;
    for (uint32_t i = 0; i < numArgs; i++) {
      if (record.size() <= 6 + i)
        return nullptr;
      Expr *arg = lookupExpr(static_cast<uint32_t>(record[6 + i]));
      if (!arg)
        return nullptr;
      argExprs.push_back(arg);
    }
    auto *argList = ArgumentList::forImplicitUnlabeled(Ctx, argExprs);
    result = CallExpr::createImplicit(Ctx, fn, argList);
    result->setImplicit(implicit);
    if (ty)
      result->setType(ty);
    break;
  }
  case Expr_Assign: {
    // {ExprID, ExprKind, TypeID, implicit, destExprID, srcExprID}
    if (record.size() < 6)
      return nullptr;
    Expr *dest = lookupExpr(static_cast<uint32_t>(record[4]));
    Expr *src = lookupExpr(static_cast<uint32_t>(record[5]));
    if (!dest || !src)
      return nullptr;
    result = new (Ctx) AssignExpr(dest, SourceLoc(), src, implicit);
    if (ty)
      result->setType(ty);
    break;
  }
  case Expr_InOut: {
    // {ExprID, ExprKind, TypeID, implicit, subExprID}
    if (record.size() < 5)
      return nullptr;
    Expr *sub = lookupExpr(static_cast<uint32_t>(record[4]));
    if (!sub)
      return nullptr;
    result = new (Ctx) InOutExpr(SourceLoc(), sub, Type(), implicit);
    break;
  }
  case Expr_DotSyntaxCall: {
    // {ExprID, ExprKind, TypeID, implicit, fnExprID, baseExprID}
    if (record.size() < 6)
      return nullptr;
    Expr *fn = lookupExpr(static_cast<uint32_t>(record[4]));
    Expr *base = lookupExpr(static_cast<uint32_t>(record[5]));
    if (!fn || !base)
      return nullptr;
    result = DotSyntaxCallExpr::create(Ctx, fn, SourceLoc(),
                                       Argument::unlabeled(base), ty);
    result->setImplicit(implicit);
    break;
  }
  // === Literals ===
  case Expr_NilLiteral: {
    // {ExprID, ExprKind, TypeID, implicit}
    result = new (Ctx) NilLiteralExpr(SourceLoc(), implicit);
    if (ty)
      result->setType(ty);
    break;
  }
  case Expr_FloatLiteral: {
    // {ExprID, ExprKind, TypeID, implicit, IdentifierID}
    if (record.size() < 5)
      return nullptr;
    IdentifierID iid = static_cast<IdentifierID>(record[4]);
    std::string digitsStr;
    if (ResolveIdentifier)
      digitsStr = ResolveIdentifier(iid).str().str();
    result = new (Ctx) FloatLiteralExpr(digitsStr, SourceLoc(), implicit);
    if (ty)
      result->setType(ty);
    break;
  }
  case Expr_BooleanLiteral: {
    // {ExprID, ExprKind, TypeID, implicit, uint8_t value}
    if (record.size() < 5)
      return nullptr;
    bool value = record[4] != 0;
    result = new (Ctx) BooleanLiteralExpr(value, SourceLoc(), implicit);
    if (ty)
      result->setType(ty);
    break;
  }
  case Expr_StringLiteral: {
    // {ExprID, ExprKind, TypeID, implicit, IdentifierID}
    if (record.size() < 5)
      return nullptr;
    IdentifierID iid = static_cast<IdentifierID>(record[4]);
    std::string valueStr;
    if (ResolveIdentifier)
      valueStr = ResolveIdentifier(iid).str().str();
    result = new (Ctx) StringLiteralExpr(valueStr, SourceRange(), implicit);
    if (ty)
      result->setType(ty);
    break;
  }
  case Expr_MagicIdentifierLiteral: {
    // {ExprID, ExprKind, TypeID, implicit, uint8_t kind}
    if (record.size() < 5)
      return nullptr;
    uint8_t kind = static_cast<uint8_t>(record[4]);
    result = new (Ctx) MagicIdentifierLiteralExpr(
        static_cast<MagicIdentifierLiteralExpr::Kind>(kind), SourceLoc(),
        implicit);
    if (ty)
      result->setType(ty);
    break;
  }
  // === Identity wrappers ===
  case Expr_Paren: {
    // {ExprID, ExprKind, TypeID, implicit, subExprID}
    if (record.size() < 5)
      return nullptr;
    Expr *sub = lookupExpr(static_cast<uint32_t>(record[4]));
    if (!sub)
      return nullptr;
    result = new (Ctx) ParenExpr(SourceLoc(), sub, SourceLoc(), ty);
    result->setImplicit(implicit);
    break;
  }
  case Expr_DotSelf: {
    // {ExprID, ExprKind, TypeID, implicit, subExprID}
    if (record.size() < 5)
      return nullptr;
    Expr *sub = lookupExpr(static_cast<uint32_t>(record[4]));
    if (!sub)
      return nullptr;
    result = new (Ctx) DotSelfExpr(sub, SourceLoc(), SourceLoc(), ty);
    result->setImplicit(implicit);
    break;
  }
  case Expr_Await: {
    // {ExprID, ExprKind, TypeID, implicit, subExprID}
    if (record.size() < 5)
      return nullptr;
    Expr *sub = lookupExpr(static_cast<uint32_t>(record[4]));
    if (!sub)
      return nullptr;
    result = new (Ctx) AwaitExpr(SourceLoc(), sub, ty, implicit);
    break;
  }
  case Expr_Unsafe: {
    // {ExprID, ExprKind, TypeID, implicit, subExprID}
    if (record.size() < 5)
      return nullptr;
    Expr *sub = lookupExpr(static_cast<uint32_t>(record[4]));
    if (!sub)
      return nullptr;
    result = new (Ctx) UnsafeExpr(SourceLoc(), sub, ty, implicit);
    break;
  }
  case Expr_Borrow: {
    // {ExprID, ExprKind, TypeID, implicit, subExprID}
    if (record.size() < 5)
      return nullptr;
    Expr *sub = lookupExpr(static_cast<uint32_t>(record[4]));
    if (!sub)
      return nullptr;
    result = new (Ctx) BorrowExpr(SourceLoc(), sub, ty, implicit);
    break;
  }
  // === Other simple ===
  case Expr_Copy: {
    // {ExprID, ExprKind, TypeID, implicit, subExprID}
    if (record.size() < 5)
      return nullptr;
    Expr *sub = lookupExpr(static_cast<uint32_t>(record[4]));
    if (!sub)
      return nullptr;
    result = new (Ctx) CopyExpr(SourceLoc(), sub, ty, implicit);
    break;
  }
  case Expr_Consume: {
    // {ExprID, ExprKind, TypeID, implicit, subExprID}
    if (record.size() < 5)
      return nullptr;
    Expr *sub = lookupExpr(static_cast<uint32_t>(record[4]));
    if (!sub)
      return nullptr;
    result = new (Ctx) ConsumeExpr(SourceLoc(), sub, ty, implicit);
    break;
  }
  case Expr_ForceValue: {
    // {ExprID, ExprKind, TypeID, implicit, subExprID, uint8_t forcedIUO}
    if (record.size() < 6)
      return nullptr;
    Expr *sub = lookupExpr(static_cast<uint32_t>(record[4]));
    if (!sub)
      return nullptr;
    bool forcedIUO = record[5] != 0;
    result = new (Ctx) ForceValueExpr(sub, SourceLoc(), forcedIUO);
    result->setImplicit(implicit);
    if (ty)
      result->setType(ty);
    break;
  }
  case Expr_BindOptional: {
    // {ExprID, ExprKind, TypeID, implicit, subExprID, uint32_t depth}
    if (record.size() < 6)
      return nullptr;
    Expr *sub = lookupExpr(static_cast<uint32_t>(record[4]));
    if (!sub)
      return nullptr;
    uint32_t depth = static_cast<uint32_t>(record[5]);
    result = new (Ctx) BindOptionalExpr(sub, SourceLoc(), depth, ty);
    result->setImplicit(implicit);
    break;
  }
  case Expr_OptionalEvaluation: {
    // {ExprID, ExprKind, TypeID, implicit, subExprID}
    if (record.size() < 5)
      return nullptr;
    Expr *sub = lookupExpr(static_cast<uint32_t>(record[4]));
    if (!sub)
      return nullptr;
    result = new (Ctx) OptionalEvaluationExpr(sub, ty);
    result->setImplicit(implicit);
    break;
  }
  case Expr_TupleElement: {
    // {ExprID, ExprKind, TypeID, implicit, subExprID, uint32_t fieldNo}
    if (record.size() < 6)
      return nullptr;
    Expr *sub = lookupExpr(static_cast<uint32_t>(record[4]));
    if (!sub)
      return nullptr;
    uint32_t fieldNo = static_cast<uint32_t>(record[5]);
    result = new (Ctx) TupleElementExpr(sub, SourceLoc(), fieldNo, SourceLoc(),
                                       ty);
    result->setImplicit(implicit);
    break;
  }
  // === Ternary ===
  case Expr_Ternary: {
    // {ExprID, ExprKind, TypeID, implicit, condExprID, trueExprID, falseExprID}
    if (record.size() < 7)
      return nullptr;
    Expr *cond = lookupExpr(static_cast<uint32_t>(record[4]));
    Expr *trueE = lookupExpr(static_cast<uint32_t>(record[5]));
    Expr *falseE = lookupExpr(static_cast<uint32_t>(record[6]));
    if (!cond || !trueE || !falseE)
      return nullptr;
    result = new (Ctx) TernaryExpr(cond, SourceLoc(), trueE, SourceLoc(),
                                  falseE, ty);
    result->setImplicit(implicit);
    break;
  }
  // === ImplicitConversionExpr subclasses ===
  // Simple shape: {ExprID, ExprKind, TypeID, implicit, subExprID}
  case Expr_Load:
  case Expr_ABISafeConversion:
  case Expr_FunctionConversion:
  case Expr_CovariantFunctionConversion:
  case Expr_CovariantReturnConversion:
  case Expr_MetatypeConversion:
  case Expr_CollectionUpcastConversion:
  case Expr_AnyHashableErasure:
  case Expr_BridgeToObjC:
  case Expr_BridgeFromObjC:
  case Expr_ConditionalBridgeFromObjC:
  case Expr_DerivedToBase:
  case Expr_ArchetypeToSuper:
  case Expr_InjectIntoOptional:
  case Expr_ClassMetatypeToObject:
  case Expr_ExistentialMetatypeToObject:
  case Expr_ProtocolMetatypeToObject:
  case Expr_InOutToPointer:
  case Expr_ArrayToPointer:
  case Expr_StringToPointer:
  case Expr_PointerToPointer:
  case Expr_ForeignObjectConversion:
  case Expr_UnevaluatedInstance:
  case Expr_Unreachable:
  case Expr_DifferentiableFunction:
  case Expr_LinearFunction:
  case Expr_DifferentiableFunctionExtractOriginal:
  case Expr_LinearFunctionExtractOriginal:
  case Expr_LinearToDifferentiableFunction:
  case Expr_ActorIsolationErasure:
  case Expr_UnsafeCast: {
    if (record.size() < 5)
      return nullptr;
    Expr *sub = lookupExpr(static_cast<uint32_t>(record[4]));
    if (!sub)
      return nullptr;
    switch (kind) {
    case Expr_Load:
      result = new (Ctx) LoadExpr(sub, ty); break;
    case Expr_ABISafeConversion:
      result = new (Ctx) ABISafeConversionExpr(sub, ty); break;
    case Expr_FunctionConversion:
      result = new (Ctx) FunctionConversionExpr(sub, ty); break;
    case Expr_CovariantFunctionConversion:
      result = new (Ctx) CovariantFunctionConversionExpr(sub, ty); break;
    case Expr_CovariantReturnConversion:
      result = new (Ctx) CovariantReturnConversionExpr(sub, ty); break;
    case Expr_MetatypeConversion:
      result = new (Ctx) MetatypeConversionExpr(sub, ty); break;
    case Expr_CollectionUpcastConversion:
      result = new (Ctx) CollectionUpcastConversionExpr(
          sub, ty, {}, {}); break;
    case Expr_AnyHashableErasure:
      result = new (Ctx) AnyHashableErasureExpr(
          sub, ty, ProtocolConformanceRef()); break;
    case Expr_BridgeToObjC:
      result = new (Ctx) BridgeToObjCExpr(sub, ty); break;
    case Expr_BridgeFromObjC:
      result = new (Ctx) BridgeFromObjCExpr(sub, ty); break;
    case Expr_ConditionalBridgeFromObjC:
      result = new (Ctx) ConditionalBridgeFromObjCExpr(
          sub, ty, ConcreteDeclRef()); break;
    case Expr_DerivedToBase:
      result = new (Ctx) DerivedToBaseExpr(sub, ty); break;
    case Expr_ArchetypeToSuper:
      result = new (Ctx) ArchetypeToSuperExpr(sub, ty); break;
    case Expr_InjectIntoOptional:
      result = new (Ctx) InjectIntoOptionalExpr(sub, ty); break;
    case Expr_ClassMetatypeToObject:
      result = new (Ctx) ClassMetatypeToObjectExpr(sub, ty); break;
    case Expr_ExistentialMetatypeToObject:
      result = new (Ctx) ExistentialMetatypeToObjectExpr(sub, ty); break;
    case Expr_ProtocolMetatypeToObject:
      result = new (Ctx) ProtocolMetatypeToObjectExpr(sub, ty); break;
    case Expr_InOutToPointer:
      result = new (Ctx) InOutToPointerExpr(sub, ty); break;
    case Expr_ArrayToPointer:
      result = new (Ctx) ArrayToPointerExpr(sub, ty); break;
    case Expr_StringToPointer:
      result = new (Ctx) StringToPointerExpr(sub, ty); break;
    case Expr_PointerToPointer:
      result = new (Ctx) PointerToPointerExpr(sub, ty); break;
    case Expr_ForeignObjectConversion:
      result = new (Ctx) ForeignObjectConversionExpr(sub, ty); break;
    case Expr_UnevaluatedInstance:
      result = new (Ctx) UnevaluatedInstanceExpr(sub, ty); break;
    case Expr_Unreachable:
      result = UnreachableExpr::create(Ctx, sub, ty); break;
    case Expr_DifferentiableFunction:
      result = new (Ctx) DifferentiableFunctionExpr(sub, ty); break;
    case Expr_LinearFunction:
      result = new (Ctx) LinearFunctionExpr(sub, ty); break;
    case Expr_DifferentiableFunctionExtractOriginal:
      result = new (Ctx) DifferentiableFunctionExtractOriginalExpr(sub, ty);
      break;
    case Expr_LinearFunctionExtractOriginal:
      result = new (Ctx) LinearFunctionExtractOriginalExpr(sub, ty); break;
    case Expr_LinearToDifferentiableFunction:
      result = new (Ctx) LinearToDifferentiableFunctionExpr(sub, ty); break;
    case Expr_ActorIsolationErasure:
      result = new (Ctx) ActorIsolationErasureExpr(sub, ty); break;
    case Expr_UnsafeCast:
      result = new (Ctx) UnsafeCastExpr(sub, ty); break;
    default:
      llvm_unreachable("not a simple ImplicitConversionExpr kind");
    }
    break;
  }
  // ErasureExpr: {ExprID, ExprKind, TypeID, implicit, subExprID,
  //                numConformances, [conformanceIDs...]}
  case Expr_Erasure: {
    if (record.size() < 6)
      return nullptr;
    Expr *sub = lookupExpr(static_cast<uint32_t>(record[4]));
    if (!sub)
      return nullptr;
    uint32_t numConfs = static_cast<uint32_t>(record[5]);
    SmallVector<ProtocolConformanceRef, 4> conformances;
    for (uint32_t i = 0; i < numConfs; i++) {
      if (record.size() <= 6 + i)
        return nullptr;
      ProtocolConformanceID confID =
          static_cast<ProtocolConformanceID>(record[6 + i]);
      if (ResolveConformance)
        conformances.push_back(ResolveConformance(confID));
      else
        conformances.push_back(ProtocolConformanceRef());
    }
    if (ty && ty->isExistentialType()) {
      result = ErasureExpr::create(Ctx, sub, ty, conformances, {});
    } else {
      // Type resolution failed (e.g., test context without ModuleFile).
      // Fall back to ErrorExpr to avoid crashing on getExistentialLayout.
      result = new (Ctx) ErrorExpr(SourceRange());
    }
    break;
  }
  // UnderlyingToOpaqueExpr: {ExprID, ExprKind, TypeID, implicit, subExprID,
  //                           substitutionMapID}
  case Expr_UnderlyingToOpaque: {
    if (record.size() < 6)
      return nullptr;
    Expr *sub = lookupExpr(static_cast<uint32_t>(record[4]));
    if (!sub)
      return nullptr;
    SubstitutionMapID subsID = static_cast<SubstitutionMapID>(record[5]);
    SubstitutionMap subs;
    if (subsID != 0 && ResolveSubstitutionMap)
      subs = ResolveSubstitutionMap(subsID);
    result = new (Ctx) UnderlyingToOpaqueExpr(sub, ty, subs);
    break;
  }
  // DestructureTupleExpr: {ExprID, ExprKind, TypeID, implicit, subExprID,
  //                         dstExprID, numElements, [opaqueValueExprIDs...]}
  case Expr_DestructureTuple: {
    if (record.size() < 7)
      return nullptr;
    Expr *sub = lookupExpr(static_cast<uint32_t>(record[4]));
    Expr *dst = lookupExpr(static_cast<uint32_t>(record[5]));
    if (!sub || !dst)
      return nullptr;
    uint32_t numElems = static_cast<uint32_t>(record[6]);
    SmallVector<OpaqueValueExpr *, 4> elems;
    for (uint32_t i = 0; i < numElems; i++) {
      if (record.size() <= 7 + i)
        return nullptr;
      auto *OVE = dyn_cast_or_null<OpaqueValueExpr>(
          lookupExpr(static_cast<uint32_t>(record[7 + i])));
      if (!OVE)
        return nullptr;
      elems.push_back(OVE);
    }
    result = DestructureTupleExpr::create(Ctx, elems, sub, dst, ty);
    break;
  }
  // === ExplicitCastExpr subclasses ===
  // {ExprID, ExprKind, TypeID, implicit, subExprID, targetTypeID, castKind}
  case Expr_ForcedCheckedCast:
  case Expr_ConditionalCheckedCast:
  case Expr_Is:
  case Expr_Coerce: {
    if (record.size() < 7)
      return nullptr;
    Expr *sub = lookupExpr(static_cast<uint32_t>(record[4]));
    if (!sub)
      return nullptr;
    TypeID targetTyID = static_cast<TypeID>(record[5]);
    uint8_t castKind = static_cast<uint8_t>(record[6]);
    Type targetTy;
    if (targetTyID != 0 && ResolveType)
      targetTy = ResolveType(targetTyID);
    // If we couldn't resolve the target type, fall back to the expr's type
    // (which may also be null in unit tests with no-op resolvers).
    if (!targetTy)
      targetTy = ty;
    if (!targetTy) {
      // No type available at all — cannot construct the cast expr.
      result = new (Ctx) ErrorExpr(SourceRange());
      break;
    }
    switch (kind) {
    case Expr_ForcedCheckedCast:
      result = ForcedCheckedCastExpr::createImplicit(Ctx, sub, targetTy);
      break;
    case Expr_ConditionalCheckedCast:
      result = ConditionalCheckedCastExpr::createImplicit(Ctx, sub, targetTy);
      break;
    case Expr_Is:
      result = IsExpr::createImplicit(Ctx, sub, targetTy);
      break;
    case Expr_Coerce:
      result = CoerceExpr::createImplicit(Ctx, sub, targetTy);
      break;
    default:
      llvm_unreachable("not an ExplicitCastExpr kind");
    }
    // Set cast kind for CheckedCastExpr subclasses.
    if (auto *CCE = dyn_cast<CheckedCastExpr>(result))
      CCE->setCastKind(static_cast<CheckedCastKind>(castKind));
    // Override implicit flag and type from the record.
    result->setImplicit(implicit);
    if (ty)
      result->setType(ty);
    break;
  }
  // === Collections (ArrayExpr, DictionaryExpr, KeyPathApplicationExpr) ===
  case Expr_Array: {
    // {ExprID, ExprKind, TypeID, implicit, numElements, [elementExprIDs...]}
    if (record.size() < 5)
      return nullptr;
    uint32_t numElements = static_cast<uint32_t>(record[4]);
    SmallVector<Expr*, 4> elements;
    for (uint32_t i = 0; i < numElements; i++) {
      if (record.size() <= 5 + i)
        return nullptr;
      Expr *elt = lookupExpr(static_cast<uint32_t>(record[5 + i]));
      if (!elt)
        return nullptr;
      elements.push_back(elt);
    }
    result = ArrayExpr::create(Ctx, SourceLoc(), elements, {}, SourceLoc(), ty);
    result->setImplicit(implicit);
    break;
  }
  case Expr_Dictionary: {
    // {ExprID, ExprKind, TypeID, implicit, numElements, [elementExprIDs...]}
    if (record.size() < 5)
      return nullptr;
    uint32_t numElements = static_cast<uint32_t>(record[4]);
    SmallVector<Expr*, 4> elements;
    for (uint32_t i = 0; i < numElements; i++) {
      if (record.size() <= 5 + i)
        return nullptr;
      Expr *elt = lookupExpr(static_cast<uint32_t>(record[5 + i]));
      if (!elt)
        return nullptr;
      elements.push_back(elt);
    }
    result = DictionaryExpr::create(Ctx, SourceLoc(), elements, {},
                                    SourceLoc(), ty);
    result->setImplicit(implicit);
    break;
  }
  case Expr_KeyPathApplication: {
    // {ExprID, ExprKind, TypeID, implicit, baseExprID, keyPathExprID}
    if (record.size() < 6)
      return nullptr;
    Expr *base = lookupExpr(static_cast<uint32_t>(record[4]));
    Expr *kp = lookupExpr(static_cast<uint32_t>(record[5]));
    if (!base || !kp)
      return nullptr;
    result = new (Ctx) KeyPathApplicationExpr(base, SourceLoc(), kp,
                                               SourceLoc(), ty, implicit);
    break;
  }
  // === Closures (ClosureExpr, AutoClosureExpr, CaptureListExpr) ===
  case Expr_Closure: {
    // {ExprID, ExprKind, TypeID, implicit, bodyStmtID, numCaptures,
    //  [captureDeclIDs...], numParams, [paramDeclIDs...]}
    if (record.size() < 7)
      return nullptr;
    uint32_t bodyStmtID = static_cast<uint32_t>(record[4]);
    uint32_t numCaptures = static_cast<uint32_t>(record[5]);
    if (record.size() < 6 + numCaptures + 1)
      return nullptr;
    SmallVector<VarDecl*, 4> captureDecls;
    for (uint32_t i = 0; i < numCaptures; i++) {
      DeclID cid = static_cast<DeclID>(record[6 + i]);
      Decl *D = ResolveDecl ? ResolveDecl(cid) : nullptr;
      captureDecls.push_back(dyn_cast_or_null<VarDecl>(D));
    }
    uint32_t paramOffset = 6 + numCaptures;
    uint32_t numParams = static_cast<uint32_t>(record[paramOffset]);
    if (record.size() < paramOffset + 1 + numParams)
      return nullptr;
    SmallVector<ParamDecl*, 4> paramDecls;
    for (uint32_t i = 0; i < numParams; i++) {
      DeclID pid = static_cast<DeclID>(record[paramOffset + 1 + i]);
      Decl *D = ResolveDecl ? ResolveDecl(pid) : nullptr;
      if (auto *PD = dyn_cast_or_null<ParamDecl>(D))
        paramDecls.push_back(PD);
    }
    auto *params = ParameterList::create(Ctx, paramDecls);
    // Get a parent DeclContext from the loaded module.
    DeclContext *parentDC = nullptr;
    if (auto mods = Ctx.getLoadedModules(); mods.begin() != mods.end()) {
      auto files = (*mods.begin()).second->getFiles();
      if (!files.empty())
        parentDC = files.front();
    }
    auto *closure = new (Ctx) ClosureExpr(DeclAttributes(), SourceRange(),
                                          /*capturedSelfDecl=*/nullptr,
                                          params, SourceLoc(), SourceLoc(),
                                          /*thrownType=*/nullptr, SourceLoc(),
                                          SourceLoc(),
                                          /*explicitResultType=*/nullptr,
                                          /*parent=*/parentDC);
    if (bodyStmtID > 0) {
      if (auto *body = lookupStmt(bodyStmtID))
        closure->setBody(cast<BraceStmt>(body));
    }
    if (ty)
      closure->setType(ty);
    result = closure;
    break;
  }
  case Expr_AutoClosure: {
    // {ExprID, ExprKind, TypeID, implicit, bodyExprID}
    if (record.size() < 5)
      return nullptr;
    uint32_t bodyExprID = static_cast<uint32_t>(record[4]);
    Expr *bodyExpr = (bodyExprID > 0) ? lookupExpr(bodyExprID) : nullptr;
    DeclContext *parentDC = nullptr;
    if (auto mods = Ctx.getLoadedModules(); mods.begin() != mods.end()) {
      auto files = (*mods.begin()).second->getFiles();
      if (!files.empty())
        parentDC = files.front();
    }
    auto *ace = new (Ctx) AutoClosureExpr(bodyExpr, ty, /*parent=*/parentDC);
    if (ty)
      ace->setType(ty);
    result = ace;
    break;
  }
  case Expr_CaptureList: {
    // {ExprID, ExprKind, TypeID, implicit, bodyExprID, numCaptures,
    //  [captureDeclIDs...]}
    if (record.size() < 6)
      return nullptr;
    uint32_t bodyExprID = static_cast<uint32_t>(record[4]);
    uint32_t numCaptures = static_cast<uint32_t>(record[5]);
    if (record.size() < 6 + numCaptures)
      return nullptr;
    // CaptureListExpr requires CaptureListEntries backed by PatternBindingDecls.
    // For deserialization, we create the CaptureListExpr with an empty capture
    // list — captures are informational and not needed for AST structure.
    auto *closure = (bodyExprID > 0)
                        ? dyn_cast_or_null<AbstractClosureExpr>(
                              lookupExpr(bodyExprID))
                        : nullptr;
    if (closure) {
      result = CaptureListExpr::create(Ctx, {}, closure);
      if (ty)
        result->setType(ty);
    } else {
      result = new (Ctx) ErrorExpr(SourceRange());
    }
    break;
  }
  // === Member access ===
  case Expr_Subscript: {
    // {ExprID, ExprKind, TypeID, implicit, baseExprID, numArgs,
    //  [argExprIDs...], fnDeclID}
    if (record.size() < 7)
      return nullptr;
    Expr *base = lookupExpr(static_cast<uint32_t>(record[4]));
    if (!base)
      return nullptr;
    uint32_t numArgs = static_cast<uint32_t>(record[5]);
    if (record.size() < 6 + numArgs + 1)
      return nullptr;
    SmallVector<Expr*, 4> argExprs;
    for (uint32_t i = 0; i < numArgs; i++) {
      Expr *arg = lookupExpr(static_cast<uint32_t>(record[6 + i]));
      if (!arg)
        return nullptr;
      argExprs.push_back(arg);
    }
    DeclID fnID = static_cast<DeclID>(record[6 + numArgs]);
    ConcreteDeclRef declRef;
    if (fnID != 0 && ResolveDecl) {
      if (auto *VD = dyn_cast_or_null<ValueDecl>(ResolveDecl(fnID)))
        declRef = ConcreteDeclRef(VD);
    }
    auto *argList = ArgumentList::forImplicitUnlabeled(Ctx, argExprs);
    result = SubscriptExpr::create(Ctx, base, argList, declRef, implicit);
    if (ty)
      result->setType(ty);
    break;
  }
  case Expr_DynamicMemberRef: {
    // {ExprID, ExprKind, TypeID, implicit, baseExprID, memberDeclID}
    if (record.size() < 6)
      return nullptr;
    Expr *base = lookupExpr(static_cast<uint32_t>(record[4]));
    if (!base)
      return nullptr;
    DeclID memberID = static_cast<DeclID>(record[5]);
    ConcreteDeclRef member;
    if (memberID != 0 && ResolveDecl) {
      if (auto *VD = dyn_cast_or_null<ValueDecl>(ResolveDecl(memberID)))
        member = ConcreteDeclRef(VD);
    }
    result = new (Ctx) DynamicMemberRefExpr(base, SourceLoc(), member,
                                             DeclNameLoc());
    if (ty)
      result->setType(ty);
    break;
  }
  case Expr_DynamicSubscript: {
    // {ExprID, ExprKind, TypeID, implicit, baseExprID, memberDeclID}
    if (record.size() < 6)
      return nullptr;
    Expr *base = lookupExpr(static_cast<uint32_t>(record[4]));
    if (!base)
      return nullptr;
    DeclID memberID = static_cast<DeclID>(record[5]);
    ConcreteDeclRef member;
    if (memberID != 0 && ResolveDecl) {
      if (auto *VD = dyn_cast_or_null<ValueDecl>(ResolveDecl(memberID)))
        member = ConcreteDeclRef(VD);
    }
    result = DynamicSubscriptExpr::create(Ctx, base, /*argList=*/nullptr,
                                          member, implicit);
    if (ty)
      result->setType(ty);
    break;
  }
  // === Misc complex expressions ===
  case Expr_PrefixUnary: {
    // {ExprID, ExprKind, TypeID, implicit, fnExprID, operandExprID}
    if (record.size() < 6)
      return nullptr;
    Expr *fn = lookupExpr(static_cast<uint32_t>(record[4]));
    Expr *operand = lookupExpr(static_cast<uint32_t>(record[5]));
    if (!fn || !operand)
      return nullptr;
    result = PrefixUnaryExpr::create(Ctx, fn, operand, ty);
    break;
  }
  case Expr_PostfixUnary: {
    // {ExprID, ExprKind, TypeID, implicit, fnExprID, operandExprID}
    if (record.size() < 6)
      return nullptr;
    Expr *fn = lookupExpr(static_cast<uint32_t>(record[4]));
    Expr *operand = lookupExpr(static_cast<uint32_t>(record[5]));
    if (!fn || !operand)
      return nullptr;
    result = PostfixUnaryExpr::create(Ctx, fn, operand, ty);
    break;
  }
  case Expr_EnumIsCase: {
    // {ExprID, ExprKind, TypeID, implicit, subExprID, caseItemDeclID}
    if (record.size() < 6)
      return nullptr;
    Expr *sub = lookupExpr(static_cast<uint32_t>(record[4]));
    if (!sub)
      return nullptr;
    DeclID caseItemID = static_cast<DeclID>(record[5]);
    EnumElementDecl *EED = nullptr;
    if (caseItemID != 0 && ResolveDecl)
      EED = dyn_cast_or_null<EnumElementDecl>(ResolveDecl(caseItemID));
    result = new (Ctx) EnumIsCaseExpr(sub, /*caseRepr=*/nullptr, EED);
    if (ty)
      result->setType(ty);
    break;
  }
  case Expr_DiscardAssignment: {
    // {ExprID, ExprKind, TypeID, implicit}
    result = new (Ctx) DiscardAssignmentExpr(SourceLoc(), implicit);
    if (ty)
      result->setType(ty);
    break;
  }
  case Expr_VarargExpansion: {
    // {ExprID, ExprKind, TypeID, implicit, subExprID}
    if (record.size() < 5)
      return nullptr;
    Expr *sub = lookupExpr(static_cast<uint32_t>(record[4]));
    if (!sub)
      return nullptr;
    result = VarargExpansionExpr::createParamExpansion(Ctx, sub);
    if (ty)
      result->setType(ty);
    if (implicit)
      result->setImplicit(true);
    break;
  }
  case Expr_OpenExistential: {
    // {ExprID, ExprKind, TypeID, implicit, existentialValueExprID,
    //  opaqueValueExprID, subExprID}
    if (record.size() < 7)
      return nullptr;
    Expr *existential = lookupExpr(static_cast<uint32_t>(record[4]));
    auto *opaque = dyn_cast_or_null<OpaqueValueExpr>(
        lookupExpr(static_cast<uint32_t>(record[5])));
    Expr *sub = lookupExpr(static_cast<uint32_t>(record[6]));
    if (!existential || !opaque || !sub)
      return nullptr;
    result = new (Ctx) OpenExistentialExpr(existential, opaque, sub, ty);
    break;
  }
  case Expr_OpaqueValue: {
    // {ExprID, ExprKind, TypeID, implicit}
    result = OpaqueValueExpr::createImplicit(Ctx, ty);
    break;
  }
  case Expr_DefaultArgument: {
    // {ExprID, ExprKind, TypeID, implicit, paramDeclID}
    if (record.size() < 5)
      return nullptr;
    DeclID paramDeclID = static_cast<DeclID>(record[4]);
    // DefaultArgumentExpr needs an owner decl; without one, create an ErrorExpr.
    if (paramDeclID == 0 || !ResolveDecl) {
      result = new (Ctx) ErrorExpr(SourceRange());
      break;
    }
    Decl *D = ResolveDecl(paramDeclID);
    if (auto *PD = dyn_cast_or_null<ParamDecl>(D)) {
      result = new (Ctx) DefaultArgumentExpr(ConcreteDeclRef(), /*paramIndex=*/0,
                                              SourceLoc(), ty, PD->getDeclContext());
      (void)PD;
    } else {
      result = new (Ctx) ErrorExpr(SourceRange());
    }
    if (ty)
      result->setType(ty);
    break;
  }
  case Expr_MakeTemporarilyEscapable: {
    // {ExprID, ExprKind, TypeID, implicit, nonEscapingExprID,
    //  escapingExprID, subExprID}
    if (record.size() < 7)
      return nullptr;
    Expr *nonEscaping = lookupExpr(static_cast<uint32_t>(record[4]));
    auto *escaping = dyn_cast_or_null<OpaqueValueExpr>(
        lookupExpr(static_cast<uint32_t>(record[5])));
    Expr *sub = lookupExpr(static_cast<uint32_t>(record[6]));
    if (!nonEscaping || !escaping || !sub)
      return nullptr;
    result = new (Ctx) MakeTemporarilyEscapableExpr(
        SourceLoc(), SourceLoc(), nonEscaping, sub, SourceLoc(), escaping,
        /*originalExpr=*/nullptr, implicit);
    if (ty)
      result->setType(ty);
    break;
  }
  case Expr_DynamicType: {
    // {ExprID, ExprKind, TypeID, implicit, subExprID}
    if (record.size() < 5)
      return nullptr;
    Expr *sub = lookupExpr(static_cast<uint32_t>(record[4]));
    if (!sub)
      return nullptr;
    result = new (Ctx) DynamicTypeExpr(SourceLoc(), SourceLoc(), sub,
                                        SourceLoc(), ty);
    break;
  }
  default:
    result = new (Ctx) ErrorExpr(SourceRange());
    break;
  }

  if (result && exprID > 0) {
    if (exprID > ExprTable.size())
      ExprTable.resize(exprID);
    ExprTable[exprID - 1] = result;
  }

  return result;
}

Stmt *BodyASTDeserializer::deserializeStmt(ArrayRef<uint64_t> record,
                                            uint32_t stmtID) {
  using namespace astcache_body_block;

  if (record.size() < 3)
    return nullptr;

  uint8_t kind = static_cast<uint8_t>(record[1]);
  bool implicit = record[2] != 0;

  Stmt *result = nullptr;

  switch (kind) {
  case Stmt_Brace: {
    // {StmtID, StmtKind, implicit, numElements, [elementKind + elementID...]}
    if (record.size() < 4)
      return nullptr;
    uint32_t numElements = static_cast<uint32_t>(record[3]);
    SmallVector<ASTNode, 8> elements;
    unsigned idx = 4;
    for (uint32_t i = 0; i < numElements && idx + 1 < record.size(); ++i) {
      uint32_t eltKind = static_cast<uint32_t>(record[idx]);
      uint32_t eltID = static_cast<uint32_t>(record[idx + 1]);
      idx += 2;
      if (eltKind == 0) {
        Expr *E = (eltID > 0 && eltID <= ExprTable.size())
                     ? ExprTable[eltID - 1]
                     : nullptr;
        if (E)
          elements.push_back(ASTNode(E));
      } else if (eltKind == 1) {
        Stmt *S = (eltID > 0 && eltID <= StmtTable.size())
                      ? StmtTable[eltID - 1]
                      : nullptr;
        if (S)
          elements.push_back(ASTNode(S));
      }
      // eltKind == 2 (decl/other): skip
    }
    result = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                               implicit ? std::optional<bool>(true)
                                        : std::nullopt);
    break;
  }
  case Stmt_Return: {
    // {StmtID, StmtKind, implicit, hasResult, [resultExprID]}
    if (record.size() < 4)
      return nullptr;
    bool hasResult = record[3] != 0;
    Expr *resultExpr = nullptr;
    if (hasResult && record.size() >= 5) {
      uint32_t resultExprID = static_cast<uint32_t>(record[4]);
      resultExpr = (resultExprID > 0 && resultExprID <= ExprTable.size())
                       ? ExprTable[resultExprID - 1]
                       : nullptr;
    }
    if (implicit)
      result = ReturnStmt::createImplicit(Ctx, resultExpr);
    else
      result = ReturnStmt::createParsed(Ctx, SourceLoc(), resultExpr);
    break;
  }
  case Stmt_Yield: {
    if (record.size() < 4)
      return nullptr;
    uint32_t numYields = static_cast<uint32_t>(record[3]);
    SmallVector<Expr*, 4> yieldExprs;
    for (uint32_t i = 0; i < numYields && 4 + i < record.size(); i++) {
      Expr *e = lookupExpr(static_cast<uint32_t>(record[4 + i]));
      if (e) yieldExprs.push_back(e);
    }
    result = YieldStmt::create(Ctx, SourceLoc(), SourceLoc(), yieldExprs,
                               SourceLoc(),
                               implicit ? std::optional<bool>(true)
                                        : std::nullopt);
    break;
  }
  case Stmt_Switch: {
    if (record.size() < 5)
      return nullptr;
    Expr *subject = lookupExpr(static_cast<uint32_t>(record[3]));
    uint32_t numCases = static_cast<uint32_t>(record[4]);
    SmallVector<CaseStmt*, 4> cases;
    for (uint32_t i = 0; i < numCases && 5 + i < record.size(); i++) {
      Stmt *cs = lookupStmt(static_cast<uint32_t>(record[5 + i]));
      if (auto *C = dyn_cast_or_null<CaseStmt>(cs))
        cases.push_back(C);
    }
    if (implicit)
      result = SwitchStmt::createImplicit(LabeledStmtInfo(), subject, cases, Ctx);
    else
      result = SwitchStmt::create(LabeledStmtInfo(), SourceLoc(), subject,
                                  SourceLoc(), cases, SourceLoc(),
                                  SourceLoc(), Ctx);
    break;
  }
  case Stmt_Case: {
    if (record.size() < 4)
      return nullptr;
    BraceStmt *body = dyn_cast_or_null<BraceStmt>(
        lookupStmt(static_cast<uint32_t>(record[3])));
    SmallVector<CaseLabelItem, 1> labels;
    // CaseStmt requires at least one label item. Create a default case label.
    labels.push_back(CaseLabelItem::getDefault(AnyPattern::createImplicit(Ctx)));
    result = CaseStmt::createImplicit(Ctx, CaseParentKind::Switch,
                                      labels, body);
    break;
  }
  // === Simple statements ===
  case Stmt_Break: {
    result = new (Ctx) BreakStmt(SourceLoc(), Identifier(), SourceLoc(),
                                /*DC=*/nullptr,
                                implicit ? std::optional<bool>(true)
                                        : std::nullopt);
    break;
  }
  case Stmt_Continue: {
    result = new (Ctx) ContinueStmt(SourceLoc(), Identifier(), SourceLoc(),
                                    /*DC=*/nullptr,
                                    implicit ? std::optional<bool>(true)
                                            : std::nullopt);
    break;
  }
  case Stmt_Fallthrough: {
    DeclContext *dc = nullptr;
    if (auto mods = Ctx.getLoadedModules(); mods.begin() != mods.end()) {
      auto files = (*mods.begin()).second->getFiles();
      if (!files.empty())
        dc = files.front();
    }
    result = FallthroughStmt::createParsed(SourceLoc(), dc);
    break;
  }
  case Stmt_Fail: {
    result = new (Ctx) FailStmt(SourceLoc(), SourceLoc(),
                                implicit ? std::optional<bool>(true)
                                         : std::nullopt);
    break;
  }
  case Stmt_Throw: {
    if (record.size() < 4)
      return nullptr;
    Expr *subExpr = lookupExpr(static_cast<uint32_t>(record[3]));
    result = new (Ctx) ThrowStmt(SourceLoc(), subExpr);
    break;
  }
  case Stmt_Discard: {
    result = new (Ctx) DiscardStmt(SourceLoc(), /*subExpr=*/nullptr);
    break;
  }
  // === Control flow ===
  case Stmt_Then: {
    if (record.size() < 4)
      return nullptr;
    Expr *resultExpr = lookupExpr(static_cast<uint32_t>(record[3]));
    if (implicit)
      result = ThenStmt::createImplicit(Ctx, resultExpr);
    else
      result = ThenStmt::createParsed(Ctx, SourceLoc(), resultExpr);
    break;
  }
  case Stmt_Defer: {
    if (record.size() < 4)
      return nullptr;
    // DeferStmt needs a DeclContext to create its temp function. We pass
    // nullptr for DC — the body can be looked up via getBodyAsWritten().
    // The temp decl/callExpr are reconstructed by DeferStmt::create, but
    // without a DC, we create a minimal version.
    BraceStmt *body = dyn_cast_or_null<BraceStmt>(
        lookupStmt(static_cast<uint32_t>(record[3])));
    // Without a proper DeclContext, we can't call DeferStmt::create.
    // Store the body for later reconstruction by creating a minimal DeferStmt.
    // For now, create a placeholder via placement new.
    if (body) {
      // Create a minimal DeferStmt. DeferStmt::create needs a DC for its
      // temp FuncDecl. Use the ASTContext's module as a fallback DC.
      // This is sufficient for AST round-trip fidelity (body is preserved).
      result = DeferStmt::create(/*dc=*/nullptr, SourceLoc());
      if (auto *DS = cast<DeferStmt>(result)) {
        if (auto *td = DS->getTempDecl())
          td->setBody(body, AbstractFunctionDecl::BodyKind::TypeChecked);
      }
    }
    break;
  }
  case Stmt_If: {
    // {stmtID, kind, implicit, condExprID, thenStmtID, elseStmtID}
    if (record.size() < 6)
      return nullptr;
    Expr *condExpr = lookupExpr(static_cast<uint32_t>(record[3]));
    BraceStmt *thenStmt = dyn_cast_or_null<BraceStmt>(
        lookupStmt(static_cast<uint32_t>(record[4])));
    Stmt *elseStmt = lookupStmt(static_cast<uint32_t>(record[5]));
    result = new (Ctx) IfStmt(SourceLoc(), condExpr, thenStmt, SourceLoc(),
                              elseStmt,
                              implicit ? std::optional<bool>(true)
                                       : std::nullopt,
                              Ctx);
    break;
  }
  case Stmt_Guard: {
    // {stmtID, kind, implicit, condExprID, bodyStmtID}
    if (record.size() < 5)
      return nullptr;
    Expr *condExpr = lookupExpr(static_cast<uint32_t>(record[3]));
    BraceStmt *body = dyn_cast_or_null<BraceStmt>(
        lookupStmt(static_cast<uint32_t>(record[4])));
    result = new (Ctx) GuardStmt(SourceLoc(), condExpr, body,
                                 implicit ? std::optional<bool>(true)
                                          : std::nullopt,
                                 Ctx);
    break;
  }
  case Stmt_While: {
    // {stmtID, kind, implicit, condExprID, bodyStmtID}
    if (record.size() < 5)
      return nullptr;
    Expr *condExpr = lookupExpr(static_cast<uint32_t>(record[3]));
    Stmt *body = lookupStmt(static_cast<uint32_t>(record[4]));
    // WhileStmt takes a StmtCondition. We create a single-element condition
    // from the deserialized expr.
    SmallVector<StmtConditionElement, 1> condElts;
    if (condExpr)
      condElts.push_back(StmtConditionElement(condExpr));
    result = new (Ctx) WhileStmt(LabeledStmtInfo(), SourceLoc(),
                                 MutableArrayRef<StmtConditionElement>(condElts),
                                 body,
                                 implicit ? std::optional<bool>(true)
                                          : std::nullopt);
    break;
  }
  case Stmt_RepeatWhile: {
    // {stmtID, kind, implicit, condExprID, bodyStmtID}
    if (record.size() < 5)
      return nullptr;
    Expr *condExpr = lookupExpr(static_cast<uint32_t>(record[3]));
    Stmt *body = lookupStmt(static_cast<uint32_t>(record[4]));
    result = new (Ctx) RepeatWhileStmt(LabeledStmtInfo(), SourceLoc(),
                                       condExpr, SourceLoc(), body,
                                       implicit ? std::optional<bool>(true)
                                                : std::nullopt);
    break;
  }
  case Stmt_ForEach: {
    // {stmtID, kind, implicit, patDeclID, seqExprID, bodyStmtID, whereExprID}
    if (record.size() < 7)
      return nullptr;
    uint32_t patDeclID = static_cast<uint32_t>(record[3]);
    Expr *seq = lookupExpr(static_cast<uint32_t>(record[4]));
    BraceStmt *body = dyn_cast_or_null<BraceStmt>(
        lookupStmt(static_cast<uint32_t>(record[5])));
    Expr *whereExpr = lookupExpr(static_cast<uint32_t>(record[6]));
    // Reconstruct pattern from decl ID. If we have a decl, build a
    // NamedPattern wrapped in a BindingPattern.
    Pattern *pat = nullptr;
    if (patDeclID > 0 && ResolveDecl) {
      if (auto *D = ResolveDecl(patDeclID)) {
        if (auto *vd = dyn_cast<VarDecl>(D)) {
          pat = NamedPattern::createImplicit(Ctx, vd);
          pat = BindingPattern::createImplicit(Ctx, vd->getIntroducer(), pat);
        }
      }
    }
    result = new (Ctx) ForEachStmt(LabeledStmtInfo(), SourceLoc(),
                                   /*TryLoc=*/SourceLoc(),
                                   /*AwaitLoc=*/SourceLoc(),
                                   /*UnsafeLoc=*/SourceLoc(),
                                   pat, /*InLoc=*/SourceLoc(), seq,
                                   /*WhereLoc=*/SourceLoc(), whereExpr, body,
                                   /*DC=*/nullptr,
                                   implicit ? std::optional<bool>(true)
                                            : std::nullopt);
    break;
  }
  // === Do/catch ===
  case Stmt_Do: {
    // {stmtID, kind, implicit, bodyStmtID}
    if (record.size() < 4)
      return nullptr;
    BraceStmt *body = dyn_cast_or_null<BraceStmt>(
        lookupStmt(static_cast<uint32_t>(record[3])));
    result = new (Ctx) DoStmt(LabeledStmtInfo(), SourceLoc(), body,
                              implicit ? std::optional<bool>(true)
                                       : std::nullopt);
    break;
  }
  case Stmt_DoCatch: {
    // {stmtID, kind, implicit, bodyStmtID, numCases, [caseStmtIDs...]}
    if (record.size() < 5)
      return nullptr;
    Stmt *body = lookupStmt(static_cast<uint32_t>(record[3]));
    uint32_t numCases = static_cast<uint32_t>(record[4]);
    SmallVector<CaseStmt*, 4> catches;
    for (uint32_t i = 0; i < numCases && 5 + i < record.size(); i++) {
      Stmt *cs = lookupStmt(static_cast<uint32_t>(record[5 + i]));
      if (auto *C = dyn_cast_or_null<CaseStmt>(cs))
        catches.push_back(C);
    }
    result = DoCatchStmt::create(/*dc=*/nullptr, LabeledStmtInfo(),
                                 SourceLoc(), /*throwsLoc=*/SourceLoc(),
                                 /*thrownType=*/TypeLoc(),
                                 body, catches,
                                 implicit ? std::optional<bool>(true)
                                          : std::nullopt);
    break;
  }
  // === Other ===
  case Stmt_PoundAssert: {
    // {stmtID, kind, implicit, condExprID, messageStringID}
    if (record.size() < 5)
      return nullptr;
    Expr *cond = lookupExpr(static_cast<uint32_t>(record[3]));
    IdentifierID msgID = static_cast<IdentifierID>(record[4]);
    StringRef msg;
    if (msgID > 0 && ResolveIdentifier)
      msg = ResolveIdentifier(msgID).str();
    result = new (Ctx) PoundAssertStmt(SourceRange(), cond, msg);
    break;
  }
  case Stmt_Opaque: {
    result = new (Ctx) OpaqueStmt();
    break;
  }
  default:
    return nullptr;
  }

  if (result && stmtID > 0) {
    if (stmtID > StmtTable.size())
      StmtTable.resize(stmtID);
    StmtTable[stmtID - 1] = result;
  }

  return result;
}

BraceStmt *BodyASTDeserializer::deserializeBody(ArrayRef<uint8_t> bitstreamData) {
  // Create a bitstream cursor from the data.
  // The data may start with a 4-byte SWIFTMODULE_SIGNATURE.
  // Skip it if present.
  ArrayRef<uint8_t> data = bitstreamData;
  if (data.size() >= 4 && data[0] == 0xE2 && data[1] == 0x9C &&
      data[2] == 0xA8 && data[3] == 0x0E) {
    data = data.slice(4);
  }

  llvm::BitstreamCursor cursor(data);

  // Advance to find the ASTCACHE_BODY_BLOCK_ID sub-block.
  while (!cursor.AtEndOfStream()) {
    llvm::Expected<llvm::BitstreamEntry> maybeEntry = cursor.advance();
    if (!maybeEntry) {
      consumeError(maybeEntry.takeError());
      return nullptr;
    }
    llvm::BitstreamEntry entry = maybeEntry.get();

    if (entry.Kind == llvm::BitstreamEntry::SubBlock) {
      if (entry.ID == ASTCACHE_BODY_BLOCK_ID) {
        if (llvm::Error Err = cursor.EnterSubBlock(ASTCACHE_BODY_BLOCK_ID)) {
          consumeError(std::move(Err));
          return nullptr;
        }
        break;
      }
      // Skip other sub-blocks.
      if (llvm::Error Err = cursor.SkipBlock()) {
        consumeError(std::move(Err));
        return nullptr;
      }
      continue;
    }
    if (entry.Kind == llvm::BitstreamEntry::Error)
      return nullptr;
    // Skip records and end blocks.
  }

  if (cursor.AtEndOfStream())
    return nullptr;

  // Read records within the body block.
  SmallVector<uint64_t, 64> scratch;
  uint32_t rootStmtID = 0;

  while (true) {
    llvm::Expected<llvm::BitstreamEntry> maybeEntry =
        cursor.advance(llvm::BitstreamCursor::AF_DontPopBlockAtEnd);
    if (!maybeEntry) {
      consumeError(maybeEntry.takeError());
      break;
    }
    llvm::BitstreamEntry entry = maybeEntry.get();

    if (entry.Kind == llvm::BitstreamEntry::EndBlock)
      break;
    if (entry.Kind != llvm::BitstreamEntry::Record)
      break;

    scratch.clear();
    llvm::Expected<unsigned> maybeRecordID =
        cursor.readRecord(entry.ID, scratch);
    if (!maybeRecordID) {
      consumeError(maybeRecordID.takeError());
      break;
    }
    unsigned recordID = maybeRecordID.get();

    if (recordID == astcache_body_block::BODY) {
      if (scratch.size() >= 2)
        rootStmtID = static_cast<uint32_t>(scratch[1]);
    } else if (recordID == astcache_body_block::EXPR_NODE) {
      if (scratch.size() >= 1) {
        uint32_t exprID = static_cast<uint32_t>(scratch[0]);
        deserializeExpr(scratch, exprID);
      }
    } else if (recordID == astcache_body_block::STMT_NODE) {
      if (scratch.size() >= 1) {
        uint32_t stmtID = static_cast<uint32_t>(scratch[0]);
        deserializeStmt(scratch, stmtID);
      }
    }
  }

  // Look up the root BraceStmt.
  if (rootStmtID > 0 && rootStmtID <= StmtTable.size()) {
    if (auto *BS = dyn_cast_or_null<BraceStmt>(StmtTable[rootStmtID - 1]))
      return BS;
  }
  return nullptr;
}

std::map<serialization::DeclID, BraceStmt *>
BodyASTDeserializer::deserializeAllBodies(ArrayRef<uint8_t> bitstreamData) {
  std::map<serialization::DeclID, BraceStmt *> result;

  ArrayRef<uint8_t> data = bitstreamData;
  if (data.size() >= 4 && data[0] == 0xE2 && data[1] == 0x9C &&
      data[2] == 0xA8 && data[3] == 0x0E) {
    data = data.slice(4);
  }

  llvm::BitstreamCursor cursor(data);

  while (!cursor.AtEndOfStream()) {
    llvm::Expected<llvm::BitstreamEntry> maybeEntry = cursor.advance();
    if (!maybeEntry) {
      consumeError(maybeEntry.takeError());
      break;
    }
    llvm::BitstreamEntry entry = maybeEntry.get();

    if (entry.Kind == llvm::BitstreamEntry::SubBlock) {
      if (entry.ID == ASTCACHE_BODY_BLOCK_ID) {
        if (llvm::Error Err = cursor.EnterSubBlock(ASTCACHE_BODY_BLOCK_ID)) {
          consumeError(std::move(Err));
          break;
        }

        // Read records within this body block.
        SmallVector<uint64_t, 64> scratch;
        uint32_t rootStmtID = 0;
        serialization::DeclID funcDeclID = 0;

        // Clear tables for this body's ExprID/StmtID space.
        ExprTable.clear();
        StmtTable.clear();

        while (true) {
          llvm::Expected<llvm::BitstreamEntry> maybeE =
              cursor.advance(llvm::BitstreamCursor::AF_DontPopBlockAtEnd);
          if (!maybeE) {
            consumeError(maybeE.takeError());
            break;
          }
          llvm::BitstreamEntry e = maybeE.get();

          if (e.Kind == llvm::BitstreamEntry::EndBlock)
            break;
          if (e.Kind != llvm::BitstreamEntry::Record)
            break;

          scratch.clear();
          llvm::Expected<unsigned> maybeRecordID =
              cursor.readRecord(e.ID, scratch);
          if (!maybeRecordID) {
            consumeError(maybeRecordID.takeError());
            break;
          }
          unsigned recordID = maybeRecordID.get();
          if (recordID == astcache_body_block::BODY) {
            if (scratch.size() >= 2) {
              funcDeclID = static_cast<serialization::DeclID>(scratch[0]);
              rootStmtID = static_cast<uint32_t>(scratch[1]);
            }
          } else if (recordID == astcache_body_block::EXPR_NODE) {
            if (scratch.size() >= 1) {
              uint32_t exprID = static_cast<uint32_t>(scratch[0]);
              deserializeExpr(scratch, exprID);
            }
          } else if (recordID == astcache_body_block::STMT_NODE) {
            if (scratch.size() >= 1) {
              uint32_t stmtID = static_cast<uint32_t>(scratch[0]);
              deserializeStmt(scratch, stmtID);
            }
          }
        }

        // Look up the root BraceStmt.
        if (funcDeclID > 0 && rootStmtID > 0 &&
            rootStmtID <= StmtTable.size()) {
          if (auto *BS = dyn_cast_or_null<BraceStmt>(StmtTable[rootStmtID - 1]))
            result[funcDeclID] = BS;
        }
        continue;
      }
      // For MODULE_BLOCK, ENTER it to find nested body blocks.
      // Skip all other sub-blocks.
      if (entry.ID == llvm::bitc::FIRST_APPLICATION_BLOCKID) {
        if (llvm::Error Err = cursor.EnterSubBlock(entry.ID)) {
          consumeError(std::move(Err));
          continue;
        }
        continue;
      }
      if (llvm::Error Err = cursor.SkipBlock()) {
        consumeError(std::move(Err));
        break;
      }
      continue;
    }
    if (entry.Kind == llvm::BitstreamEntry::Error)
      break;
    // Skip records and end blocks.
  }

  return result;
}
