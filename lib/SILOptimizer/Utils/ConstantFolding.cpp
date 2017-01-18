//===--- ConstantFolding.cpp - Utils for SIL constant folding -------------===//
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

#include "swift/SILOptimizer/Utils/ConstantFolding.h"

using namespace swift;

APInt swift::constantFoldBitOperation(APInt lhs, APInt rhs, BuiltinValueKind ID) {
  switch (ID) {
    default: llvm_unreachable("Not all cases are covered!");
    case BuiltinValueKind::And:
      return lhs.And(rhs);
    case BuiltinValueKind::AShr:
      return lhs.ashr(rhs);
    case BuiltinValueKind::LShr:
      return lhs.lshr(rhs);
    case BuiltinValueKind::Or:
      return lhs.Or(rhs);
    case BuiltinValueKind::Shl:
      return lhs.shl(rhs);
    case BuiltinValueKind::Xor:
      return lhs.Xor(rhs);
  }
}

APInt swift::constantFoldComparison(APInt lhs, APInt rhs, BuiltinValueKind ID) {
  bool result;
  switch (ID) {
    default: llvm_unreachable("Invalid integer compare kind");
    case BuiltinValueKind::ICMP_EQ:  result = lhs == rhs; break;
    case BuiltinValueKind::ICMP_NE:  result = lhs != rhs; break;
    case BuiltinValueKind::ICMP_SLT: result = lhs.slt(rhs); break;
    case BuiltinValueKind::ICMP_SGT: result = lhs.sgt(rhs); break;
    case BuiltinValueKind::ICMP_SLE: result = lhs.sle(rhs); break;
    case BuiltinValueKind::ICMP_SGE: result = lhs.sge(rhs); break;
    case BuiltinValueKind::ICMP_ULT: result = lhs.ult(rhs); break;
    case BuiltinValueKind::ICMP_UGT: result = lhs.ugt(rhs); break;
    case BuiltinValueKind::ICMP_ULE: result = lhs.ule(rhs); break;
    case BuiltinValueKind::ICMP_UGE: result = lhs.uge(rhs); break;
  }
  return APInt(1, result);
}

APInt swift::constantFoldBinaryWithOverflow(APInt lhs, APInt rhs,
                                            bool &Overflow,
                                            llvm::Intrinsic::ID ID) {
  switch (ID) {
    default: llvm_unreachable("Invalid case");
    case llvm::Intrinsic::sadd_with_overflow:
      return lhs.sadd_ov(rhs, Overflow);
    case llvm::Intrinsic::uadd_with_overflow:
      return lhs.uadd_ov(rhs, Overflow);
    case llvm::Intrinsic::ssub_with_overflow:
      return lhs.ssub_ov(rhs, Overflow);
    case llvm::Intrinsic::usub_with_overflow:
      return lhs.usub_ov(rhs, Overflow);
    case llvm::Intrinsic::smul_with_overflow:
      return lhs.smul_ov(rhs, Overflow);
    case llvm::Intrinsic::umul_with_overflow:
      return lhs.umul_ov(rhs, Overflow);
  }
}

APInt swift::constantFoldDiv(APInt lhs, APInt rhs, bool &Overflow,
                             BuiltinValueKind ID) {
  assert(rhs != 0 && "division by zero");
  switch (ID) {
    default : llvm_unreachable("Invalid case");
    case BuiltinValueKind::SDiv:
      return lhs.sdiv_ov(rhs, Overflow);
    case BuiltinValueKind::SRem:
      // Check for overflow
      lhs.sdiv_ov(rhs, Overflow);
      return lhs.srem(rhs);
    case BuiltinValueKind::UDiv:
      Overflow = false;
      return lhs.udiv(rhs);
    case BuiltinValueKind::URem:
      Overflow = false;
      return lhs.urem(rhs);
  }
}

APInt swift::constantFoldCast(APInt val, const BuiltinInfo &BI) {
  // Get the cast result.
  Type SrcTy = BI.Types[0];
  Type DestTy = BI.Types.size() == 2 ? BI.Types[1] : Type();
  uint32_t SrcBitWidth =
  SrcTy->castTo<BuiltinIntegerType>()->getGreatestWidth();
  uint32_t DestBitWidth =
  DestTy->castTo<BuiltinIntegerType>()->getGreatestWidth();
  
  APInt CastResV;
  if (SrcBitWidth == DestBitWidth) {
    return val;
  } else switch (BI.ID) {
    default : llvm_unreachable("Invalid case.");
    case BuiltinValueKind::Trunc:
    case BuiltinValueKind::TruncOrBitCast:
      return val.trunc(DestBitWidth);
    case BuiltinValueKind::ZExt:
    case BuiltinValueKind::ZExtOrBitCast:
      return val.zext(DestBitWidth);
      break;
    case BuiltinValueKind::SExt:
    case BuiltinValueKind::SExtOrBitCast:
      return val.sext(DestBitWidth);
  }
}
