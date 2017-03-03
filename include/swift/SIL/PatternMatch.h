//===--- PatternMatch.h - SIL Pattern Matching Infrastructure ---*- C++ -*-===//
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
// This file provides a simple and efficient mechanism for performing general
// tree-based pattern matches on SIL. The power of these routines is that it
// allows you to write concise patterns that are expressive and easy to
// understand. The other major advantage of this is that it allows you to
// trivially capture/bind elements in the pattern to variables.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_PATTERNMATCH_H
#define SWIFT_SIL_PATTERNMATCH_H

#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILUndef.h"
namespace swift {

class SILInstruction;

namespace PatternMatch {

//===----------------------------------------------------------------------===//
//                       Basic Matching Infrastructure
//===----------------------------------------------------------------------===//

/// Applies the given pattern to V.
template<typename Val, typename Pattern>
bool match(Val *V, const Pattern &P) {
  return const_cast<Pattern &>(P).match(V);
}

/// Explicit template instantiation for SILValue so we can access the value
/// inside.
template<typename Pattern>
bool match(SILValue V, const Pattern &P) {
  return const_cast<Pattern &>(P).match(&*V);
}

template<typename SubPatternTy>
struct OneUse_match {
  SubPatternTy SubPattern;

  OneUse_match(const SubPatternTy &SP) : SubPattern(SP) {}

  template<typename OpTy>
  bool match(OpTy *V) {
    return V->hasOneUse() && SubPattern.match(V);
  }
};

/// Match if the input has one use and satisfies the given subpattern.
template<typename SubPatternTy>
inline OneUse_match<SubPatternTy> m_OneUse(const SubPatternTy &SubPattern) {
  return SubPattern;
}

template<typename Class>
struct class_match {
  template<typename ITy>
  bool match(ITy *V) { return isa<Class>(V); }
};

template<typename Class>
struct bind_ty {
  Class *&VR;
  bind_ty(Class *&V) : VR(V) {}

  template<typename ITy>
  bool match(ITy *V) {
    if (Class *CV = dyn_cast<Class>(V)) {
      VR = CV;
      return true;
    }
    return false;
  }
};

//===----------------------------------------------------------------------===//
//                            Matching Combinators
//===----------------------------------------------------------------------===//

template<typename LTy, typename RTy>
struct match_combine_or {
  LTy L;
  RTy R;

  match_combine_or(const LTy &Left, const RTy &Right) : L(Left), R(Right) { }

  template<typename ITy>
  bool match(ITy *V) {
    if (L.match(V))
      return true;
    if (R.match(V))
      return true;
    return false;
  }
};

template<typename LTy, typename RTy>
struct match_combine_and {
  LTy L;
  RTy R;

  match_combine_and(const LTy &Left, const RTy &Right) : L(Left), R(Right) { }

  template<typename ITy>
  bool match(ITy *V) {
    if (L.match(V))
      if (R.match(V))
        return true;
    return false;
  }
};

/// Combine two pattern matchers matching L || R
template<typename LTy, typename RTy>
inline match_combine_or<LTy, RTy> m_CombineOr(const LTy &L, const RTy &R) {
  return match_combine_or<LTy, RTy>(L, R);
}

/// Combine two pattern matchers matching L && R
template<typename LTy, typename RTy>
inline match_combine_and<LTy, RTy> m_CombineAnd(const LTy &L, const RTy &R) {
  return match_combine_and<LTy, RTy>(L, R);
}

/// Helper class to track the return type of vararg m_CombineOr matcher.
template <typename ...Arguments>
struct OneOf_match;

template <typename T0>
struct OneOf_match<T0> {
  typedef T0 Ty;
};

template <typename T0, typename T1>
struct OneOf_match<T0, T1> {
  typedef match_combine_or<T0, T1> Ty;
};

template <typename T0, typename T1, typename ...Arguments>
struct OneOf_match<T0, T1, Arguments ...> {
  typedef typename OneOf_match<match_combine_or<T0, T1>, Arguments ...>::Ty Ty;
};

/// This is a vararg version of m_CombineOr. It is a boolean "or" of
/// matchers provided as its parameters.
template<typename LTy, typename RTy, typename ...RTys>
inline typename OneOf_match<LTy, RTy, RTys...>::Ty
m_CombineOr(const LTy &L, const RTy &R, const RTys &...Args) {
  return m_CombineOr(m_CombineOr(L, R), Args...);
}

/// boolean or operator for combining matchers using
/// a conventional || syntax.
template<typename T1, typename T2, typename T3, typename T4>
inline typename OneOf_match<match_combine_and<T1, T2>,
                            match_combine_and<T3, T4>>::Ty
operator || (const match_combine_and<T1, T2> &Op1,
             const match_combine_and<T3, T4> &Op2) {
  return m_CombineOr(Op1, Op2);
}

template<typename T1, typename T2, typename T3, typename T4>
inline typename OneOf_match<match_combine_or<T1, T2>,
                            match_combine_and<T3, T4>>::Ty
operator || (const match_combine_or<T1, T2> &Op1,
             const match_combine_and<T3, T4> &Op2) {
  return m_CombineOr(Op1, Op2);
}

//===----------------------------------------------------------------------===//
//                               Base Matchers
//===----------------------------------------------------------------------===//

/// Match an arbitrary ValueBase, and if the match was successful do not
/// capture.
inline class_match<ValueBase> m_ValueBase() {
  return class_match<ValueBase>();
}

/// Match an arbitrary ValueBase, capturing the ValueBase if the match succeeds.
inline bind_ty<ValueBase> m_ValueBase(ValueBase *&V) {
  return V;
}

struct silvalue_bind {
  SILValue &Value;

  silvalue_bind(SILValue &V) : Value(V) { }

  template <typename ITy>
  bool match(ITy V) {
    return false;
  }

  bool match(SILValue V) {
    Value = V;
    return true;
  }
};

/// Match an arbitrary SILValue, capturing the SILValue if the match succeeds.
inline silvalue_bind m_SILValue(SILValue &V) {
  return V;
}

struct specificval_ty {
  const ValueBase *Val;
  specificval_ty(const ValueBase *V) : Val(V) {}

  template<typename ITy>
  bool match(ITy *V) {
    return V == Val;
  }
};

/// Return a matcher which only matches on inputs that satisfy pointer equality
/// with V.
inline specificval_ty m_Specific(const ValueBase *V) { return V; }

/// Define class_match and bind_ty matchers for all VALUE SILNodes.
///
/// class_match matchers match arbitrary Class * and do not capture on success.
/// bind_ty matchers match arbitrary Class * and do capture on success.
#define VALUE(Class, Parent)                                             \
  inline class_match<Class> m_##Class() { return class_match<Class>(); } \
  inline bind_ty<Class> m_##Class(Class *&V) { return V; }
#include "swift/SIL/SILNodes.def"

static inline APInt extendAPInt(const APInt &A, unsigned bitWidth,
                                bool isSigned) {
  if (isSigned)
    return A.sext(bitWidth);
  return A.zext(bitWidth);
}

static inline bool isSameAPIntValue(const APInt &I1, const APInt &I2,
                                    bool isSigned) {
  if (I1.getBitWidth() == I2.getBitWidth())
    return I1 == I2;

  if (I1.getBitWidth() > I2.getBitWidth())
    return I1 == extendAPInt(I2, I1.getBitWidth(), isSigned);

  return extendAPInt(I1, I2.getBitWidth(), isSigned) == I2;
}

// Builtin Integer Matcher
struct integerliteral_ty {
  APInt Value;
  bool isSigned;
  integerliteral_ty(APInt V, bool S) : Value(V), isSigned(S) { }

  template<typename ITy>
  bool match(ITy *V) {
    auto *Literal = dyn_cast<IntegerLiteralInst>(V);
    if (!Literal)
      return false;

    // This should eventually be refactored into APInt::isSameValue by giving it
    // a signed flag.
    return isSameAPIntValue(Value, Literal->getValue(), isSigned);
  }
};

static inline integerliteral_ty m_IntegerLiteralInst(APInt V, bool isSigned) {
  return {V, isSigned};
}

template <uint64_t value>
struct match_integer {
  template<typename ITy>
  bool match(ITy *V) {
    auto *Literal = dyn_cast<IntegerLiteralInst>(V);
    if (!Literal)
      return false;
    return Literal->getValue() == value;
  }
};

using m_Zero = match_integer<0>;
using m_One = match_integer<1>;

//===----------------------------------------------------------------------===//
//                             Unary Instructions
//===----------------------------------------------------------------------===//

template<typename OpMatchTy, ValueKind Kind>
struct UnaryOp_match {
  OpMatchTy OpMatch;

  UnaryOp_match(const OpMatchTy &Op) : OpMatch(Op) { }

  template<typename OpTy>
  bool match(OpTy *V) {
    if (V->getKind() != Kind)
      return false;

    auto *I = dyn_cast<SILInstruction>(V);
    if (!I || I->getNumOperands() != 1)
      return false;

    return OpMatch.match(I->getOperand(0));
  }
};

// XMacro for generating a matcher for unary op instructions that can apply
// further matchers to the operands of the unary operation.
#define UNARY_OP_MATCH_WITH_ARG_MATCHER(Class)        \
  template <typename Ty>                              \
  UnaryOp_match<Ty, ValueKind::Class>                 \
  m_##Class(const Ty &T) {                            \
    return T;                                         \
  }
UNARY_OP_MATCH_WITH_ARG_MATCHER(AllocRefDynamicInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(LoadWeakInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(ConvertFunctionInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(UpcastInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(PointerToAddressInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(AddressToPointerInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(UncheckedRefCastInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(UncheckedAddrCastInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(UncheckedTrivialBitCastInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(UncheckedBitwiseCastInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(RawPointerToRefInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(RefToUnownedInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(UnownedToRefInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(RefToUnmanagedInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(UnmanagedToRefInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(ThinToThickFunctionInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(ThickToObjCMetatypeInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(ObjCToThickMetatypeInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(ObjCMetatypeToObjectInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(ObjCExistentialMetatypeToObjectInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(IsNonnullInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(RetainValueInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(ReleaseValueInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(AutoreleaseValueInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(UncheckedEnumDataInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(InitEnumDataAddrInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(InjectEnumAddrInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(UncheckedTakeEnumDataAddrInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(ValueMetatypeInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(ExistentialMetatypeInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(TupleExtractInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(TupleElementAddrInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(StructExtractInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(StructElementAddrInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(LoadInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(RefElementAddrInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(ClassMethodInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(SuperMethodInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(DynamicMethodInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(OpenExistentialAddrInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(OpenExistentialRefInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(OpenExistentialOpaqueInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(InitExistentialAddrInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(InitExistentialOpaqueInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(InitExistentialRefInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(DeinitExistentialAddrInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(DeinitExistentialOpaqueInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(ProjectBlockStorageInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(StrongRetainInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(StrongReleaseInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(StrongRetainUnownedInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(UnownedRetainInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(UnownedReleaseInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(FixLifetimeInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(CopyBlockInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(DeallocStackInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(DeallocRefInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(DeallocPartialRefInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(DeallocBoxInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(DestroyAddrInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(CondFailInst)
UNARY_OP_MATCH_WITH_ARG_MATCHER(ReturnInst)
#undef UNARY_OP_MATCH_WITH_ARG_MATCHER

//===----------------------------------------------------------------------===//
//                            Binary Instructions
//===----------------------------------------------------------------------===//

template<typename LHSTy, typename RHSTy, ValueKind Kind>
struct BinaryOp_match {
  LHSTy L;
  RHSTy R;

  BinaryOp_match(const LHSTy &LHS, const RHSTy &RHS) : L(LHS), R(RHS) {}

  template<typename OpTy>
  bool match(OpTy *V) {
    if (V->getKind() != Kind)
      return false;

    auto *I = dyn_cast<SILInstruction>(V);
    if (!I || I->getNumOperands() != 2)
      return false;

    return L.match((ValueBase *)I->getOperand(0)) &&
           R.match((ValueBase *)I->getOperand(1));
  }
};

template <typename LTy, typename RTy>
BinaryOp_match<LTy, RTy, ValueKind::IndexRawPointerInst>
m_IndexRawPointerInst(const LTy &Left, const RTy &Right) {
  return {Left, Right};
}

//===----------------------------------------------------------------------===//
//                         Address/Struct Projections
//===----------------------------------------------------------------------===//

template <typename LTy>
struct tupleextract_ty {
  LTy L;
  unsigned index;
  tupleextract_ty(const LTy &Left, unsigned i) : L(Left), index(i) { }

  template <typename ITy>
  bool match(ITy *V) {
    auto *TEI = dyn_cast<TupleExtractInst>(V);
    if (!TEI)
      return false;

    return TEI->getFieldNo() == index && L.match((ValueBase *)TEI->getOperand());
  }
};

template <typename LTy>
tupleextract_ty<LTy> m_TupleExtractInst(const LTy &Left, unsigned Index) {
  return tupleextract_ty<LTy>(Left, Index);
}

//===----------------------------------------------------------------------===//
//              Function/Builtin/Intrinsic Application Matchers
//===----------------------------------------------------------------------===//

//===
// Callee matcher.
//

template <typename CalleeTy>
struct Callee_match;

template<>
struct Callee_match<SILFunction &> {
  const SILFunction &Fun;

  Callee_match(const SILFunction &F) : Fun(F) {}

  template <typename ITy>
  bool match(ITy *V) {
    auto *AI = dyn_cast<ApplyInst>(V);
    if (!AI)
      return false;

    return AI->getReferencedFunction() == &Fun;
  }
};

template<>
struct Callee_match<BuiltinValueKind> {
  BuiltinValueKind Kind;

  Callee_match(BuiltinValueKind K) : Kind(K) { }

  template <typename ITy>
  bool match(ITy *V) {
    auto *BI = dyn_cast<BuiltinInst>(V);
    if (!BI)
      return false;

    return BI->getBuiltinInfo().ID == Kind;
  }
};

template<>
struct Callee_match<llvm::Intrinsic::ID> {
  llvm::Intrinsic::ID IntrinsicID;

  Callee_match(const llvm::Intrinsic::ID ID) : IntrinsicID(ID) { }

  template <typename ITy>
  bool match(ITy *V) {
    auto *BI = dyn_cast<BuiltinInst>(V);
    if (!BI)
      return false;

    return BI->getIntrinsicInfo().ID == IntrinsicID;
  }
};

/// Match a callee argument.
///
/// We use explicit specialization of Callee_match to handle SILFunctions,
/// Builtins, and Intrinsics all with this one function.

template<typename CalleeTy>
inline Callee_match<CalleeTy> m_Callee(CalleeTy Callee) {
  return Callee;
}

//===
// Argument matcher
//

template <typename SubPatternTy>
struct Argument_match {
  unsigned OpI;
  SubPatternTy Val;
  Argument_match(unsigned OpIdx, const SubPatternTy &V) : OpI(OpIdx), Val(V) { }

  template <typename ITy>
  bool match(ITy *V) {
    if (auto *Apply = dyn_cast<ApplyInst>(V)) {
      return Val.match((ValueBase *)Apply->getArgument(OpI));
    }
    if (auto *Builtin = dyn_cast<BuiltinInst>(V)) {
      return Val.match((ValueBase *)Builtin->getArguments()[OpI]);
    }
    return false;
  }
};

// Explicit specialization for silvalue.
template <>
struct Argument_match<silvalue_bind> {
  unsigned OpI;
  silvalue_bind Val;
  Argument_match(unsigned OpIdx, const silvalue_bind &V) : OpI(OpIdx), Val(V) { }

  template <typename ITy>
  bool match(ITy *V) {
    if (auto *Apply = dyn_cast<ApplyInst>(V)) {
      return Val.match(Apply->getArgument(OpI));
    }
    if (auto *Builtin = dyn_cast<BuiltinInst>(V)) {
      return Val.match(Builtin->getArguments()[OpI]);
    }
    return false;
  }
};

/// Match the Ith argument with SubPatternTy.
template<unsigned OpI, typename SubPatternTy>
inline Argument_match<SubPatternTy> m_Argument(const SubPatternTy &Op) {
  return Argument_match<SubPatternTy>(OpI, Op);
}

//===
// ApplyInst
//
// ApplyInst matchers are a boolean and of a Callee_matcher and a list of
// argument matchers.

template <typename CalleeTy, typename ...Arguments>
struct Apply_match;

template <typename CalleeTy>
struct Apply_match<CalleeTy> {
  typedef Callee_match<CalleeTy> Ty;
};

template <typename CalleeTy, typename T0>
struct Apply_match<CalleeTy, T0> {
  typedef match_combine_and<Callee_match<CalleeTy>, Argument_match<T0>> Ty;
};

template <typename CalleeTy, typename T0, typename ...Arguments>
struct Apply_match<CalleeTy, T0, Arguments ...> {
  typedef match_combine_and<typename Apply_match<CalleeTy, Arguments ...>::Ty,
                            Argument_match<T0> > Ty;
};

/// Match only an ApplyInst's Callee.
template <typename CalleeTy>
inline typename Apply_match<CalleeTy>::Ty
m_ApplyInst(CalleeTy Callee) {
  return Callee;
}

/// Match an ApplyInst's Callee and first argument.
template <unsigned Index=0, typename CalleeTy, typename T0>
inline typename Apply_match<CalleeTy, T0>::Ty
m_ApplyInst(CalleeTy Callee, const T0 &Op0) {
  return m_CombineAnd(m_Callee(Callee), m_Argument<Index>(Op0));
}

/// Match an ApplyInst's Callee and up to the ApplyInsts Nth argument, where N
/// is sizeof...(Arguments) + 1.
template <unsigned Index=0, typename CalleeTy, typename T0,
          typename ...Arguments>
inline typename Apply_match<CalleeTy, T0, Arguments ...>::Ty
m_ApplyInst(CalleeTy Callee, const T0 &Op0, const Arguments &...Args) {
  return m_CombineAnd(m_ApplyInst<Index+1>(Callee, Args...),
                      m_Argument<Index>(Op0));
}

/// Match only a BuiltinInst's callee.
inline typename Apply_match<BuiltinValueKind>::Ty
m_BuiltinInst(BuiltinValueKind Callee) {
  return Callee;
}
  
/// Match a BuiltinInst's Callee and first argument.
template <unsigned Index=0, typename T0>
inline typename Apply_match<BuiltinValueKind, T0>::Ty
m_BuiltinInst(BuiltinValueKind Callee, const T0 &Op0) {
  return m_CombineAnd(m_Callee(Callee), m_Argument<Index>(Op0));
}

/// Match an ApplyInst's Callee and up to the ApplyInsts Nth argument, where N
/// is sizeof...(Arguments) + 1.
template <unsigned Index=0, typename T0,
          typename ...Arguments>
inline typename Apply_match<BuiltinValueKind, T0, Arguments ...>::Ty
m_BuiltinInst(BuiltinValueKind Callee, const T0 &Op0, const Arguments &...Args) {
  return m_CombineAnd(m_BuiltinInst<Index+1>(Callee, Args...),
                      m_Argument<Index>(Op0));
}
  
//===----------------------------------------------------------------------===//
//                             Builtin Instructions
//===----------------------------------------------------------------------===//
/// Return type of builtin instruction matchers.
template <typename ...Tys>
using BuiltinApplyTy = typename Apply_match<BuiltinValueKind, Tys...>::Ty;


/// XMacro for generating a matcher for unary builtin instructions that can
/// apply further matchers to the operands of the builtin operation.
#define BUILTIN_UNARY_OP_MATCH_WITH_ARG_MATCHER(PatternName, Kind) \
  template <typename Ty>                                           \
  BuiltinApplyTy<Ty>                                               \
  m_##PatternName(const Ty &T) {                                   \
    return m_ApplyInst(BuiltinValueKind::Kind, T);                 \
  }

/// XMacro for generating a matcher for binary builtin instructions that can
/// apply further matchers to the operands of the builtin operation.
#define BUILTIN_BINARY_OP_MATCH_WITH_ARG_MATCHER(PatternName, Kind) \
  template <typename Ty1, typename Ty2>                             \
  BuiltinApplyTy<Ty1, Ty2>                                          \
  m_##PatternName(const Ty1 &T1, const Ty2 &T2) {                   \
    return m_ApplyInst(BuiltinValueKind::Kind, T1, T2);             \
  }

/// XMacro for generating a matcher for varargs builtin instructions that can
/// apply further matchers to the operands of the builtin operation.
#define BUILTIN_VARARGS_OP_MATCH_WITH_ARG_MATCHER(PatternName, Kind) \
  template <typename ...Tys>                                         \
  BuiltinApplyTy<Tys...>                                             \
  m_##PatternName(const Tys& ...Args) {                              \
    return m_ApplyInst(BuiltinValueKind::Kind, Args...);             \
  }

#define BUILTIN_CAST_OPERATION(Id, Name, Attrs) BUILTIN(Id, Name, Attrs) \
  BUILTIN_UNARY_OP_MATCH_WITH_ARG_MATCHER(Id, Id)

#define BUILTIN_CAST_OR_BITCAST_OPERATION(Id, Name, Attrs) \
  BUILTIN_UNARY_OP_MATCH_WITH_ARG_MATCHER(Id, Id)

#define BUILTIN_BINARY_OPERATION(Id, Name, Attrs, Overload) \
  BUILTIN_BINARY_OP_MATCH_WITH_ARG_MATCHER(Id, Id)

#define BUILTIN_BINARY_PREDICATE(Id, Name, Attrs, Overload) \
  BUILTIN_BINARY_OP_MATCH_WITH_ARG_MATCHER(Id, Id)

#define BUILTIN_MISC_OPERATION(Id, Name, Attrs, Overload) \
  BUILTIN_VARARGS_OP_MATCH_WITH_ARG_MATCHER(Id, Id)

#define BUILTIN(Id, Name, Attrs)

// Define matchers for most of builtin instructions.
#include "swift/AST/Builtins.def"

//===
// Convenience compound builtin instructions matchers that succeed
// if any of the sub-matchers succeed.
//

/// Matcher for any of the builtin checked conversions.
template <typename T0>
inline typename OneOf_match<BuiltinApplyTy<T0>, BuiltinApplyTy<T0>>::Ty
m_CheckedConversion(const T0 &Op0) {
  return m_USCheckedConversion(Op0) || m_SUCheckedConversion(Op0);
}

/// Matcher for any of the builtin ExtOrBitCast instructions.
template <typename T0>
inline typename OneOf_match<BuiltinApplyTy<T0>, BuiltinApplyTy<T0>>::Ty
m_ExtOrBitCast(const T0 &Op0) {
  return m_ZExtOrBitCast(Op0) || m_SExtOrBitCast(Op0);
}

/// Matcher for any of the builtin [SZ]Ext instructions.
template <typename T0>
inline typename OneOf_match<BuiltinApplyTy<T0>, BuiltinApplyTy<T0>>::Ty
m_Ext(const T0 &Op0) {
  return m_ZExt(Op0) || m_SExt(Op0);
}

/// Matcher for any of the builtin CheckedTrunc instructions.
template <typename T0>
inline typename OneOf_match<BuiltinApplyTy<T0>, BuiltinApplyTy<T0>,
                            BuiltinApplyTy<T0>, BuiltinApplyTy<T0>>::Ty
m_CheckedTrunc(const T0 &Op0) {
  return m_UToSCheckedTrunc(Op0) || m_SToUCheckedTrunc(Op0) ||
         m_UToUCheckedTrunc(Op0) || m_SToSCheckedTrunc(Op0);
}

} // end namespace PatternMatch
} // end namespace swift

#endif
