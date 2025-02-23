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
#include "swift/SIL/SILInstruction.h"
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

  bool match(SILValue v) { return isa<Class>(&*v); }
};

template<typename Class>
struct bind_ty {
  Class *&VR;
  bind_ty(Class *&V) : VR(V) {}

  template<typename ITy>
  bool match(ITy *V) {
    if (auto *CV = dyn_cast<Class>(V)) {
      VR = CV;
      return true;
    }
    return false;
  }

  bool match(SILValue v) { return match(&*v); }
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

  bool match(SILValue v) { return match(&*v); }
};

template<typename LTy, typename RTy>
struct match_combine_and {
  LTy L;
  RTy R;

  match_combine_and(const LTy &Left, const RTy &Right) : L(Left), R(Right) { }

  bool match(SILValue v) { return match(&*v); }

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
  using Ty = T0;
};

template <typename T0, typename T1>
struct OneOf_match<T0, T1> {
  using Ty = match_combine_or<T0, T1>;
};

template <typename T0, typename T1, typename ...Arguments>
struct OneOf_match<T0, T1, Arguments ...> {
  using Ty = typename OneOf_match<match_combine_or<T0, T1>, Arguments...>::Ty;
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
struct specificintegerliteral_ty {
  APInt Value;
  bool isSigned;
  specificintegerliteral_ty(APInt V, bool S) : Value(V), isSigned(S) {}

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

static inline specificintegerliteral_ty
m_SpecificIntegerLiteral(APInt V, bool isSigned) {
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
//                        Instruction Operand Matchers
//===----------------------------------------------------------------------===//

namespace detail {

struct GetOperandsFunctor {
  template <size_t... Idx>
  std::array<SILValue, sizeof...(Idx)>
  operator()(SILInstruction *i, std::index_sequence<Idx...> seq) const {
    return {i->getOperand(Idx)...};
  }
};

template <typename... MatcherTys> struct MatcherFunctor {
  std::tuple<MatcherTys...> matchers;

  MatcherFunctor(const MatcherTys &... matchers) : matchers(matchers...) {}

  template <typename MatcherTy> bool individual(MatcherTy matcher, SILValue v) {
    return matcher.match(v);
  }

  template <size_t... Idx>
  std::array<bool, sizeof...(MatcherTys)>
  matchHelper(const std::array<SILValue, sizeof...(MatcherTys)> &operands,
              std::index_sequence<Idx...> seq) {
    return {individual(std::get<Idx>(matchers), std::get<Idx>(operands))...};
  }

  bool match(SILInstruction *i) {
    std::array<SILValue, sizeof...(MatcherTys)> operands =
        GetOperandsFunctor{}(i, std::index_sequence_for<MatcherTys...>{});
    auto tmpResult =
        matchHelper(operands, std::index_sequence_for<MatcherTys...>{});
    for (unsigned i : indices(tmpResult)) {
      if (!tmpResult[i])
        return false;
    }
    return true;
  }
};

} // namespace detail

// NOTE: These matchers only can accept compound, non-fundamental
// types. This prevents by mistake passing in int, float, etc to these
// matchers.
template <SILInstructionKind Kind, typename... MatcherTys>
struct InstructionOperand_match {
  // This just makes sure that we catch common mistakes passing
  // fundamental types (i.e. int, float, etc) to this API.
  static_assert(
      are_all_compound<MatcherTys...>::value,
      "Expected all matcher tys to be non-fundamental compound types?!");
  detail::MatcherFunctor<MatcherTys...> matcherFunctor;

  static constexpr unsigned NumMatchers = sizeof...(MatcherTys);

  // Only allow for these to be constructed from non-fundamental types.
  InstructionOperand_match(const MatcherTys &... matchers)
      : matcherFunctor(matchers...) {}

  bool match(SILNode *node) {
    if (node->getKind() != SILNodeKind(Kind))
      return false;
    return match(cast<SILInstruction>(node));
  }

  bool match(SILInstruction *i) {
    if (i->getKind() != Kind || !(i->getNumOperands() <= NumMatchers))
      return false;

    return matcherFunctor.match(i);
  }
};

#define FULL_INST(ID, NAME, PARENT, MEMBEHAVIOR, MAYRELEASE)                   \
  template <typename... MatcherTys>                                            \
  InstructionOperand_match<SILInstructionKind::ID, MatcherTys...> m_##ID(      \
      const MatcherTys &... matchers) {                                        \
    return {matchers...};                                                      \
  }
#include "swift/SIL/SILNodes.def"

//===----------------------------------------------------------------------===//
//                         Address/Struct Projections
//===----------------------------------------------------------------------===//

/// Match either a tuple_extract that the index field from a tuple or the
/// indexth destructure_tuple result.
template <typename LTy> struct tupleextractoperation_ty {
  LTy L;
  unsigned index;
  tupleextractoperation_ty(const LTy &Left, unsigned i) : L(Left), index(i) {}

  bool match(SILValue v) {
    auto *inst = v->getDefiningInstruction();
    if (!inst)
      return false;
    return match(inst);
  }

  template <typename ITy> bool match(ITy *V) {
    if (auto *TEI = dyn_cast<TupleExtractInst>(V)) {
      return TEI->getFieldIndex() == index &&
             L.match((ValueBase *)TEI->getOperand());
    }

    if (auto *DTR = dyn_cast<MultipleValueInstructionResult>(V)) {
      if (auto *DT = dyn_cast<DestructureTupleInst>(DTR->getParent())) {
        return DTR->getIndex() == index &&
          L.match((ValueBase *)DT->getOperand());
      }
    }

    return false;
  }
};

template <typename LTy>
tupleextractoperation_ty<LTy> m_TupleExtractOperation(const LTy &Left,
                                                      unsigned Index) {
  return tupleextractoperation_ty<LTy>(Left, Index);
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

    return AI->getReferencedFunctionOrNull() == &Fun;
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
  using Ty = Callee_match<CalleeTy>;
};

template <typename CalleeTy, typename T0>
struct Apply_match<CalleeTy, T0> {
  using Ty = match_combine_and<Callee_match<CalleeTy>, Argument_match<T0>>;
};

template <typename CalleeTy, typename T0, typename ...Arguments>
struct Apply_match<CalleeTy, T0, Arguments ...> {
  using Ty = match_combine_and<typename Apply_match<CalleeTy, Arguments...>::Ty,
                               Argument_match<T0>>;
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

#define BUILTIN_BINARY_OPERATION_ALL(Id, Name, Attrs, Overload)                \
  BUILTIN_BINARY_OP_MATCH_WITH_ARG_MATCHER(Id, Id)

#define BUILTIN_BINARY_PREDICATE(Id, Name, Attrs, Overload) \
  BUILTIN_BINARY_OP_MATCH_WITH_ARG_MATCHER(Id, Id)

#define BUILTIN_MISC_OPERATION(Id, Name, Attrs, Overload) \
  BUILTIN_VARARGS_OP_MATCH_WITH_ARG_MATCHER(Id, Id)

#define BUILTIN(Id, Name, Attrs)

// Define matchers for most of builtin instructions.
#include "swift/AST/Builtins.def"

#undef BUILTIN_UNARY_OP_MATCH_WITH_ARG_MATCHER
#undef BUILTIN_BINARY_OP_MATCH_WITH_ARG_MATCHER
#undef BUILTIN_VARARGS_OP_MATCH_WITH_ARG_MATCHER
#undef BUILTIN_CAST_OPERATION
#undef BUILTIN_CAST_OR_BITCAST_OPERATION
#undef BUILTIN_BINARY_OPERATION_ALL
#undef BUILTIN_BINARY_PREDICATE
#undef BUILTIN_MISC_OPERATION
#undef BUILTIN

//===
// Convenience compound builtin instructions matchers that succeed
// if any of the sub-matchers succeed.
//

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
