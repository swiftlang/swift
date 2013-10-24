//===--- PatternMatch.h - SIL Pattern Matching Infrastructure ---*- C++ -*-===//
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

#include "swift/SIL/SILInstruction.h"

namespace swift {
namespace PatternMatch {

//===----------------------------------------------------------------------===//
//                       Basic Matching Infrastructure
//===----------------------------------------------------------------------===//

/// Applies the given pattern to V.
template<typename Val, typename Pattern>
bool match(Val *V, const Pattern &P) {
  return const_cast<Pattern &>(P).match(V);
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

//===----------------------------------------------------------------------===//
//              Function/Builtin/Intrinsic Application Matchers
//===----------------------------------------------------------------------===//

//===
// Callee matcher.
//

template <typename CalleeTy>
struct Callee_match;

template<>
struct Callee_match<SILFunction *> {
  const SILFunction *Fun;

  Callee_match(const SILFunction *F) : Fun(F) {}

  template <typename ITy>
  bool match(ITy *V) {
    auto *FunctionRef = dyn_cast<FunctionRefInst>(V);
    if (!FunctionRef)
      return false;

    return FunctionRef->getReferencedFunction() == Fun;
  }
};

template<>
struct Callee_match<BuiltinValueKind> {
  BuiltinValueKind Kind;

  Callee_match(const BuiltinValueKind K) : Kind(K) { }

  template <typename ITy>
  bool match(ITy *V) {
    auto *BuiltinRef = dyn_cast<BuiltinFunctionRefInst>(V);
    if (!BuiltinRef)
      return false;

    return BuiltinRef->getBuiltinInfo().ID == Kind;
  }
};

template<>
struct Callee_match<llvm::Intrinsic::ID> {
  llvm::Intrinsic::ID IntrinsicID;

  Callee_match(const llvm::Intrinsic::ID  ID) : IntrinsicID(ID) { }

  template <typename ITy>
  bool match(ITy *V) {
    auto *BuiltinRef = dyn_cast<BuiltinFunctionRefInst>(V);
    if (!BuiltinRef)
      return false;

    return BuiltinRef->getIntrinsicInfo().ID == IntrinsicID;
  }
};

/// Match a callee argument.
///
/// We use explicit specialization of Callee_match to handle SILFunctions,
/// Builtins, and Intrinsics all with this one function.
template <typename CalleeTy>
inline Callee_match<CalleeTy> m_Callee(CalleeTy *Callee) {
  return Callee_match<CalleeTy>(Callee);
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
    auto *Apply = dyn_cast<ApplyInst>(V);
    if (!Apply)
      return false;

    return Val.match(&*Apply->getOperand(OpI));
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
struct m_Apply_Ty;

template <typename CalleeTy>
struct m_Apply_Ty<CalleeTy> {
  typedef Callee_match<CalleeTy> Ty;
};

template <typename CalleeTy, typename T0>
struct m_Apply_Ty<CalleeTy, T0> {
  typedef match_combine_and<Callee_match<CalleeTy>, Argument_match<T0>> Ty;
};

template <typename CalleeTy, typename T0, typename ...Arguments>
struct m_Apply_Ty<CalleeTy, T0, Arguments ...> {
  typedef match_combine_and<typename m_Apply_Ty<CalleeTy, Arguments ...>::Ty,
                            Argument_match<T0> > Ty;
};

/// Match only an ApplyInst's Callee.
template <typename CalleeTy>
inline typename m_Apply_Ty<Callee_match<CalleeTy>>::Ty
m_ApplyInst(CalleeTy *Callee) {
  return m_Callee(Callee);
}

/// Match an ApplyInst's Callee and first argument.
template <typename CalleeTy, typename T0, unsigned Index=0>
inline typename m_Apply_Ty<Callee_match<CalleeTy>, T0>::Ty
m_ApplyInst(CalleeTy *Callee, const T0 &Op0) {
  return m_CombineAnd(m_Callee(Callee), m_Argument<Index>(Op0));
}

/// Match an ApplyInst's Callee and up to the ApplyInsts Nth argument, where N
/// is sizeof...(Arguments) + 1.
template <typename CalleeTy, typename T0, unsigned Index=0,
          typename ...Arguments>
inline typename m_Apply_Ty<Callee_match<CalleeTy>, T0, Arguments ...>::Ty
m_ApplyInst(CalleeTy *Callee, const T0 &Op0, const Arguments &...Args) {
  return m_CombineAnd(m_ApplyInst<Index+1>(Callee, Args...),
                      m_Argument<Index>(Op0));
}

} // end namespace PatternMatch
} // end namespace swift

#endif
