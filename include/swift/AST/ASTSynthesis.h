//===--- ASTSynthesis.h - Convenient Swift AST synthesis --------*- C++ -*-===//
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

#ifndef SWIFT_ASTSYNTHESIS_H
#define SWIFT_ASTSYNTHESIS_H

#include "swift/AST/Types.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ParameterList.h"

namespace swift {

struct SynthesisContext {
  ASTContext &Context;
  DeclContext *DC;

  SynthesisContext(ASTContext &ctx, DeclContext *DC)
    : Context(ctx), DC(DC) {}
};

/// A synthesizer which generates a specific type.
enum SingletonTypeSynthesizer {
  _void,
  _nativeObject,
  _job,
};
inline Type synthesizeType(SynthesisContext &SC,
                           SingletonTypeSynthesizer kind) {
  switch (kind) {
  case _void: return SC.Context.TheEmptyTupleType;
  case _nativeObject: return SC.Context.TheNativeObjectType;
  case _job: return SC.Context.TheJobType;
  }
}

/// Helper types for variadic synthesis.
template <class... Ss>
struct VariadicSynthesizerStorage;

template <>
struct VariadicSynthesizerStorage<> {
  constexpr VariadicSynthesizerStorage() {}

  template <class T, class Fn>
  void collect(SynthesisContext &SC, SmallVectorImpl<T> &results,
               Fn fn) const {}
};
template <class Head, class... Tail>
struct VariadicSynthesizerStorage<Head, Tail...> {
  Head head;
  VariadicSynthesizerStorage<Tail...> tail;
  constexpr VariadicSynthesizerStorage(Head head, Tail... tail)
    : head(head), tail(tail...) {}

  template <class T, class Fn>
  void collect(SynthesisContext &SC,
               SmallVectorImpl<T> &results,
               Fn fn) const {
    results.push_back(fn(SC, head));
    tail.collect(SC, results, fn);
  }
};

/// Synthesize tuple type elements.
template <class S>
TupleTypeElt synthesizeTupleTypeElt(SynthesisContext &SC, S s) {
  return synthesizeType(SC, s);
}
struct SynthesizeTupleTypeElt {
  template <class S>
  TupleTypeElt operator()(SynthesisContext &SC, const S &s) {
    synthesizeTupleTypeElt(SC, s);
  }
};

/// Synthesize tuple types.
template <class... Elts>
struct TupleSynthesizer {
  VariadicSynthesizerStorage<Elts...> Elements;
};
template <class... Elts>
constexpr TupleSynthesizer<Elts...> _tuple(Elts... elts) {
  return {{elts...}};
}
template <class... Elts>
Type synthesizeType(SynthesisContext &SC,
                    const TupleSynthesizer<Elts...> &tuple) {
  SmallVector<TupleTypeElt, sizeof...(Elts)> elts;
  tuple.Elements.collect(SC, elts, SynthesizeTupleTypeElt());
  return TupleType::get(elts, SC.Context);
}

/// Synthesize parameter declarations.
template <class S>
ParamDecl *synthesizeParamDecl(SynthesisContext &SC, const S &s) {
  auto type = synthesizeType(SC, s);
  auto PD = new (SC.Context) ParamDecl(SourceLoc(), SourceLoc(),
                                       Identifier(), SourceLoc(),
                                       Identifier(), SC.DC);
  PD->setInterfaceType(type);
  PD->setImplicit();
  return PD;
}
template <class S>
FunctionType::Param synthesizeParamType(SynthesisContext &SC, const S &s) {
  auto type = synthesizeType(SC, s);
  return type;
}

/// Parameter specifiers.
template <class S>
struct SpecifiedParamSynthesizer { ParamSpecifier specifier; S sub; };
template <class G>
constexpr SpecifiedParamSynthesizer<G> _owned(G sub) {
  return {ParamSpecifier::Owned, sub};
}
template <class G>
constexpr SpecifiedParamSynthesizer<G> _inout(G sub) {
  return {ParamSpecifier::InOut, sub};
}
template <class S>
ParamDecl *synthesizeParamDecl(SynthesisContext &SC,
                               const SpecifiedParamSynthesizer<S> &s) {
  auto param = synthesizeParamDecl(SC, s.sub);
  param->setSpecifier(s.specifier);
  return param;
}
template <class S>
FunctionType::Param synthesizeParamType(SynthesisContext &SC,
                                        const SpecifiedParamSynthesizer<S> &s) {
  auto param = synthesizeParamType(SC, s.sub);
  auto flags = param.getParameterFlags();
  if (s.specifier == ParamSpecifier::Owned)
    flags = flags.withOwned(true);
  if (s.specifier == ParamSpecifier::InOut)
    flags = flags.withInOut(true);
  return param.withFlags(flags);
}

/// Synthesize a parameter list.
template <class... Params>
struct ParameterListSynthesizer {
  VariadicSynthesizerStorage<Params...> params;
};
template <class... Params>
constexpr ParameterListSynthesizer<Params...> _parameters(Params... ps) {
  return {{ps...}};
}

struct SynthesizeParamDecl {
  template <class S>
  ParamDecl *operator()(SynthesisContext &SC, const S &s) {
    // Found by argument-dependent lookup.
    return synthesizeParamDecl(SC, s);
  }
};
template <class... Params>
ParameterList *synthesizeParameterList(SynthesisContext &SC,
                    const ParameterListSynthesizer<Params...> &list) {
  SmallVector<ParamDecl*, 4> decls;
  list.params.collect(SC, decls, SynthesizeParamDecl());
  return ParameterList::create(SC.Context, decls);
}

struct SynthesizeParamType {
  template <class S>
  FunctionType::Param operator()(SynthesisContext &SC, const S &s) {
    // Found by argument-dependent lookup.
    return synthesizeParamType(SC, s);
  }
};
template <class... Params>
void synthesizeParameterTypes(SynthesisContext &SC,
                        const ParameterListSynthesizer<Params...> &list,
                              SmallVectorImpl<FunctionType::Param> &types) {
  list.params.collect(SC, types, SynthesizeParamType());
}

/// Synthesize function ExtInfo.
enum FunctionRepresentationSynthesizer {
  _thin,
  _thick
};
template <class S> struct ThrowsSynthesizer { S sub; };
template <class S> struct AsyncSynthesizer { S sub; };
template <class S>
constexpr ThrowsSynthesizer<S> _throws(S sub) { return {sub}; }
template <class S>
constexpr AsyncSynthesizer<S> _async(S sub) { return {sub}; }

inline ASTExtInfo synthesizeExtInfo(SynthesisContext &SC,
                                    FunctionRepresentationSynthesizer kind) {
  switch (kind) {
  case _thin: return ASTExtInfo().withRepresentation(
                                            FunctionTypeRepresentation::Thin);
  case _thick: return ASTExtInfo().withRepresentation(
                                           FunctionTypeRepresentation::Swift);
  }
}
template <class S>
ASTExtInfo synthesizeExtInfo(SynthesisContext &SC,
                             const ThrowsSynthesizer<S> &s) {
  return synthesizeExtInfo(SC, s.sub).withThrows();
}
template <class S>
ASTExtInfo synthesizeExtInfo(SynthesisContext &SC,
                             const AsyncSynthesizer<S> &s) {
  return synthesizeExtInfo(SC, s.sub).withAsync();
}

/// Synthesize a function type.
template <class ExtInfoS, class ResultS, class ParamsS>
struct FunctionTypeSynthesizer {
  ExtInfoS extInfo;
  ResultS result;
  ParamsS parameters;
};
template <class ExtInfoS, class ResultS, class ParamsS>
FunctionTypeSynthesizer<ExtInfoS, ResultS, ParamsS>
_function(ExtInfoS extInfo, ResultS result, ParamsS params) {
  return {extInfo, result, params};
}
template <class ExtInfoS, class ResultS, class ParamsS>
Type synthesizeType(SynthesisContext &SC,
             const FunctionTypeSynthesizer<ExtInfoS, ResultS, ParamsS> &fn) {
  SmallVector<FunctionType::Param, 4> paramTypes;
  synthesizeParameterTypes(SC, fn.parameters, paramTypes);
  auto extInfo = synthesizeExtInfo(SC, fn.extInfo);
  auto resultType = synthesizeType(SC, fn.result);
  return FunctionType::get(paramTypes, resultType, extInfo);
}

/// Synthesize optionals.
template <class S>
struct OptionalSynthesizer {
  S sub;
};
template <class S>
constexpr OptionalSynthesizer<S> _optional(S sub) { return {sub}; }
template <class S>
Type synthesizeType(SynthesisContext &SC, const OptionalSynthesizer<S> &s) {
  return OptionalType::get(synthesizeType(SC, s.sub));
}

} // end namespace swift

#endif