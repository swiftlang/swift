//===--- ParseSIL.cpp - SIL File Parsing logic ----------------------------===//
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

#include "SILTypeParser.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/SourceFile.h"
#include "swift/Subsystems.h"

using namespace swift;

namespace {

// Resolve the generic environments for parsed generic function and box types.
class HandleSILGenericParamsWalker : public ASTWalker {
  SourceFile *SF;

public:
  HandleSILGenericParamsWalker(SourceFile *SF) : SF(SF) {}

  bool walkToTypeReprPre(TypeRepr *T) override {
    if (auto fnType = dyn_cast<FunctionTypeRepr>(T)) {
      if (auto generics = fnType->getGenericParams()) {
        auto env = handleSILGenericParams(generics, SF);
        fnType->setGenericEnvironment(env);
      }
      if (auto generics = fnType->getPatternGenericParams()) {
        auto env = handleSILGenericParams(generics, SF);
        fnType->setPatternGenericEnvironment(env);
      }
    }
    if (auto boxType = dyn_cast<SILBoxTypeRepr>(T)) {
      if (auto generics = boxType->getGenericParams()) {
        auto env = handleSILGenericParams(generics, SF);
        boxType->setGenericEnvironment(env);
      }
    }
    return true;
  }
};

}

///   sil-type:
///     '$' '*'? attribute-list (generic-params)? type
///
bool SILTypeParser::parseSILType(SILType &Result,
                                 GenericEnvironment *&ParsedGenericEnv,
                                 bool IsFuncDecl,
                                 GenericEnvironment *OuterGenericEnv) {
  ParsedGenericEnv = nullptr;

  if (P.parseToken(tok::sil_dollar, diag::expected_sil_type))
    return true;

  // If we have a '*', then this is an address type.
  SILValueCategory category = SILValueCategory::Object;
  if (P.Tok.isAnyOperator() && P.Tok.getText().startswith("*")) {
    category = SILValueCategory::Address;
    P.consumeStartingCharacterOfCurrentToken();
  }

  // Parse attributes.
  ParamDecl::Specifier specifier;
  SourceLoc specifierLoc;
  TypeAttributes attrs;
  P.parseTypeAttributeList(specifier, specifierLoc, attrs);

  // Global functions are implicitly @convention(thin) if not specified
  // otherwise.
  if (IsFuncDecl && !attrs.has(TAK_convention)) {
    // Use a random location.
    attrs.setAttr(TAK_convention, P.PreviousLoc);
    attrs.ConventionArguments =
        TypeAttributes::Convention::makeSwiftConvention("thin");
  }

  ParserResult<TypeRepr> TyR = P.parseType(diag::expected_sil_type,
                                           /*handleCodeCompletion*/ true,
                                           /*isSILFuncDecl*/ IsFuncDecl);

  if (TyR.isNull())
    return true;

  TyR.get()->walk(HandleSILGenericParamsWalker(&P.SF));

  // Save the top-level function generic environment if there was one.
  if (auto fnType = dyn_cast<FunctionTypeRepr>(TyR.get()))
    if (auto env = fnType->getGenericEnvironment())
      ParsedGenericEnv = env;

  // Apply attributes to the type.
  TypeLoc Ty =
      P.applyAttributeToType(TyR.get(), attrs, specifier, specifierLoc);

  if (checkType(Ty, OuterGenericEnv))
    return true;

  Result =
      SILType::getPrimitiveType(Ty.getType()->getCanonicalType(), category);

  // Invoke the callback on the parsed type.
  callback(Ty.getType());

  return false;
}

bool SILTypeParser::parseSILType(SILType &Result) {
  GenericEnvironment *IgnoredEnv;
  return parseSILType(Result, IgnoredEnv);
}

bool SILTypeParser::parseSILType(SILType &Result, SourceLoc &TypeLoc) {
  TypeLoc = P.Tok.getLoc();
  return parseSILType(Result);
}

bool SILTypeParser::parseSILType(SILType &Result, SourceLoc &TypeLoc,
                                 GenericEnvironment *&parsedGenericEnv,
                                 GenericEnvironment *parentGenericEnv) {
  TypeLoc = P.Tok.getLoc();
  return parseSILType(Result, parsedGenericEnv, false, parentGenericEnv);
}
