//===--- SILParser.h ------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_PARSER_SILPARSER_H
#define SWIFT_SIL_PARSER_SILPARSER_H

#include "SILParserState.h"

#include "swift/AST/DiagnosticsParse.h"
#include "swift/Parse/Parser.h"
#include "swift/SIL/SILCoverageMap.h"
#include "swift/Sema/SILTypeResolutionContext.h"

namespace swift {

struct ParsedSubstitution {
  SourceLoc loc;
  Type replacement;
};

struct ParsedSpecAttr {
  ArrayRef<RequirementRepr> requirements;
  bool exported;
  SILSpecializeAttr::SpecializationKind kind;
  SILFunction *target = nullptr;
  Identifier spiGroupID;
  ModuleDecl *spiModule;
  AvailabilityRange availability = AvailabilityRange::alwaysAvailable();
};

/// The parser for an individual SIL function.
class SILParser {
  friend SILParserState;

public:
  Parser &P;
  SILModule &SILMod;
  SILParserState &TUState;
  SILFunction *F = nullptr;
  GenericSignature ContextGenericSig;
  GenericParamList *ContextGenericParams = nullptr;

private:
  /// HadError - Have we seen an error parsing this function?
  bool HadError = false;

  /// Transient state for parsing a multiple optional attributes
  /// in parseSpecificSILInstruction:
  ///   <regular syntax>[, keyword1][, keyword2]
  bool parsedComma = false;

  /// Data structures used to perform name lookup of basic blocks.
  llvm::DenseMap<Identifier, SILBasicBlock *> BlocksByName;
  llvm::DenseMap<SILBasicBlock *, Located<Identifier>> UndefinedBlocks;

  /// The set of opened packs in the function, indexed by UUID.
  /// Note that we don't currently support parsing references to
  /// opened packs prior to their instruction, although this is
  /// theoretically possible if basic blocks are not sorted in
  /// dominance order.
  SILTypeResolutionContext::OpenedPackElementsMap OpenedPackElements;

  /// Data structures used to perform name lookup for local values.
  llvm::StringMap<ValueBase *> LocalValues;
  llvm::StringMap<llvm::SmallVector<SpecifyTestInst *>> TestSpecsWithRefs;
  llvm::StringMap<SourceLoc> ForwardRefLocalValues;

  Type performTypeResolution(TypeRepr *TyR, bool IsSILType,
                             GenericSignature GenericSig,
                             GenericParamList *GenericParams);

  void convertRequirements(ArrayRef<RequirementRepr> From,
                           SmallVectorImpl<Requirement> &To,
                           SmallVectorImpl<Type> &typeErasedParams);

  ProtocolConformanceRef
  parseProtocolConformanceHelper(ProtocolDecl *&proto,
                                 GenericSignature GenericSig,
                                 GenericParamList *WitnessParams);

public:
  SILParser(Parser &P)
      : P(P), SILMod(static_cast<SILParserState *>(P.SIL)->M),
        TUState(*static_cast<SILParserState *>(P.SIL)) {}

  ~SILParser();

  /// diagnoseProblems - After a function is fully parse, emit any diagnostics
  /// for errors and return true if there were any.
  bool diagnoseProblems();

  /// getGlobalNameForReference - Given a reference to a global name, look it
  /// up and return an appropriate SIL function.
  SILFunction *getGlobalNameForReference(Identifier Name, CanSILFunctionType Ty,
                                         SourceLoc Loc,
                                         bool IgnoreFwdRef = false);
  /// getGlobalNameForDefinition - Given a definition of a global name, look
  /// it up and return an appropriate SIL function.
  SILFunction *getGlobalNameForDefinition(Identifier Name,
                                          CanSILFunctionType Ty, SourceLoc Loc);

  /// getBBForDefinition - Return the SILBasicBlock for a definition of the
  /// specified block.
  SILBasicBlock *getBBForDefinition(Identifier Name, SourceLoc Loc);

  /// getBBForReference - return the SILBasicBlock of the specified name.  The
  /// source location is used to diagnose a failure if the block ends up never
  /// being defined.
  SILBasicBlock *getBBForReference(Identifier Name, SourceLoc Loc);

  struct UnresolvedValueName {
    StringRef Name;
    SourceLoc NameLoc;

    bool isUndef() const { return Name == "undef"; }
  };

  /// getLocalValue - Get a reference to a local value with the specified name
  /// and type.
  SILValue getLocalValue(UnresolvedValueName Name, SILType Type, SILLocation L,
                         SILBuilder &B);

  /// setLocalValue - When an instruction or block argument is defined, this
  /// method is used to register it and update our symbol table.
  void setLocalValue(ValueBase *Value, StringRef Name, SourceLoc NameLoc);

  SILDebugLocation getDebugLoc(SILBuilder &B, SILLocation Loc) {
    return SILDebugLocation(Loc, F->getDebugScope());
  }

  /// @{ Primitive parsing.

  /// \verbatim
  ///   sil-identifier ::= [A-Za-z_0-9]+
  /// \endverbatim
  bool parseSILIdentifier(Identifier &Result, SourceLoc &Loc, DiagRef D);

  template <typename... DiagArgTypes, typename... ArgTypes>
  bool parseSILIdentifier(Identifier &Result, Diag<DiagArgTypes...> ID,
                          ArgTypes... Args) {
    SourceLoc L;
    return parseSILIdentifier(Result, L, {ID, {Args...}});
  }

  template <typename T, typename... DiagArgTypes, typename... ArgTypes>
  bool parseSILIdentifierSwitch(T &Result, ArrayRef<StringRef> Strings,
                                Diag<DiagArgTypes...> ID, ArgTypes... Args) {
    Identifier TmpResult;
    SourceLoc L;
    if (parseSILIdentifier(TmpResult, L, {ID, {Args...}})) {
      return true;
    }

    auto Iter = std::find(Strings.begin(), Strings.end(), TmpResult.str());
    if (Iter == Strings.end()) {
      P.diagnose(P.Tok, ID, Args...);
      return true;
    }

    Result = T(*Iter);
    return false;
  }

  template <typename... DiagArgTypes, typename... ArgTypes>
  bool parseSILIdentifier(Identifier &Result, SourceLoc &L,
                          Diag<DiagArgTypes...> ID, ArgTypes... Args) {
    return parseSILIdentifier(Result, L, {ID, {Args...}});
  }

  template <typename T>
  bool
  parseSILQualifier(std::optional<T> &result,
                    llvm::function_ref<std::optional<T>(StringRef)> parseName);

  bool parseVerbatim(StringRef identifier);

  template <typename T>
  bool parseInteger(T &Result, DiagRef D) {
    if (!P.Tok.is(tok::integer_literal)) {
      P.diagnose(P.Tok, D);
      return true;
    }
    bool error = parseIntegerLiteral(P.Tok.getText(), 0, Result);
    P.consumeToken(tok::integer_literal);
    return error;
  }

  template <typename T>
  bool parseIntegerLiteral(StringRef text, unsigned radix, T &result) {
    text = prepareIntegerLiteralForParsing(text);
    return text.getAsInteger(radix, result);
  }

  StringRef prepareIntegerLiteralForParsing(StringRef text) {
    // tok::integer_literal can contain characters that the library
    // parsing routines don't expect.
    if (text.contains('_'))
      text = P.copyAndStripUnderscores(text);
    return text;
  }

  /// @}

  /// @{ Type parsing.
  bool parseASTType(CanType &result,
                    GenericSignature genericSig = GenericSignature(),
                    GenericParamList *genericParams = nullptr,
                    bool forceContextualType = false);
  bool parseASTType(CanType &result, SourceLoc &TypeLoc,
                    GenericSignature genericSig = GenericSignature(),
                    GenericParamList *genericParams = nullptr,
                    bool forceContextualType = false) {
    TypeLoc = P.Tok.getLoc();
    return parseASTType(result, genericSig, genericParams, forceContextualType);
  }

  bool parseASTPackType(CanPackType &result) {
    SourceLoc loc;
    CanType rawType;
    if (parseASTType(rawType, loc))
      return true;
    result = dyn_cast<PackType>(rawType);
    if (!result) {
      P.diagnose(loc, diag::expected_sil_type_kind, "match $Pack{...}");
      return true;
    }
    return false;
  }

  bool parseASTTypeOrValue(CanType &result,
                           GenericSignature genericSig = GenericSignature(),
                           GenericParamList *genericParams = nullptr,
                           bool forceContextualType = false);

  std::optional<StringRef>
  parseOptionalAttribute(ArrayRef<StringRef> expected) {
    // We parse here @ <identifier>.
    if (P.Tok.getKind() != tok::at_sign)
      return std::nullopt;

    auto name = P.peekToken().getText();
    if (!is_contained(expected, name))
      return std::nullopt;

    // Ok, we can do this.
    P.consumeToken(tok::at_sign);
    P.consumeToken(tok::identifier);
    return name;
  }

  bool parseSILOwnership(ValueOwnershipKind &OwnershipKind) {
    // We parse here @ <identifier>.
    if (!P.consumeIf(tok::at_sign)) {
      // If we fail, we must have @any ownership. We check elsewhere in the
      // parser that this matches what the function signature wants.
      OwnershipKind = OwnershipKind::None;
      return false;
    }

    StringRef AllOwnershipKinds[4] = {"unowned", "owned", "guaranteed", "none"};
    return parseSILIdentifierSwitch(OwnershipKind, AllOwnershipKinds,
                                    diag::expected_sil_value_ownership_kind);
  }
  void bindSILGenericParams(TypeRepr *TyR);
  bool parseSILType(SILType &Result, GenericSignature &parsedGenericSig,
                    GenericParamList *&parsedGenericParams,
                    bool IsFuncDecl = false,
                    GenericSignature parentGenericSig = GenericSignature(),
                    GenericParamList *parentGenericParams = nullptr);
  bool parseSILType(SILType &Result) {
    GenericSignature IgnoredSig;
    GenericParamList *IgnoredParams = nullptr;
    return parseSILType(Result, IgnoredSig, IgnoredParams);
  }
  bool parseSILType(SILType &Result, SourceLoc &TypeLoc) {
    TypeLoc = P.Tok.getLoc();
    return parseSILType(Result);
  }
  bool parseSILType(SILType &Result, SourceLoc &TypeLoc,
                    GenericSignature &parsedGenericSig,
                    GenericParamList *&parsedGenericParams,
                    GenericSignature parentGenericSig = GenericSignature(),
                    GenericParamList *parentGenericParams = nullptr) {
    TypeLoc = P.Tok.getLoc();
    return parseSILType(Result, parsedGenericSig, parsedGenericParams, false,
                        parentGenericSig, parentGenericParams);
  }
  /// @}

  bool parseSILDottedPath(ValueDecl *&Decl,
                          SmallVectorImpl<ValueDecl *> &values);
  bool parseSILDottedPath(ValueDecl *&Decl) {
    SmallVector<ValueDecl *, 4> values;
    return parseSILDottedPath(Decl, values);
  }
  bool parseSILDottedPathWithoutPound(ValueDecl *&Decl,
                                      SmallVectorImpl<ValueDecl *> &values);
  bool parseSILDottedPathWithoutPound(ValueDecl *&Decl) {
    SmallVector<ValueDecl *, 4> values;
    return parseSILDottedPathWithoutPound(Decl, values);
  }
  /// At the time of calling this function, we may not have the type of the
  /// Decl yet. So we return a SILDeclRef on the first lookup result and also
  /// return all the lookup results. After parsing the expected type, the
  /// caller of this function can choose the one that has the expected type.
  bool parseSILDeclRef(SILDeclRef &Result,
                       SmallVectorImpl<ValueDecl *> &values);
  bool parseSILDeclRef(SILDeclRef &Result) {
    SmallVector<ValueDecl *, 4> values;
    return parseSILDeclRef(Result, values);
  }
  bool parseSILDeclRef(SILDeclRef &Member, bool FnTypeRequired);
  bool parseGlobalName(Identifier &Name);
  bool parseValueName(UnresolvedValueName &Name);
  bool parseValueRef(SILValue &Result, SILType Ty, SILLocation Loc,
                     SILBuilder &B);
  bool parseTypedValueRef(SILValue &Result, SourceLoc &Loc, SILBuilder &B);
  bool parseTypedValueRef(SILValue &Result, SILBuilder &B) {
    SourceLoc Tmp;
    return parseTypedValueRef(Result, Tmp, B);
  }
  bool parseSILOpcode(SILInstructionKind &Opcode, SourceLoc &OpcodeLoc,
                      StringRef &OpcodeName);
  bool parseSILDebugVar(SILDebugVariable &Var);

  bool parseSILDebugInfoExpression(SILDebugInfoExpression &DIExpr);

  /// Parses the basic block arguments as part of branch instruction.
  bool parseSILBBArgsAtBranch(SmallVector<SILValue, 6> &Args, SILBuilder &B);

  bool parseSILLocation(SILLocation &L);
  bool parseScopeRef(SILDebugScope *&DS);
  bool parseForwardingOwnershipKind(ValueOwnershipKind &forwardingKind);
  bool parseSILDebugLocation(SILLocation &L, SILBuilder &B);
  bool parseSpecificSILInstruction(SILBuilder &B, SILInstructionKind Opcode,
                                   SourceLoc OpcodeLoc, StringRef OpcodeName,
                                   SILInstruction *&ResultVal);

  bool parseSILInstruction(SILBuilder &B);
  bool parseCallInstruction(SILLocation InstLoc, SILInstructionKind Opcode,
                            SILBuilder &B, SILInstruction *&ResultVal);
  bool parseSILFunctionRef(SILLocation InstLoc, SILFunction *&ResultFn);

  bool parseSILBasicBlock(SILBuilder &B);
  bool parseKeyPathPatternComponent(KeyPathPatternComponent &component,
                                    SmallVectorImpl<SILType> &operandTypes,
                                    SourceLoc componentLoc,
                                    Identifier componentKind,
                                    SILLocation InstLoc,
                                    GenericSignature patternSig,
                                    GenericParamList *patternParams);
  bool isStartOfSILInstruction();

  bool parseSubstitutions(SmallVectorImpl<ParsedSubstitution> &parsed,
                          GenericSignature GenericSig = GenericSignature(),
                          GenericParamList *GenericParams = nullptr);

  ProtocolConformanceRef
  parseProtocolConformance(ProtocolDecl *&proto, GenericSignature &genericSig,
                           GenericParamList *&genericParams);
  ProtocolConformanceRef parseProtocolConformance() {
    ProtocolDecl *dummy = nullptr;
    GenericSignature genericSig;
    GenericParamList *genericParams = nullptr;
    return parseProtocolConformance(dummy, genericSig, genericParams);
  }

  std::optional<llvm::coverage::Counter>
  parseSILCoverageExpr(llvm::coverage::CounterExpressionBuilder &Builder);

  template <class T>
  struct ParsedEnum {
    std::optional<T> Value;
    StringRef Name;
    SourceLoc Loc;

    bool isSet() const { return Value.has_value(); }
    T operator*() const { return *Value; }
  };

  template <class T>
  void setEnum(ParsedEnum<T> &existing, T value, StringRef name,
               SourceLoc loc) {
    if (existing.Value) {
      if (*existing.Value == value) {
        P.diagnose(loc, diag::duplicate_attribute, /*modifier*/ 1);
      } else {
        P.diagnose(loc, diag::mutually_exclusive_attr_names, name,
                   existing.Name,
                   /*modifier*/ 1);
      }
      P.diagnose(existing.Loc, diag::previous_attribute, /*modifier*/ 1);
    }
    existing.Value = value;
    existing.Name = name;
    existing.Loc = loc;
  }

  template <class T>
  void maybeSetEnum(bool allowed, ParsedEnum<T> &existing, T value,
                    StringRef name, SourceLoc loc) {
    if (allowed)
      setEnum(existing, value, name, loc);
    else
      P.diagnose(loc, diag::unknown_attr_name, name);
  }
  
  /// Parse into checked cast options, such as [prohibit_isolated_conformances].
  CheckedCastInstOptions parseCheckedCastInstOptions(bool *isExact);
};

} // namespace swift

#endif
