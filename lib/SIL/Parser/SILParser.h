//===--- SILParser.h - SILParser declaration ------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILPARSER_H
#define SWIFT_SIL_SILPARSER_H

#include "SILParserFunctionBuilder.h"
#include "SILParserTranslationUnitState.h"
#include "swift/AST/SourceFile.h"
#include "swift/Basic/LLVM.h"
#include "swift/Parse/Parser.h"
#include "swift/SIL/SILModule.h"

//===----------------------------------------------------------------------===//
// SILParser
//===----------------------------------------------------------------------===//

namespace swift {
namespace silparser {

struct ParsedSubstitution {
  SourceLoc loc;
  Type replacement;
};

struct ParsedSpecAttr {
  ArrayRef<RequirementRepr> requirements;
  bool exported;
  SILSpecializeAttr::SpecializationKind kind;
};

enum class ConformanceContext {
  /// A normal conformance parse.
  Ordinary,

  /// We're parsing this for a SIL witness table.
  /// Leave any generic parameter clauses in scope, and use an explicit
  /// self-conformance instead of an abstract one.
  WitnessTable,
};

class SILParser {
  friend SILParserTUState;

public:
  Parser &P;
  SILModule &SILMod;
  SILParserTUState &TUState;
  SILFunction *F = nullptr;
  GenericEnvironment *ContextGenericEnv = nullptr;

private:
  /// HadError - Have we seen an error parsing this function?
  bool HadError = false;

  /// Data structures used to perform name lookup of basic blocks.
  llvm::DenseMap<Identifier, SILBasicBlock *> BlocksByName;
  llvm::DenseMap<SILBasicBlock *, Located<Identifier>> UndefinedBlocks;

  /// Data structures used to perform name lookup for local values.
  llvm::StringMap<ValueBase *> LocalValues;
  llvm::StringMap<SourceLoc> ForwardRefLocalValues;

  /// A callback to be invoked every time a type was deserialized.
  std::function<void(Type)> ParsedTypeCallback;

  bool performTypeLocChecking(TypeLoc &T, bool IsSILType,
                              GenericEnvironment *GenericEnv = nullptr,
                              DeclContext *DC = nullptr);

  void convertRequirements(SILFunction *F, ArrayRef<RequirementRepr> From,
                           SmallVectorImpl<Requirement> &To);

  ProtocolConformanceRef parseProtocolConformanceHelper(
      ProtocolDecl *&proto, GenericEnvironment *GenericEnv,
      ConformanceContext context, ProtocolDecl *defaultForProto);

public:
  SILParser(Parser &P)
      : P(P), SILMod(static_cast<SILParserTUState *>(P.SIL)->M),
        TUState(*static_cast<SILParserTUState *>(P.SIL)),
        ParsedTypeCallback([](Type ty) {}) {}

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
  bool parseSILIdentifier(Identifier &Result, SourceLoc &Loc,
                          const Diagnostic &D);

  template <typename... DiagArgTypes, typename... ArgTypes>
  bool parseSILIdentifier(Identifier &Result, Diag<DiagArgTypes...> ID,
                          ArgTypes... Args) {
    SourceLoc L;
    return parseSILIdentifier(Result, L, Diagnostic(ID, Args...));
  }

  template <typename T, typename... DiagArgTypes, typename... ArgTypes>
  bool parseSILIdentifierSwitch(T &Result, ArrayRef<StringRef> Strings,
                                Diag<DiagArgTypes...> ID, ArgTypes... Args) {
    Identifier TmpResult;
    SourceLoc L;
    if (parseSILIdentifier(TmpResult, L, Diagnostic(ID, Args...))) {
      return true;
    }

    auto Iter = std::find(Strings.begin(), Strings.end(), TmpResult.str());
    if (Iter == Strings.end()) {
      P.diagnose(P.Tok, Diagnostic(ID, Args...));
      return true;
    }

    Result = T(*Iter);
    return false;
  }

  template <typename... DiagArgTypes, typename... ArgTypes>
  bool parseSILIdentifier(Identifier &Result, SourceLoc &L,
                          Diag<DiagArgTypes...> ID, ArgTypes... Args) {
    return parseSILIdentifier(Result, L, Diagnostic(ID, Args...));
  }

  bool parseVerbatim(StringRef identifier);

  template <typename T> bool parseInteger(T &Result, const Diagnostic &D) {
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
  bool parseASTType(CanType &result, GenericEnvironment *environment = nullptr);
  bool parseASTType(CanType &result, SourceLoc &TypeLoc) {
    TypeLoc = P.Tok.getLoc();
    return parseASTType(result);
  }
  bool parseASTType(CanType &result, SourceLoc &TypeLoc,
                    GenericEnvironment *env) {
    TypeLoc = P.Tok.getLoc();
    return parseASTType(result, env);
  }
  bool parseSILOwnership(ValueOwnershipKind &OwnershipKind) {
    // We parse here @ <identifier>.
    if (!P.consumeIf(tok::at_sign)) {
      // If we fail, we must have @any ownership. We check elsewhere in the
      // parser that this matches what the function signature wants.
      OwnershipKind = ValueOwnershipKind::None;
      return false;
    }

    StringRef AllOwnershipKinds[3] = {"unowned", "owned", "guaranteed"};
    return parseSILIdentifierSwitch(OwnershipKind, AllOwnershipKinds,
                                    diag::expected_sil_value_ownership_kind);
  }
  bool parseSILType(SILType &Result, GenericEnvironment *&parsedGenericEnv,
                    bool IsFuncDecl = false,
                    GenericEnvironment *parentGenericEnv = nullptr);
  bool parseSILType(SILType &Result) {
    GenericEnvironment *IgnoredEnv;
    return parseSILType(Result, IgnoredEnv);
  }
  bool parseSILType(SILType &Result, SourceLoc &TypeLoc) {
    TypeLoc = P.Tok.getLoc();
    return parseSILType(Result);
  }
  bool parseSILType(SILType &Result, SourceLoc &TypeLoc,
                    GenericEnvironment *&parsedGenericEnv,
                    GenericEnvironment *parentGenericEnv = nullptr) {
    TypeLoc = P.Tok.getLoc();
    return parseSILType(Result, parsedGenericEnv, false, parentGenericEnv);
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

  /// Parses the basic block arguments as part of branch instruction.
  bool parseSILBBArgsAtBranch(SmallVector<SILValue, 6> &Args, SILBuilder &B);

  bool parseSILLocation(SILLocation &L);
  bool parseScopeRef(SILDebugScope *&DS);
  bool parseSILDebugLocation(SILLocation &L, SILBuilder &B,
                             bool parsedComma = false);
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
                                    GenericEnvironment *patternEnv);
  bool isStartOfSILInstruction();

  bool parseSubstitutions(SmallVectorImpl<ParsedSubstitution> &parsed,
                          GenericEnvironment *GenericEnv = nullptr,
                          ProtocolDecl *defaultForProto = nullptr);

  ProtocolConformanceRef parseProtocolConformance(
      ProtocolDecl *&proto, GenericEnvironment *&genericEnv,
      ConformanceContext context, ProtocolDecl *defaultForProto);
  ProtocolConformanceRef parseProtocolConformance(ProtocolDecl *defaultForProto,
                                                  ConformanceContext context) {
    ProtocolDecl *dummy;
    GenericEnvironment *env;
    return parseProtocolConformance(dummy, env, context, defaultForProto);
  }

  Optional<llvm::coverage::Counter>
  parseSILCoverageExpr(llvm::coverage::CounterExpressionBuilder &Builder);

  template <class T> struct ParsedEnum {
    Optional<T> Value;
    StringRef Name;
    SourceLoc Loc;

    bool isSet() const { return Value.hasValue(); }
    T operator*() const { return *Value; }
  };

  template <class T>
  void setEnum(ParsedEnum<T> &existing, T value, StringRef name,
               SourceLoc loc) {
    if (existing.Value) {
      if (*existing.Value == value) {
        P.diagnose(loc, diag::duplicate_attribute, /*modifier*/ 1);
      } else {
        P.diagnose(loc, diag::mutually_exclusive_attrs, name, existing.Name,
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
      P.diagnose(loc, diag::unknown_attribute, name);
  }
};

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_SIL_SILPARSER_H
