//===--- ParseSIL.cpp - SIL File Parsing logic ----------------------------===//
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

#include "swift/Basic/Fallthrough.h"
#include "swift/AST/ArchetypeBuilder.h"
#include "swift/AST/NameLookup.h"
#include "swift/Parse/Parser.h"
#include "swift/Parse/Lexer.h"
#include "swift/SIL/AbstractionPattern.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILUndef.h"
#include "swift/Subsystems.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/ADT/StringSwitch.h"
using namespace swift;

//===----------------------------------------------------------------------===//
// SILParserState implementation
//===----------------------------------------------------------------------===//

namespace swift {
  class SILParserTUState {
  public:
    SILParserTUState() {}
    ~SILParserTUState();
    
    /// This is all of the forward referenced functions with
    /// the location for where the reference is.
    llvm::DenseMap<Identifier,
                   std::pair<SILFunction*, SourceLoc>> ForwardRefFns;
    
    /// Did we parse a sil_stage for this module?
    bool DidParseSILStage = false;
    
    DiagnosticEngine *Diags = nullptr;
  };
}

SILParserState::SILParserState(SILModule *M) : M(M) {
  S = M ? new SILParserTUState() : nullptr;
}

SILParserState::~SILParserState() {
  delete S;
}

SILParserTUState::~SILParserTUState() {
  if (!ForwardRefFns.empty())
    for (auto Entry : ForwardRefFns)
      Diags->diagnose(Entry.second.second, diag::sil_use_of_undefined_value,
                      Entry.first.str());
}


//===----------------------------------------------------------------------===//
// SILParser
//===----------------------------------------------------------------------===//

namespace {
  struct ParsedSubstitution {
    SourceLoc loc;
    Identifier name;
    Type replacement;
  };

  class SILParser {
  public:
    Parser &P;
    SILModule &SILMod;
    SILParserTUState &TUState;
    SILFunction *F;
  private:
    /// HadError - Have we seen an error parsing this function?
    bool HadError = false;

    /// Data structures used to perform name lookup of basic blocks.
    llvm::DenseMap<Identifier, SILBasicBlock*> BlocksByName;
    llvm::DenseMap<SILBasicBlock*,
                   std::pair<SourceLoc, Identifier>> UndefinedBlocks;

    /// Data structures used to perform name lookup for local values.
    llvm::StringMap<ValueBase*> LocalValues;
    llvm::StringMap<std::vector<SILValue>> ForwardMRVLocalValues;
    llvm::StringMap<SourceLoc> ForwardRefLocalValues;

    bool performTypeLocChecking(TypeLoc &T, bool IsSIL = true);
    bool parseApplySubstitutions(
                   SmallVectorImpl<ParsedSubstitution> &parsed);
    bool handleGenericParams(TypeLoc &T, ArchetypeBuilder *builder);
    bool parseSpecConformanceSubstitutions(
                   SmallVectorImpl<ParsedSubstitution> &parsed);
    ProtocolConformance *parseProtocolConformanceHelper(ProtocolDecl *&proto,
                                                        bool localScope);
  public:

    SILParser(Parser &P) : P(P), SILMod(*P.SIL->M), TUState(*P.SIL->S) {}

    /// diagnoseProblems - After a function is fully parse, emit any diagnostics
    /// for errors and return true if there were any.
    bool diagnoseProblems();

    /// getGlobalNameForReference - Given a reference to a global name, look it
    /// up and return an appropriate SIL function.
    SILFunction *getGlobalNameForReference(Identifier Name,
                                           CanSILFunctionType Ty,
                                           SourceLoc Loc);
    /// getGlobalNameForDefinition - Given a definition of a global name, look
    /// it up and return an appropriate SIL function.
    SILFunction *getGlobalNameForDefinition(Identifier Name,
                                            CanSILFunctionType Ty,
                                            SourceLoc Loc);

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
      unsigned ResultVal;

      bool isUndef() const { return Name == "undef"; }
      bool isMRV() const { return ResultVal != ~0U; }
    };

    /// getLocalValue - Get a reference to a local value with the specified name
    /// and type.
    SILValue getLocalValue(UnresolvedValueName Name, SILType Type,
                           SILLocation L);

    /// setLocalValue - When an instruction or block argument is defined, this
    /// method is used to register it and update our symbol table.
    void setLocalValue(ValueBase *Value, StringRef Name, SourceLoc NameLoc);
    
  public:
    /// @{ Primitive parsing.

    /// \verbatim
    ///   sil-identifier ::= [A-Za-z_0-9]+
    /// \endverbatim
    bool parseSILIdentifier(Identifier &Result, SourceLoc &Loc,
                            const Diagnostic &D);

    template<typename ...DiagArgTypes, typename ...ArgTypes>
    bool parseSILIdentifier(Identifier &Result, Diag<DiagArgTypes...> ID,
                            ArgTypes... Args) {
      SourceLoc L;
      return parseSILIdentifier(Result, L, Diagnostic(ID, Args...));
    }

    template<typename ...DiagArgTypes, typename ...ArgTypes>
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
      P.Tok.getText().getAsInteger(0, Result);
      P.consumeToken(tok::integer_literal);
      return false;
    }

    /// @}

    // Parsing logic.
    bool parseASTType(CanType &result);
    bool parseSILType(SILType &Result, GenericParamList *&genericParams,
                      bool IsFuncDecl = false);
    bool parseSILType(SILType &Result) {
      GenericParamList *Junk;
      return parseSILType(Result, Junk);
    }
    bool parseSILType(SILType &Result, SourceLoc &TypeLoc) {
      TypeLoc = P.Tok.getLoc();
      return parseSILType(Result);
    }
    bool parseSILType(SILType &Result, SourceLoc &TypeLoc,
                      GenericParamList *&GenericParams) {
      TypeLoc = P.Tok.getLoc();
      return parseSILType(Result, GenericParams);
    }

    /// Parse a SIL type without the leading '$' or value category specifier.
    bool parseSILTypeWithoutQualifiers(SILType &Result,
                                       SILValueCategory category,
                                       const TypeAttributes &attrs,
                                       GenericParamList *&genericParams,
                                       bool IsFuncDecl = false);
    bool parseSILTypeWithoutQualifiers(SILType &Result,
                                       SILValueCategory category,
                                       const TypeAttributes &attrs) {
      GenericParamList *Junk;
      return parseSILTypeWithoutQualifiers(Result, category, attrs, Junk);
    }

    bool parseSILDottedPath(ValueDecl *&Decl,
                            SmallVectorImpl<ValueDecl *> &values);
    bool parseSILDottedPath(ValueDecl *&Decl) {
      SmallVector<ValueDecl *, 4> values;
      return parseSILDottedPath(Decl, values);
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
    bool parseGlobalName(Identifier &Name);
    bool parseValueName(UnresolvedValueName &Name);
    bool parseValueRef(SILValue &Result, SILType Ty, SILLocation Loc);
    bool parseTypedValueRef(SILValue &Result, SourceLoc &Loc);
    bool parseTypedValueRef(SILValue &Result) {
      SourceLoc Tmp;
      return parseTypedValueRef(Result, Tmp);
    }
    bool parseSILOpcode(ValueKind &Opcode, SourceLoc &OpcodeLoc,
                        StringRef &OpcodeName);

    /// \brief Parses the basic block arguments as part of branch instruction.
    bool parseSILBBArgsAtBranch(SmallVector<SILValue, 6> &Args);

    bool parseSILInstruction(SILBasicBlock *BB);
    bool parseCallInstruction(SILLocation InstLoc,
                              ValueKind Opcode, SILBuilder &B,
                              ValueBase *&ResultVal);
    bool parseSILFunctionRef(SILLocation InstLoc,
                             SILBuilder &B, ValueBase *&ResultVal);

    bool parseSILBasicBlock();
    
    bool isStartOfSILInstruction();

    ProtocolConformance *parseProtocolConformance(ProtocolDecl *&proto,
                             GenericParamList *&generics,
                             ArchetypeBuilder &builder, bool localScope);
    ProtocolConformance *parseProtocolConformance(ArchetypeBuilder &builder) {
      ProtocolDecl *dummy;
      GenericParamList *gp;
      return parseProtocolConformance(dummy, gp, builder, true);
    }

    Optional<llvm::coverage::Counter>
    parseSILCoverageExpr(llvm::coverage::CounterExpressionBuilder &Builder);
  };
} // end anonymous namespace.

bool SILParser::parseSILIdentifier(Identifier &Result, SourceLoc &Loc,
                                   const Diagnostic &D) {
  switch (P.Tok.getKind()) {
  case tok::identifier:
    Result = P.Context.getIdentifier(P.Tok.getText());
    break;
  case tok::string_literal: {
    // Drop the double quotes.
    StringRef rawString = P.Tok.getText().drop_front().drop_back();
    Result = P.Context.getIdentifier(rawString);
    break;
  }
  case tok::oper_binary_unspaced:  // fixme?
  case tok::oper_binary_spaced:
    // A binary operator can be part of a SILDeclRef.
    Result = P.Context.getIdentifier(P.Tok.getText());
    break;
  case tok::kw_deinit:
    Result = P.Context.Id_deinit;
    break;
  case tok::kw_init:
    Result = P.Context.Id_init;
    break;
  case tok::kw_subscript:
    Result = P.Context.Id_subscript;
    break;
  default:
    // If it's some other keyword, grab an identifier for it.
    if (P.Tok.isKeyword()) {
      Result = P.Context.getIdentifier(P.Tok.getText());
      break;
    }
    P.diagnose(P.Tok, D);
    return true;
  }

  Loc = P.Tok.getLoc();
  P.consumeToken();
  return false;
}

bool SILParser::parseVerbatim(StringRef name) {
  Identifier tok;
  SourceLoc loc;

  if (parseSILIdentifier(tok, loc, diag::expected_tok_in_sil_instr, name)) {
    return true;
  }
  if (tok.str() != name) {
    P.diagnose(loc, diag::expected_tok_in_sil_instr, name);
    return true;
  }
  return false;
}

/// diagnoseProblems - After a function is fully parse, emit any diagnostics
/// for errors and return true if there were any.
bool SILParser::diagnoseProblems() {
  // Check for any uses of basic blocks that were not defined.
  if (!UndefinedBlocks.empty()) {
    // FIXME: These are going to come out in nondeterminstic order.
    for (auto Entry : UndefinedBlocks)
      P.diagnose(Entry.second.first, diag::sil_undefined_basicblock_use,
                 Entry.second.second);

    HadError = true;
  }
  
  if (!ForwardRefLocalValues.empty()) {
    // FIXME: These are going to come out in nondeterminstic order.
    for (auto &Entry : ForwardRefLocalValues)
      P.diagnose(Entry.second, diag::sil_use_of_undefined_value,
                 Entry.first());
    HadError = true;
  }
  
  return HadError;
}

/// getGlobalNameForDefinition - Given a definition of a global name, look
/// it up and return an appropriate SIL function.
SILFunction *SILParser::getGlobalNameForDefinition(Identifier Name,
                                                   CanSILFunctionType Ty,
                                                   SourceLoc Loc) {
  // Check to see if a function of this name has been forward referenced.  If so
  // complete the forward reference.
  auto It = TUState.ForwardRefFns.find(Name);
  if (It != TUState.ForwardRefFns.end()) {
    SILFunction *Fn = It->second.first;
    
    // Verify that the types match up.
    if (Fn->getLoweredFunctionType() != Ty) {
      P.diagnose(Loc, diag::sil_value_use_type_mismatch, Name.str(),
                 Fn->getLoweredFunctionType(), Ty);
      P.diagnose(It->second.second, diag::sil_prior_reference);
      auto loc = SILFileLocation(Loc);
      Fn = SILFunction::create(SILMod, SILLinkage::Private, "", Ty,
                               nullptr, loc, IsNotBare, IsNotTransparent, IsNotFragile);
      Fn->setDebugScope(new (SILMod) SILDebugScope(loc, *Fn));
    }
    
    assert(Fn->isExternalDeclaration() && "Forward defns cannot have bodies!");
    TUState.ForwardRefFns.erase(It);

    // Move the function to this position in the module.
    SILMod.getFunctionList().remove(Fn);
    SILMod.getFunctionList().push_back(Fn);

    return Fn;
  }
  
  auto loc = SILFileLocation(Loc);
  // If we don't have a forward reference, make sure the function hasn't been
  // defined already.
  if (SILMod.lookUpFunction(Name.str()) != nullptr) {
    P.diagnose(Loc, diag::sil_value_redefinition, Name.str());
    auto fn = SILFunction::create(SILMod, SILLinkage::Private, "", Ty,
                                  nullptr, loc, IsNotBare, IsNotTransparent,
                                  IsNotFragile);
    fn->setDebugScope(new (SILMod) SILDebugScope(loc, *fn));
    return fn;
  }

  // Otherwise, this definition is the first use of this name.
  auto fn = SILFunction::create(SILMod, SILLinkage::Private, Name.str(),
                                Ty, nullptr, loc, IsNotBare, IsNotTransparent,
                                IsNotFragile);
  fn->setDebugScope(new (SILMod) SILDebugScope(loc, *fn));
  return fn;
}



/// getGlobalNameForReference - Given a reference to a global name, look it
/// up and return an appropriate SIL function.
SILFunction *SILParser::getGlobalNameForReference(Identifier Name,
                                                  CanSILFunctionType Ty,
                                                  SourceLoc Loc) {
  auto loc = SILFileLocation(Loc);
  
  // Check to see if we have a function by this name already.
  if (SILFunction *FnRef = SILMod.lookUpFunction(Name.str())) {
    // If so, check for matching types.
    if (FnRef->getLoweredFunctionType() != Ty) {
      P.diagnose(Loc, diag::sil_value_use_type_mismatch,
                 Name.str(), FnRef->getLoweredFunctionType(), Ty);
      FnRef = SILFunction::create(SILMod, SILLinkage::Private, "", Ty, nullptr,
                                  loc, IsNotBare, IsNotTransparent,
                                  IsNotFragile);
      FnRef->setDebugScope(new (SILMod) SILDebugScope(loc, *FnRef));
    }
    return FnRef;
  }
  
  // If we didn't find a function, create a new one - it must be a forward
  // reference.
  auto Fn = SILFunction::create(SILMod, SILLinkage::Private,
                                Name.str(), Ty, nullptr, loc, IsNotBare,
                                IsNotTransparent, IsNotFragile);
  Fn->setDebugScope(new (SILMod) SILDebugScope(loc, *Fn));
  TUState.ForwardRefFns[Name] = { Fn, Loc };
  TUState.Diags = &P.Diags;
  return Fn;
}


/// getBBForDefinition - Return the SILBasicBlock for a definition of the
/// specified block.
SILBasicBlock *SILParser::getBBForDefinition(Identifier Name, SourceLoc Loc) {
  // If there was no name specified for this block, just create a new one.
  if (Name.empty())
    return new (SILMod) SILBasicBlock(F);
  
  SILBasicBlock *&BB = BlocksByName[Name];
  // If the block has never been named yet, just create it.
  if (BB == nullptr)
    return BB = new (SILMod) SILBasicBlock(F);

  // If it already exists, it was either a forward reference or a redefinition.
  // If it is a forward reference, it should be in our undefined set.
  if (!UndefinedBlocks.erase(BB)) {
    // If we have a redefinition, return a new BB to avoid inserting
    // instructions after the terminator.
    P.diagnose(Loc, diag::sil_basicblock_redefinition, Name);
    HadError = true;
    return new (SILMod) SILBasicBlock(F);
  }

  // FIXME: Splice the block to the end of the function so they come out in the
  // right order.
  return BB;
}

/// getBBForReference - return the SILBasicBlock of the specified name.  The
/// source location is used to diagnose a failure if the block ends up never
/// being defined.
SILBasicBlock *SILParser::getBBForReference(Identifier Name, SourceLoc Loc) {
  // If the block has already been created, use it.
  SILBasicBlock *&BB = BlocksByName[Name];
  if (BB != nullptr)
    return BB;

  // Otherwise, create it and remember that this is a forward reference so
  // that we can diagnose use without definition problems.
  BB = new (SILMod) SILBasicBlock(F);
  UndefinedBlocks[BB] = {Loc, Name};
  return BB;
}

///   sil-global-name:
///     '@' identifier
bool SILParser::parseGlobalName(Identifier &Name) {
  return P.parseToken(tok::at_sign, diag::expected_sil_value_name) ||
         parseSILIdentifier(Name, diag::expected_sil_value_name);
}

/// getLocalValue - Get a reference to a local value with the specified name
/// and type.
SILValue SILParser::getLocalValue(UnresolvedValueName Name, SILType Type,
                                  SILLocation Loc) {
  if (Name.isUndef())
    return SILUndef::get(Type, &SILMod);

  // Check to see if this is already defined.
  ValueBase *&Entry = LocalValues[Name.Name];

  if (Entry) {
    // If this value is already defined, check it to make sure types match.
    SILType EntryTy;

    // If this is a reference to something with multiple results, get the right
    // one.
    if (Name.isMRV()) {
      if (Name.ResultVal >= Entry->getTypes().size()) {
        HadError = true;
        P.diagnose(Name.NameLoc, diag::invalid_sil_value_name_result_number);
        // Make sure to return something of the requested type.
        return new (SILMod) GlobalAddrInst(Loc, Type);
      }
    }

    EntryTy = Entry->getType(Name.isMRV() ? Name.ResultVal : 0);

    if (EntryTy != Type) {
      HadError = true;
      P.diagnose(Name.NameLoc, diag::sil_value_use_type_mismatch, Name.Name,
                 EntryTy.getSwiftRValueType(), Type.getSwiftRValueType());
      // Make sure to return something of the requested type.
      return new (SILMod) GlobalAddrInst(Loc, Type);
    }

    return SILValue(Entry, Name.isMRV() ? Name.ResultVal : 0);
  }
  
  // Otherwise, this is a forward reference.  Create a dummy node to represent
  // it until we see a real definition.
  ForwardRefLocalValues[Name.Name] = Name.NameLoc;

  if (!Name.isMRV()) {
    Entry = new (SILMod) GlobalAddrInst(Loc, Type);
    return Entry;
  }

  // If we have multiple results, track them through ForwardMRVLocalValues.
  std::vector<SILValue> &Placeholders = ForwardMRVLocalValues[Name.Name];
  if (Placeholders.size() <= Name.ResultVal)
    Placeholders.resize(Name.ResultVal+1);

  if (!Placeholders[Name.ResultVal])
    Placeholders[Name.ResultVal] =
      new (SILMod) GlobalAddrInst(Loc, Type);
  return Placeholders[Name.ResultVal];
}

/// setLocalValue - When an instruction or block argument is defined, this
/// method is used to register it and update our symbol table.
void SILParser::setLocalValue(ValueBase *Value, StringRef Name,
                              SourceLoc NameLoc) {
  ValueBase *&Entry = LocalValues[Name];

  // If this value was already defined, it is either a redefinition, or a
  // specification for a forward referenced value.
  if (Entry) {
    if (!ForwardRefLocalValues.erase(Name)) {
      P.diagnose(NameLoc, diag::sil_value_redefinition, Name);
      HadError = true;
      return;
    }

    // If the forward reference was of the wrong type, diagnose this now.
    if (Entry->getTypes() != Value->getTypes()) {
      // FIXME: report correct entry
      P.diagnose(NameLoc, diag::sil_value_def_type_mismatch, Name,
                 Entry->getType(0).getSwiftRValueType(),
                 Value->getType(0).getSwiftRValueType());
      HadError = true;
    } else {
      // Forward references only live here if they have a single result.
      SILValue(Entry).replaceAllUsesWith(SILValue(Value));
    }
    Entry = Value;
    return;
  }

  // Otherwise, just store it in our map.
  Entry = Value;

  // If Entry has multiple values, it may be forward referenced.
  if (Entry->getTypes().size() > 1) {
    auto It = ForwardMRVLocalValues.find(Name);
    if (It != ForwardMRVLocalValues.end()) {
      // Take the information about the forward ref out of the maps.
      std::vector<SILValue> Entries(std::move(It->second));
      SourceLoc Loc = ForwardRefLocalValues[Name];

      // Remove the entries from the maps.
      ForwardRefLocalValues.erase(Name);
      ForwardMRVLocalValues.erase(It);

      // Verify that any forward-referenced values line up.
      if (Entries.size() > Value->getTypes().size()) {
        P.diagnose(Loc, diag::sil_value_def_type_mismatch, Name,
                   Entry->getType(0).getSwiftRValueType(),
                   Value->getType(0).getSwiftRValueType());
        HadError = true;
        return;
      }

      // Validate that any forward-referenced elements have the right type, and
      // RAUW them.
      for (unsigned i = 0, e = Entries.size(); i != e; ++i) {
        if (!Entries[i]) continue;

        if (Entries[i]->getType(0) != Value->getType(i)) {
          P.diagnose(Loc, diag::sil_value_def_type_mismatch, Name,
                     Entry->getType(0).getSwiftRValueType(),
                     Value->getType(i).getSwiftRValueType());
          HadError = true;
          return;
        }

        Entries[i].replaceAllUsesWith(SILValue(Value, i));
      }
    }
  }
}


//===----------------------------------------------------------------------===//
// SIL Parsing Logic
//===----------------------------------------------------------------------===//

/// parseSILLinkage - Parse a linkage specifier if present.
///   sil-linkage:
///     /*empty*/          // default depends on whether this is a definition
///     'public'
///     'hidden'
///     'shared'
///     'private'
///     'public_external'
///     'hidden_external'
///     'private_external'
static bool parseSILLinkage(Optional<SILLinkage> &Result, Parser &P) {
  if (P.Tok.is(tok::kw_public)) {
    // Unfortunate collision with access control keywords.
    Result = SILLinkage::Public;
    P.consumeToken();
  } else if (P.Tok.is(tok::kw_private)) {
    Result = SILLinkage::Private;
    P.consumeToken();
  } else if (P.Tok.isNot(tok::identifier)) {
    Result = None;
  } else if (P.Tok.getText() == "hidden") {
    Result = SILLinkage::Hidden;
    P.consumeToken(tok::identifier);
  } else if (P.Tok.getText() == "shared") {
    Result = SILLinkage::Shared;
    P.consumeToken(tok::identifier);
  } else if (P.Tok.getText() == "public_external") {
    Result = SILLinkage::PublicExternal;
    P.consumeToken(tok::identifier);
  } else if (P.Tok.getText() == "hidden_external") {
    Result = SILLinkage::HiddenExternal;
    P.consumeToken(tok::identifier);
  } else if (P.Tok.getText() == "shared_external") {
    Result = SILLinkage::SharedExternal;
    P.consumeToken(tok::identifier);
  } else if (P.Tok.getText() == "private_external") {
    Result = SILLinkage::PrivateExternal;
    P.consumeToken(tok::identifier);
  } else {
    Result = None;
  }
  return false;
}

/// Given whether it's known to be a definition, resolve an optional
/// SIL linkage to a real one.
static SILLinkage resolveSILLinkage(Optional<SILLinkage> linkage,
                                    bool isDefinition) {
  if (linkage.hasValue()) {
    return linkage.getValue();
  } else if (isDefinition) {
    return SILLinkage::DefaultForDefinition;
  } else {
    return SILLinkage::DefaultForDeclaration;
  }
}

/// Parse an option attribute ('[' Expected ']')?
static bool parseSILOptional(bool &Result, SILParser &SP, StringRef Expected) {
  if (SP.P.consumeIf(tok::l_square)) {
    Identifier Id;
    SP.parseSILIdentifier(Id, diag::expected_in_attribute_list);
    if (Id.str() != Expected)
      return true;
    SP.P.parseToken(tok::r_square, diag::expected_in_attribute_list);
    Result = true;
  }
  return false;
}

static bool parseDeclSILOptional(bool *isTransparent, bool *isFragile,
                                 bool *isThunk, bool *isGlobalInit,
                                 Inline_t *inlineStrategy,
                                 std::string *Semantics, EffectsKind *MRK,
                                 Parser &P) {
  while (P.consumeIf(tok::l_square)) {
    if (P.Tok.isNot(tok::identifier)) {
      P.diagnose(P.Tok, diag::expected_in_attribute_list);
      return true;
    } else if (isTransparent && P.Tok.getText() == "transparent")
      *isTransparent = true;
    else if (isFragile && P.Tok.getText() == "fragile")
      *isFragile = true;
    else if (isThunk && P.Tok.getText() == "thunk")
      *isThunk = true;
    else if (isGlobalInit && P.Tok.getText() == "global_init")
      *isGlobalInit = true;
    else if (inlineStrategy && P.Tok.getText() == "noinline")
      *inlineStrategy = NoInline;
    else if (inlineStrategy && P.Tok.getText() == "always_inline")
      *inlineStrategy = AlwaysInline;
    else if (MRK && P.Tok.getText() == "readnone")
      *MRK = EffectsKind::ReadNone;
    else if (MRK && P.Tok.getText() == "readonly")
      *MRK = EffectsKind::ReadOnly;
    else if (MRK && P.Tok.getText() == "readwrite")
      *MRK = EffectsKind::ReadWrite;
    else if (Semantics && P.Tok.getText() == "semantics") {
      P.consumeToken(tok::identifier);
      if (P.Tok.getKind() != tok::string_literal) {
        P.diagnose(P.Tok, diag::expected_in_attribute_list);
        return true;
      }
  
      // Drop the double quotes.
      StringRef rawString = P.Tok.getText().drop_front().drop_back();
      *Semantics = rawString;
      P.consumeToken(tok::string_literal);

      P.parseToken(tok::r_square, diag::expected_in_attribute_list);
      continue;
    }
    else {
      P.diagnose(P.Tok, diag::expected_in_attribute_list);
      return true;
    }
    P.consumeToken(tok::identifier);
    P.parseToken(tok::r_square, diag::expected_in_attribute_list);
  }
  return false;
}

bool SILParser::performTypeLocChecking(TypeLoc &T, bool IsSIL) {
  // Do some type checking / name binding for the parsed type.
  assert(P.SF.ASTStage == SourceFile::Parsing &&
         "Unexpected stage during parsing!");
  return swift::performTypeLocChecking(P.Context, T, /*SIL*/ IsSIL, &P.SF);
}

/// Find the top-level ValueDecl or Module given a name.
static llvm::PointerUnion<ValueDecl*, Module*> lookupTopDecl(Parser &P,
             Identifier Name) {
  // Use UnqualifiedLookup to look through all of the imports.
  // We have to lie and say we're done with parsing to make this happen.
  assert(P.SF.ASTStage == SourceFile::Parsing &&
         "Unexpected stage during parsing!");
  llvm::SaveAndRestore<SourceFile::ASTStage_t> ASTStage(P.SF.ASTStage,
                                                        SourceFile::Parsed);
  UnqualifiedLookup DeclLookup(Name, &P.SF, nullptr);
  assert(DeclLookup.isSuccess() && DeclLookup.Results.size() == 1);
  if (DeclLookup.Results.back().Kind == UnqualifiedLookupResult::ModuleName) {
    Module *Mod = DeclLookup.Results.back().getNamedModule();
    return Mod;
  }
  assert(DeclLookup.Results.back().hasValueDecl());
  ValueDecl *VD = DeclLookup.Results.back().getValueDecl();
  return VD;
}

/// Find the ValueDecl given a type and a member name.
static ValueDecl *lookupMember(Parser &P, Type Ty, Identifier Name,
                               SourceLoc Loc,
                               SmallVectorImpl<ValueDecl *> &Lookup,
                               bool ExpectMultipleResults) {
  unsigned options = NL_QualifiedDefault;
  // FIXME: a bit of a hack.
  if (Name == P.Context.Id_deinit || Name == P.Context.Id_init)
    options = options & ~NL_VisitSupertypes;
  P.SF.lookupQualified(Ty, Name, options, nullptr, Lookup);

  if (Lookup.empty() || (!ExpectMultipleResults && Lookup.size() != 1)) {
    P.diagnose(Loc, diag::sil_member_decl_not_found);
    return nullptr;
  }
  return Lookup[0];
}

bool SILParser::handleGenericParams(TypeLoc &T, ArchetypeBuilder *builder) {
  if (T.wasValidated() || !T.getType().isNull())
     return false;

  if (auto fnType = dyn_cast<FunctionTypeRepr>(T.getTypeRepr()))
    if (auto gp = fnType->getGenericParams()) {
      SmallVector<ArchetypeBuilder *, 1> builders(1, builder);
      SmallVector<GenericParamList *, 1> gps(1, gp);
      handleSILGenericParams(P.Context, gps, &P.SF, builders);
    }
  return false;
}

bool SILParser::parseASTType(CanType &result) {
  ParserResult<TypeRepr> parsedType = P.parseType();
  if (parsedType.isNull()) return true;
  TypeLoc loc = parsedType.get();
  if (performTypeLocChecking(loc, false))
    return true;
  result = loc.getType()->getCanonicalType();
  return false;
}

bool SILParser::parseSILTypeWithoutQualifiers(SILType &Result,
                                              SILValueCategory category,
                                              const TypeAttributes &attrs,
                                              GenericParamList *&GenericParams,
                                              bool IsFuncDecl){
  GenericParams = nullptr;

  // If this is part of a function decl, generic parameters are visible in the
  // function body; otherwise, they are visible when parsing the type.
  Optional<Scope> GenericsScope;
  if (!IsFuncDecl)
    GenericsScope.emplace(&P, ScopeKind::Generics); 

  ParserResult<TypeRepr> TyR = P.parseType(diag::expected_sil_type);

  // Exit the scope introduced for the generic parameters.
  if (!IsFuncDecl)
    GenericsScope.reset();

  if (TyR.isNull())
    return true;

  if (auto fnType = dyn_cast<FunctionTypeRepr>(TyR.get())) {
    if (auto generics = fnType->getGenericParams()) {
      GenericParams = generics;

      // Collect the nested GenericParamLists and pass it to
      // handleSILGenericParams.
      SmallVector<GenericParamList *, 4> nestedLists;
      SmallVector<ArchetypeBuilder *, 4> builders;
      for (; generics; generics = generics->getOuterParameters()) {
        nestedLists.push_back(generics);
        auto *builder = new ArchetypeBuilder(*P.SF.getParentModule(), P.Diags);
        generics->setBuilder(builder);
        builders.push_back(builder);
      }
      handleSILGenericParams(P.Context, nestedLists, &P.SF, builders);
    }
  }
  
  // Apply attributes to the type.
  TypeLoc Ty = P.applyAttributeToType(TyR.get(), attrs);

  // We need the builders to be live here for generating GenericSignature.
  bool retCode = performTypeLocChecking(Ty);
  for (auto generics = GenericParams; generics;
       generics = generics->getOuterParameters())
    if (auto *builder = generics->getBuilder()) {
      delete builder;
      generics->setBuilder(nullptr);
    }
  if (retCode)
    return true;

  Result = SILType::getPrimitiveType(Ty.getType()->getCanonicalType(),
                                     category);
  return false;

}

///   sil-type:
///     '$' '*'? attribute-list (generic-params)? type
///
bool SILParser::parseSILType(SILType &Result, GenericParamList *&GenericParams,
                             bool IsFuncDecl){
  if (P.parseToken(tok::sil_dollar, diag::expected_sil_type))
    return true;

  // If we have a '*', then this is an address type.
  SILValueCategory category = SILValueCategory::Object;
  if (P.Tok.isAnyOperator() && P.Tok.getText() == "*") {
    category = SILValueCategory::Address;
    P.consumeToken();
  }

  // Parse attributes.
  TypeAttributes attrs;
  P.parseTypeAttributeList(attrs);

  // Global functions are implicitly @thin.
  if (IsFuncDecl && !(attrs.has(TAK_thick) || attrs.has(TAK_thin) ||
                      attrs.has(TAK_objc_block))) {
    // Use a random location.
    attrs.setAttr(TAK_thin, P.PreviousLoc);
  }

  // Handle @local_storage, which changes the SIL value category.
  if (attrs.has(TAK_local_storage)) {
    // Require '*' on local_storage values.
    if (category != SILValueCategory::Address)
      P.diagnose(attrs.getLoc(TAK_local_storage),
                 diag::sil_local_storage_non_address);
    category = SILValueCategory::LocalStorage;
    attrs.clearAttribute(TAK_local_storage);
  }
  return parseSILTypeWithoutQualifiers(Result, category, attrs, GenericParams,
                                       IsFuncDecl);
}

bool SILParser::parseSILDottedPath(ValueDecl *&Decl,
                                   SmallVectorImpl<ValueDecl *> &values) {
  if (P.parseToken(tok::pound, diag::expected_sil_constant))
    return true;

  // Handle sil-dotted-path.
  Identifier Id;
  SmallVector<Identifier, 4> FullName;
  SmallVector<SourceLoc, 4> Locs;
  do {
    Locs.push_back(P.Tok.getLoc());
    if (parseSILIdentifier(Id, diag::expected_sil_constant))
      return true;
    FullName.push_back(Id);
  } while (P.consumeIf(tok::period));

  // Look up ValueDecl from a dotted path.
  ValueDecl *VD;
  llvm::PointerUnion<ValueDecl*, Module *> Res = lookupTopDecl(P, FullName[0]);
  // It is possible that the last member lookup can return multiple lookup
  // results. One example is the overloaded member functions.
  if (Res.is<Module*>()) {
    assert(FullName.size() > 1 &&
           "A single module is not a full path to SILDeclRef");
    auto Mod = Res.get<Module*>();
    values.clear();
    VD = lookupMember(P, ModuleType::get(Mod), FullName[1], Locs[1], values,
                      FullName.size() == 2/*ExpectMultipleResults*/);
    for (unsigned I = 2, E = FullName.size(); I < E; I++) {
      values.clear();
      VD = lookupMember(P, VD->getType(), FullName[I], Locs[I], values,
                        I == FullName.size() - 1/*ExpectMultipleResults*/);
    }
  } else {
    VD = Res.get<ValueDecl*>();
    for (unsigned I = 1, E = FullName.size(); I < E; I++) {
      values.clear();
      VD = lookupMember(P, VD->getType(), FullName[I], Locs[I], values,
                        I == FullName.size() - 1/*ExpectMultipleResults*/);
    }
  }
  Decl = VD;
  return false;
}

static AccessorKind getAccessorKind(StringRef ident) {
  return llvm::StringSwitch<AccessorKind>(ident)
           .Case("getter", AccessorKind::IsGetter)
           .Case("setter", AccessorKind::IsSetter)
           .Case("addressor", AccessorKind::IsAddressor)
           .Case("mutableAddressor", AccessorKind::IsMutableAddressor)
           .Case("materializeForSet", AccessorKind::IsMaterializeForSet)
           .Default(AccessorKind::NotAccessor);
}

///  sil-decl-ref ::= '#' sil-identifier ('.' sil-identifier)* sil-decl-subref?
///  sil-decl-subref ::= '!' sil-decl-subref-part ('.' sil-decl-uncurry-level)?
///                      ('.' sil-decl-lang)?
///  sil-decl-subref ::= '!' sil-decl-uncurry-level ('.' sil-decl-lang)?
///  sil-decl-subref ::= '!' sil-decl-lang
///  sil-decl-subref-part ::= 'getter'
///  sil-decl-subref-part ::= 'setter'
///  sil-decl-subref-part ::= 'materializeForSet'
///  sil-decl-subref-part ::= 'allocator'
///  sil-decl-subref-part ::= 'initializer'
///  sil-decl-subref-part ::= 'enumelt'
///  sil-decl-subref-part ::= 'destroyer'
///  sil-decl-subref-part ::= 'globalaccessor'
///  sil-decl-uncurry-level ::= [0-9]+
///  sil-decl-lang ::= 'foreign'
bool SILParser::parseSILDeclRef(SILDeclRef &Result,
                                SmallVectorImpl<ValueDecl *> &values) {
  ValueDecl *VD;
  if (parseSILDottedPath(VD, values))
    return true;

  // Initialize Kind, uncurryLevel and IsObjC.
  SILDeclRef::Kind Kind = SILDeclRef::Kind::Func;
  unsigned uncurryLevel = 0;
  bool IsObjC = false;
  ResilienceExpansion expansion = ResilienceExpansion::Minimal;

  if (!P.consumeIf(tok::sil_exclamation)) {
    // Construct SILDeclRef.
    Result = SILDeclRef(VD, Kind, expansion, uncurryLevel, IsObjC);
    return false;
  }

  // Handle sil-constant-kind-and-uncurry-level.
  // ParseState indicates the value we just handled.
  // 1 means we just handled Kind, 2 means we just handled uncurryLevel.
  // We accept func|getter|setter|...|foreign or an integer when ParseState is
  // 0; accept foreign or an integer when ParseState is 1; accept foreign when
  // ParseState is 2.
  unsigned ParseState = 0;
  Identifier Id;
  do {
    if (P.Tok.is(tok::identifier)) {
      auto IdLoc = P.Tok.getLoc();
      if (parseSILIdentifier(Id, diag::expected_sil_constant))
        return true;
      AccessorKind accessorKind;
      if (!ParseState && Id.str() == "func") {
        Kind = SILDeclRef::Kind::Func;
        ParseState = 1;
      } else if (!ParseState &&
                 (accessorKind = getAccessorKind(Id.str()))
                    != AccessorKind::NotAccessor) {
        auto storageDecl = dyn_cast<AbstractStorageDecl>(VD);
        auto accessor = (storageDecl
                           ? storageDecl->getAccessorFunction(accessorKind)
                           : nullptr);
        if (!accessor) {
          P.diagnose(IdLoc, diag::referenced_value_no_accessor, 0);
          return true;
        }
        Kind = SILDeclRef::Kind::Func;
        VD = accessor;
        // Update values for this accessor kind.
        for (unsigned I = 0, E = values.size(); I < E; I++)
          if (auto otherDecl = dyn_cast<AbstractStorageDecl>(values[I]))
            if (auto otherAccessor = otherDecl->getAccessorFunction(accessorKind))
              values[I] = otherAccessor;
        ParseState = 1;
      } else if (!ParseState && Id.str() == "allocator") {
        Kind = SILDeclRef::Kind::Allocator;
        ParseState = 1;
      } else if (!ParseState && Id.str() == "initializer") {
        Kind = SILDeclRef::Kind::Initializer;
        ParseState = 1;
      } else if (!ParseState && Id.str() == "enumelt") {
        Kind = SILDeclRef::Kind::EnumElement;
        ParseState = 1;
      } else if (!ParseState && Id.str() == "destroyer") {
        Kind = SILDeclRef::Kind::Destroyer;
        ParseState = 1;
      } else if (!ParseState && Id.str() == "deallocator") {
        Kind = SILDeclRef::Kind::Deallocator;
        ParseState = 1;
      } else if (!ParseState && Id.str() == "globalaccessor") {
        Kind = SILDeclRef::Kind::GlobalAccessor;
        ParseState = 1;
      } else if (!ParseState && Id.str() == "globalgetter") {
        Kind = SILDeclRef::Kind::GlobalGetter;
        ParseState = 1;
      } else if (!ParseState && Id.str() == "ivardestroyer") {
        Kind = SILDeclRef::Kind::IVarDestroyer;
        ParseState = 1;
      } else if (!ParseState && Id.str() == "ivarinitializer") {
        Kind = SILDeclRef::Kind::IVarInitializer;
        ParseState = 1;
      } else if (Id.str() == "foreign") {
        IsObjC = true;
        break;
      } else
        break;
    } else if (ParseState < 2 && P.Tok.is(tok::integer_literal)) {
      P.Tok.getText().getAsInteger(0, uncurryLevel);
      P.consumeToken(tok::integer_literal);
      ParseState = 2;
    } else
      // TODO: resilience expansion?
      break;

  } while (P.consumeIf(tok::period));

  // Construct SILDeclRef.
  Result = SILDeclRef(VD, Kind, expansion, uncurryLevel, IsObjC);
  return false;
}

/// parseValueName - Parse a value name without a type available yet.
///
///     sil-value-name:
///       sil-local-name ('#' integer_literal)?
///       'undef'
///
bool SILParser::parseValueName(UnresolvedValueName &Result) {
  Result.Name = P.Tok.getText();

  if (P.Tok.is(tok::kw_undef)) {
    Result.NameLoc = P.consumeToken(tok::kw_undef);
    Result.ResultVal = ~1U;
    return false;
  }

  // Parse the local-name.
  if (P.parseToken(tok::sil_local_name, Result.NameLoc,
                   diag::expected_sil_value_name))
    return true;

  // If the result value specifier is present, parse it.
  if (P.consumeIf(tok::pound)) {
    unsigned Value = 0;
    if (P.Tok.isNot(tok::integer_literal) ||
        P.Tok.getText().getAsInteger(10, Value)) {
      P.diagnose(P.Tok, diag::expected_sil_value_name_result_number);
      return true;
    }

    P.consumeToken(tok::integer_literal);
    Result.ResultVal = Value;
  } else {
    Result.ResultVal = ~0U;
  }

  return false;
}

/// parseValueRef - Parse a value, given a contextual type.
///
///     sil-value-ref:
///       sil-local-name
///
bool SILParser::parseValueRef(SILValue &Result, SILType Ty,
                              SILLocation Loc) {
  UnresolvedValueName Name;
  if (parseValueName(Name)) return true;
  Result = getLocalValue(Name, Ty, Loc);
  return false;
}

/// parseTypedValueRef - Parse a type/value reference pair.
///
///    sil-typed-valueref:
///       sil-value-ref ':' sil-type
///
bool SILParser::parseTypedValueRef(SILValue &Result, SourceLoc &Loc) {
  Loc = P.Tok.getLoc();

  UnresolvedValueName Name;
  SILType Ty;
  if (parseValueName(Name) ||
      P.parseToken(tok::colon, diag::expected_sil_colon_value_ref) ||
      parseSILType(Ty))
    return true;
  
  Result = getLocalValue(Name, Ty, SILFileLocation(Loc));
  return false;
}


/// getInstructionKind - This method maps the string form of a SIL instruction
/// opcode to an enum.
bool SILParser::parseSILOpcode(ValueKind &Opcode, SourceLoc &OpcodeLoc,
                               StringRef &OpcodeName) {
  OpcodeLoc = P.Tok.getLoc();
  OpcodeName = P.Tok.getText();
  // Parse this textually to avoid Swift keywords (like 'return') from
  // interfering with opcode recognition.
  Opcode = llvm::StringSwitch<ValueKind>(OpcodeName)
    .Case("alloc_box", ValueKind::AllocBoxInst)
    .Case("alloc_existential_box", ValueKind::AllocExistentialBoxInst)
    .Case("address_to_pointer", ValueKind::AddressToPointerInst)
    .Case("alloc_stack", ValueKind::AllocStackInst)
    .Case("alloc_ref", ValueKind::AllocRefInst)
    .Case("alloc_ref_dynamic", ValueKind::AllocRefDynamicInst)
    .Case("alloc_value_buffer", ValueKind::AllocValueBufferInst)
    .Case("value_metatype", ValueKind::ValueMetatypeInst)
    .Case("witness_method", ValueKind::WitnessMethodInst)
    .Case("apply", ValueKind::ApplyInst)
    .Case("assign", ValueKind::AssignInst)
    .Case("autorelease_return", ValueKind::AutoreleaseReturnInst)
    .Case("autorelease_value", ValueKind::AutoreleaseValueInst)
    .Case("br", ValueKind::BranchInst)
    .Case("builtin", ValueKind::BuiltinInst)
    .Case("bridge_object_to_ref", ValueKind::BridgeObjectToRefInst)
    .Case("bridge_object_to_word", ValueKind::BridgeObjectToWordInst)
    .Case("checked_cast_br", ValueKind::CheckedCastBranchInst)
    .Case("checked_cast_addr_br", ValueKind::CheckedCastAddrBranchInst)
    .Case("class_method", ValueKind::ClassMethodInst)
    .Case("cond_br", ValueKind::CondBranchInst)
    .Case("cond_fail", ValueKind::CondFailInst)
    .Case("convert_function", ValueKind::ConvertFunctionInst)
    .Case("copy_addr", ValueKind::CopyAddrInst)
    .Case("copy_block", ValueKind::CopyBlockInst)
    .Case("dealloc_box", ValueKind::DeallocBoxInst)
    .Case("dealloc_existential_box", ValueKind::DeallocExistentialBoxInst)
    .Case("dealloc_ref", ValueKind::DeallocRefInst)
    .Case("dealloc_stack", ValueKind::DeallocStackInst)
    .Case("dealloc_value_buffer", ValueKind::DeallocValueBufferInst)
    .Case("debug_value", ValueKind::DebugValueInst)
    .Case("debug_value_addr", ValueKind::DebugValueAddrInst)
    .Case("deinit_existential_addr", ValueKind::DeinitExistentialAddrInst)
    .Case("destroy_addr", ValueKind::DestroyAddrInst)
    .Case("release_value", ValueKind::ReleaseValueInst)
    .Case("dynamic_method", ValueKind::DynamicMethodInst)
    .Case("dynamic_method_br", ValueKind::DynamicMethodBranchInst)
    .Case("enum", ValueKind::EnumInst)
    .Case("fix_lifetime", ValueKind::FixLifetimeInst)
    .Case("float_literal", ValueKind::FloatLiteralInst)
    .Case("index_addr", ValueKind::IndexAddrInst)
    .Case("index_raw_pointer", ValueKind::IndexRawPointerInst)
    .Case("init_block_storage_header", ValueKind::InitBlockStorageHeaderInst)
    .Case("init_enum_data_addr", ValueKind::InitEnumDataAddrInst)
    .Case("init_existential_addr", ValueKind::InitExistentialAddrInst)
    .Case("init_existential_metatype", ValueKind::InitExistentialMetatypeInst)
    .Case("init_existential_ref", ValueKind::InitExistentialRefInst)
    .Case("inject_enum_addr", ValueKind::InjectEnumAddrInst)
    .Case("integer_literal", ValueKind::IntegerLiteralInst)
    .Case("is_nonnull", ValueKind::IsNonnullInst)
    .Case("function_ref", ValueKind::FunctionRefInst)
    .Case("load", ValueKind::LoadInst)
    .Case("load_weak", ValueKind::LoadWeakInst)
    .Case("mark_dependence", ValueKind::MarkDependenceInst)
    .Case("mark_uninitialized", ValueKind::MarkUninitializedInst)
    .Case("mark_function_escape", ValueKind::MarkFunctionEscapeInst)
    .Case("metatype", ValueKind::MetatypeInst)
    .Case("objc_existential_metatype_to_object",
          ValueKind::ObjCExistentialMetatypeToObjectInst)
    .Case("objc_metatype_to_object", ValueKind::ObjCMetatypeToObjectInst)
    .Case("objc_protocol", ValueKind::ObjCProtocolInst)
    .Case("objc_to_thick_metatype", ValueKind::ObjCToThickMetatypeInst)
    .Case("open_existential_addr", ValueKind::OpenExistentialAddrInst)
    .Case("open_existential_box", ValueKind::OpenExistentialBoxInst)
    .Case("open_existential_metatype", ValueKind::OpenExistentialMetatypeInst)
    .Case("open_existential_ref", ValueKind::OpenExistentialRefInst)
    .Case("partial_apply", ValueKind::PartialApplyInst)
    .Case("pointer_to_address", ValueKind::PointerToAddressInst)
    .Case("pointer_to_thin_function", ValueKind::PointerToThinFunctionInst)
    .Case("project_block_storage", ValueKind::ProjectBlockStorageInst)
    .Case("project_value_buffer", ValueKind::ProjectValueBufferInst)
    .Case("existential_metatype", ValueKind::ExistentialMetatypeInst)
    .Case("raw_pointer_to_ref", ValueKind::RawPointerToRefInst)
    .Case("ref_element_addr", ValueKind::RefElementAddrInst)
    .Case("ref_to_bridge_object", ValueKind::RefToBridgeObjectInst)
    .Case("ref_to_raw_pointer", ValueKind::RefToRawPointerInst)
    .Case("ref_to_unmanaged", ValueKind::RefToUnmanagedInst)
    .Case("ref_to_unowned", ValueKind::RefToUnownedInst)
    .Case("retain_value", ValueKind::RetainValueInst)
    .Case("global_addr", ValueKind::GlobalAddrInst)
    .Case("strong_pin", ValueKind::StrongPinInst)
    .Case("strong_release", ValueKind::StrongReleaseInst)
    .Case("strong_retain", ValueKind::StrongRetainInst)
    .Case("strong_retain_autoreleased", ValueKind::StrongRetainAutoreleasedInst)
    .Case("strong_retain_unowned", ValueKind::StrongRetainUnownedInst)
    .Case("strong_unpin", ValueKind::StrongUnpinInst)
    .Case("return", ValueKind::ReturnInst)
    .Case("select_enum", ValueKind::SelectEnumInst)
    .Case("select_enum_addr", ValueKind::SelectEnumAddrInst)
    .Case("select_value", ValueKind::SelectValueInst)
    .Case("store", ValueKind::StoreInst)
    .Case("store_weak", ValueKind::StoreWeakInst)
    .Case("string_literal", ValueKind::StringLiteralInst)
    .Case("struct", ValueKind::StructInst)
    .Case("struct_element_addr", ValueKind::StructElementAddrInst)
    .Case("struct_extract", ValueKind::StructExtractInst)
    .Case("super_method", ValueKind::SuperMethodInst)
    .Case("switch_enum", ValueKind::SwitchEnumInst)
    .Case("switch_enum_addr",
          ValueKind::SwitchEnumAddrInst)
    .Case("switch_value", ValueKind::SwitchValueInst)
    .Case("unchecked_enum_data", ValueKind::UncheckedEnumDataInst)
    .Case("unchecked_addr_cast", ValueKind::UncheckedAddrCastInst)
    .Case("unchecked_trivial_bit_cast", ValueKind::UncheckedTrivialBitCastInst)
    .Case("unchecked_ref_bit_cast", ValueKind::UncheckedRefBitCastInst)
    .Case("unchecked_ref_cast", ValueKind::UncheckedRefCastInst)
    .Case("unchecked_take_enum_data_addr", ValueKind::UncheckedTakeEnumDataAddrInst)
    .Case("thick_to_objc_metatype", ValueKind::ThickToObjCMetatypeInst)
    .Case("thin_function_to_pointer", ValueKind::ThinFunctionToPointerInst)
    .Case("thin_to_thick_function", ValueKind::ThinToThickFunctionInst)
    .Case("tuple", ValueKind::TupleInst)
    .Case("tuple_element_addr", ValueKind::TupleElementAddrInst)
    .Case("tuple_extract", ValueKind::TupleExtractInst)
    .Case("unconditional_checked_cast", ValueKind::UnconditionalCheckedCastInst)
    .Case("unconditional_checked_cast_addr",
          ValueKind::UnconditionalCheckedCastAddrInst)
    .Case("unmanaged_to_ref", ValueKind::UnmanagedToRefInst)
    .Case("unowned_retain", ValueKind::UnownedRetainInst)
    .Case("unowned_release", ValueKind::UnownedReleaseInst)
    .Case("unowned_to_ref", ValueKind::UnownedToRefInst)
    .Case("unreachable", ValueKind::UnreachableInst)
    .Case("upcast", ValueKind::UpcastInst)
    .Default(ValueKind::SILArgument);

  if (Opcode != ValueKind::SILArgument) {
    P.consumeToken();
    return false;
  }
  P.diagnose(OpcodeLoc, diag::expected_sil_instr_opcode);
  return true;
}

bool SILParser::parseSILBBArgsAtBranch(SmallVector<SILValue, 6> &Args) {
  if (P.Tok.is(tok::l_paren)) {
    SourceLoc LParenLoc = P.consumeToken(tok::l_paren);
    SourceLoc RParenLoc;

    if (P.parseList(tok::r_paren, LParenLoc, RParenLoc,
                    tok::comma, /*OptionalSep=*/false,
                    /*AllowSepAfterLast=*/false,
                    diag::sil_basicblock_arg_rparen,
                    [&]() -> ParserStatus {
                      SILValue Arg;
                      SourceLoc ArgLoc;
                      if (parseTypedValueRef(Arg, ArgLoc))
                        return makeParserError();
                      Args.push_back(Arg);
                      return makeParserSuccess();
                    }).isError())
      return true;
  }
  return false;
}

/// Parse the substitution list for an apply instruction.
bool SILParser::parseApplySubstitutions(
                             SmallVectorImpl<ParsedSubstitution> &parsed) {
  // Check for an opening '<' bracket.
  if (!P.Tok.isContextualPunctuator("<"))
    return false;
  
  P.consumeToken();
  
  // Parse a list of Substitutions.
  do {
    SourceLoc Loc = P.Tok.getLoc();
    Substitution Sub;
    SILType Replace;
    Identifier ArcheId;
    TypeAttributes emptyAttrs;

    // Parse substitution as AST type.
    ParserResult<TypeRepr> TyR = P.parseType();
    if (TyR.isNull())
      return true;
    TypeLoc Ty = TyR.get();
    if (performTypeLocChecking(Ty, false))
      return true;
    parsed.push_back({Loc, ArcheId, Ty.getType()});
  } while (P.consumeIf(tok::comma));
  
  // Consume the closing '>'.
  if (!P.Tok.isContextualPunctuator(">")) {
    P.diagnose(P.Tok, diag::expected_tok_in_sil_instr, ">");
    return true;
  }
  P.consumeToken();
  
  return false;
}

/// Get ProtocolConformance for a replacement type.
static ProtocolConformance*
getConformanceOfReplacement(Parser &P, Type subReplacement,
                            ProtocolDecl *proto) {
  auto conformance = P.SF.getParentModule()->lookupConformance(
                       subReplacement, proto, nullptr);
  return conformance.getPointer();
}

/// Return true if B is derived from A.
static bool isDerivedFrom(ArchetypeType *A, ArchetypeType *B) {
  if (A == B)
    return true;
  auto *p = B;
  while ((p = p->getParent())) {
    if (p == A)
      return true;
  }
  return false;
}

/// Collect conformances by looking up the conformance from replacement
/// type and protocol decl in GenericParamList.
static bool getConformancesForSubstitution(Parser &P,
              GenericParamList *gp, ArchetypeType *subArchetype,
              Type subReplacement, SourceLoc loc,
              SmallVectorImpl<ProtocolConformance*> &conformances) {
  for (auto params = gp; params; params = params->getOuterParameters()) {
    for (auto param : *params) {
      if (param->getInherited().empty())
        continue;

      auto *pArch = param->getArchetype();
      if (!pArch || !isDerivedFrom(pArch, subArchetype))
        continue;

      // If subArchetype is C.Index and we have C inherits from P, we
      // need to find the protocol conformance to P.Index. For now, we
      // check if the replacement type conforms to subArchetype->getConformsTo.
      for (auto *proto : subArchetype->getConformsTo())
        if (auto *pConform =
            getConformanceOfReplacement(P, subReplacement, proto))
          conformances.push_back(pConform);
    }

    for (const auto &req : params->getRequirements()) {
      if (req.getKind() != RequirementKind::Conformance)
        continue;

      if (req.getSubject().getPointer() != subArchetype)
        continue;

      if (auto *protoTy = req.getConstraint()->getAs<ProtocolType>())
        if (auto *pConform =
            getConformanceOfReplacement(P, subReplacement, protoTy->getDecl()))
          conformances.push_back(pConform);
    }
  }

  if (subArchetype && !subReplacement->hasDependentProtocolConformances() &&
      conformances.size() != subArchetype->getConformsTo().size()) {
    P.diagnose(loc, diag::sil_substitution_mismatch);
    return true;
  }
  return false;
}

/// Reconstruct AST substitutions from parsed substitutions using archetypes
/// from a SILFunctionType.
bool getApplySubstitutionsFromParsed(
                             SILParser &SP,
                             GenericParamList *gp,
                             ArrayRef<ParsedSubstitution> parses,
                             SmallVectorImpl<Substitution> &subs) {
  // The replacement is for the corresponding archetype by ordering.
  for (auto subArchetype : gp->getAllNestedArchetypes()) {
    auto parsed = parses.front();
    parses = parses.slice(1);

    SmallVector<ProtocolConformance*, 2> conformances;
    if (getConformancesForSubstitution(SP.P, gp, subArchetype,
                                       parsed.replacement,
                                       parsed.loc, conformances))
      return true;

    subs.push_back({subArchetype, parsed.replacement,
                    SP.P.Context.AllocateCopy(conformances)});
  }
  assert(parses.empty() && "did not use all substitutions?");
  return false;
}

// FIXME: we work around canoicalization of PolymorphicFunctionType
// by generating GenericSignature and transforming the input, output
// types.
static GenericSignature *canonicalPolymorphicFunctionType(
                           PolymorphicFunctionType *Ty,
                           ASTContext &Context,
                           CanType &inTy, CanType &outTy) {
  GenericSignature *genericSig = nullptr;
  llvm::DenseMap<ArchetypeType*, Type> archetypeMap;
  genericSig
    = Ty->getGenericParams().getAsCanonicalGenericSignature(archetypeMap,
                                                            Context);
  
  auto getArchetypesAsDependentTypes = [&](Type t) -> Type {
    if (!t) return t;
    if (auto arch = t->getAs<ArchetypeType>()) {
      // As a kludge, we allow Self archetypes of protocol_methods to be
      // unapplied.
      if (arch->getSelfProtocol() && !archetypeMap.count(arch))
        return arch;
      return arch->getAsDependentType(archetypeMap);
    }
    return t;
  };
  
  inTy = Ty->getInput()
    .transform(getArchetypesAsDependentTypes)
    ->getCanonicalType();
  outTy = Ty->getResult()
    .transform(getArchetypesAsDependentTypes)
    ->getCanonicalType();
  return genericSig;
}

static bool checkPolymorphicFunctionType(PolymorphicFunctionType *Ty,
                                         PolymorphicFunctionType *Ty2,
                                         ASTContext &Context, unsigned depth) {
  bool InputIsPoly = isa<PolymorphicFunctionType>(Ty->getInput().getPointer());
  bool OutputIsPoly =
    isa<PolymorphicFunctionType>(Ty->getResult().getPointer());
  bool Input2IsPoly =
    isa<PolymorphicFunctionType>(Ty2->getInput().getPointer());
  bool Output2IsPoly =
    isa<PolymorphicFunctionType>(Ty2->getResult().getPointer());

  if ((InputIsPoly && !Input2IsPoly) || (!InputIsPoly && Input2IsPoly) ||
      (OutputIsPoly && !Output2IsPoly) || (!OutputIsPoly && Output2IsPoly))
    return false;

  // FIXME: We currently have issues canonicalizing PolymorphicFunctionType
  // whose input type or result type is also a PolymorphicFunctionType. We also
  // have issues when PolymorphicFunctionType is part of a FunctionType.
  // rdar://18021608
  if (InputIsPoly || OutputIsPoly || depth)
    return true;

  // Try to canonicalize PolymorphicFunctionType and compare.
  CanType inTy, outTy, inTy2, outTy2;
  auto sig = canonicalPolymorphicFunctionType(Ty, Context, inTy, outTy);
  auto sig2 = canonicalPolymorphicFunctionType(Ty2, Context, inTy2, outTy2);
  return sig == sig2 && inTy == inTy2 && outTy == outTy2;
}

static bool checkFunctionType(FunctionType *Ty, FunctionType *Ty2,
                              ASTContext &Context);

static bool checkASTType(CanType Ty, CanType Ty2, ASTContext &Context,
                         unsigned depth) {
  if (Ty == Ty2)
    return true;
  if (auto *InputTy = dyn_cast<FunctionType>(Ty.getPointer())) {
    auto *Input2Ty = dyn_cast<FunctionType>(Ty2.getPointer());
    if (!Input2Ty)
      return false;
    return checkFunctionType(InputTy, Input2Ty, Context);
  }
  if (auto *InputPoly = dyn_cast<PolymorphicFunctionType>(Ty.getPointer())) {
    auto *Input2Poly = dyn_cast<PolymorphicFunctionType>(Ty2.getPointer());
    if (!Input2Poly)
      return false;
    return checkPolymorphicFunctionType(InputPoly, Input2Poly, Context, depth);
  }
  return false;
}

static bool checkFunctionType(FunctionType *Ty, FunctionType *Ty2,
                              ASTContext &Context) {
  return Ty == Ty2 ||
         (checkASTType(Ty->getInput()->getCanonicalType(),
                       Ty2->getInput()->getCanonicalType(), Context, 1) &&
          checkASTType(Ty->getResult()->getCanonicalType(),
                       Ty2->getResult()->getCanonicalType(), Context, 1));
}

static ArrayRef<ProtocolConformance *>
collectExistentialConformances(Parser &P,
                               CanType conformingType, CanType protocolType) {
  SmallVector<ProtocolDecl *, 2> protocols;
  bool isExistential = protocolType->isAnyExistentialType(protocols);
  assert(isExistential);
  (void)isExistential;
  if (protocols.empty())
    return {};
  
  MutableArrayRef<ProtocolConformance *> conformances =
    P.Context.Allocate<ProtocolConformance*>(protocols.size());
  
  for (unsigned i : indices(protocols)) {
    conformances[i]
      = getConformanceOfReplacement(P, conformingType, protocols[i]);
  }
  
  return conformances;
}

///   sil-instruction:
///     (sil_local_name '=')? identifier ...
bool SILParser::parseSILInstruction(SILBasicBlock *BB) {
  // We require SIL instructions to be at the start of a line to assist
  // recovery.
  if (!P.Tok.isAtStartOfLine()) {
    P.diagnose(P.Tok, diag::expected_sil_instr_start_of_line);
    return true;
  }

  StringRef ResultName;
  SourceLoc ResultNameLoc;

  // If the instruction has a name '%foo =', parse it.
  if (P.Tok.is(tok::sil_local_name)) {
    ResultName = P.Tok.getText();
    ResultNameLoc = P.Tok.getLoc();
    P.consumeToken(tok::sil_local_name);
    if (P.parseToken(tok::equal, diag::expected_equal_in_sil_instr))
      return true;
  }
  
  ValueKind Opcode;
  SourceLoc OpcodeLoc;
  StringRef OpcodeName;
  
  // Parse the opcode name.
  if (parseSILOpcode(Opcode, OpcodeLoc, OpcodeName))
    return true;

  SILBuilder B(BB);
  SmallVector<SILValue, 4> OpList;
  SILValue Val;

  SILLocation InstLoc = SILFileLocation(OpcodeLoc);

  auto parseCastConsumptionKind = [&](Identifier name, SourceLoc loc,
                                      CastConsumptionKind &out) -> bool {
    auto kind = llvm::StringSwitch<Optional<CastConsumptionKind>>(name.str())
      .Case("take_always", CastConsumptionKind::TakeAlways)
      .Case("take_on_success", CastConsumptionKind::TakeOnSuccess)
      .Case("copy_on_success", CastConsumptionKind::CopyOnSuccess)
      .Default(None);

    if (kind) {
      out = kind.getValue();
      return false;
    }
    P.diagnose(loc, diag::expected_tok_in_sil_instr, "cast consumption kind");
    return true;
  };
  
  // Validate the opcode name, and do opcode-specific parsing logic based on the
  // opcode we find.
  ValueBase *ResultVal;
  switch (Opcode) {
  case ValueKind::SILArgument:
  case ValueKind::SILUndef:
    llvm_unreachable("not an instruction");

  case ValueKind::AllocBoxInst: {
    SILType Ty;
    if (parseSILType(Ty)) return true;
    ResultVal = B.createAllocBox(InstLoc, Ty);
    break;
  }
  case ValueKind::ApplyInst:
  case ValueKind::PartialApplyInst:
    if (parseCallInstruction(InstLoc, Opcode, B, ResultVal))
      return true;
    break;
  case ValueKind::IntegerLiteralInst: {
    SILType Ty;
    if (parseSILType(Ty) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ","))
      return true;
    
    bool Negative = false;
    if (P.Tok.isAnyOperator() && P.Tok.getText() == "-") {
      Negative = true;
      P.consumeToken();
    }
    if (P.Tok.getKind() != tok::integer_literal) {
      P.diagnose(P.Tok, diag::expected_tok_in_sil_instr, "integer");
      return true;
    }
    
    auto intTy = Ty.getAs<BuiltinIntegerType>();
    if (!intTy) {
      P.diagnose(P.Tok, diag::sil_integer_literal_not_integer_type);
      return true;
    }
    
    APInt value(intTy->getGreatestWidth(), 0);
    bool error = P.Tok.getText().getAsInteger(0, value);
    assert(!error && "integer_literal token did not parse as APInt?!");
    (void)error;
    
    if (Negative)
      value = -value; 
    if (value.getBitWidth() != intTy->getGreatestWidth())
      value = value.zextOrTrunc(intTy->getGreatestWidth());
    
    ResultVal = B.createIntegerLiteral(InstLoc, Ty, value);
    P.consumeToken(tok::integer_literal);
    break;
  }
  case ValueKind::FloatLiteralInst: {
    SILType Ty;
    if (parseSILType(Ty) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ","))
      return true;
   
    // The value is expressed as bits.
    if (P.Tok.getKind() != tok::integer_literal) {
      P.diagnose(P.Tok, diag::expected_tok_in_sil_instr, "integer");
      return true;
    }
    
    auto floatTy = Ty.getAs<BuiltinFloatType>();
    if (!floatTy) {
      P.diagnose(P.Tok, diag::sil_float_literal_not_float_type);
      return true;
    }
    
    APInt bits(floatTy->getBitWidth(), 0);
    bool error = P.Tok.getText().getAsInteger(0, bits);
    assert(!error && "float_literal token did not parse as APInt?!");
    (void)error;
    
    if (bits.getBitWidth() != floatTy->getBitWidth())
      bits = bits.zextOrTrunc(floatTy->getBitWidth());
    
    APFloat value(floatTy->getAPFloatSemantics(), bits);
    
    ResultVal = B.createFloatLiteral(InstLoc, Ty, value);
    P.consumeToken(tok::integer_literal);
    break;
  }
  case ValueKind::StringLiteralInst: {
    if (P.Tok.getKind() != tok::identifier) {
      P.diagnose(P.Tok, diag::sil_string_no_encoding);
      return true;
    }

    StringLiteralInst::Encoding encoding;
    if (P.Tok.getText() == "utf8") {
      encoding = StringLiteralInst::Encoding::UTF8;
    } else if (P.Tok.getText() == "utf16") {
      encoding = StringLiteralInst::Encoding::UTF16;
    } else {
      P.diagnose(P.Tok, diag::sil_string_invalid_encoding, P.Tok.getText());
      return true;
    }
    P.consumeToken(tok::identifier);

    if (P.Tok.getKind() != tok::string_literal) {
      P.diagnose(P.Tok, diag::expected_tok_in_sil_instr, "string");
      return true;
    }
   
    // Drop the double quotes.
    StringRef rawString = P.Tok.getText().drop_front().drop_back();

    // Ask the lexer to interpret the entire string as a literal segment.
    SmallVector<char, 128> stringBuffer;
    StringRef string = P.L->getEncodedStringSegment(rawString, stringBuffer);
    ResultVal = B.createStringLiteral(InstLoc, string, encoding);
    P.consumeToken(tok::string_literal);
    break;
  }

  case ValueKind::AllocValueBufferInst: {
    SILType Ty;
    if (parseSILType(Ty) ||
        parseVerbatim("in") ||
        parseTypedValueRef(Val))
      return true;
    ResultVal = B.createAllocValueBuffer(InstLoc, Ty, Val);
    break;
  }
  case ValueKind::ProjectValueBufferInst: {
    SILType Ty;
    if (parseSILType(Ty) ||
        parseVerbatim("in") ||
        parseTypedValueRef(Val))
      return true;
    ResultVal = B.createProjectValueBuffer(InstLoc, Ty, Val);
    break;
  }
  case ValueKind::DeallocValueBufferInst: {
    SILType Ty;
    if (parseSILType(Ty) ||
        parseVerbatim("in") ||
        parseTypedValueRef(Val))
      return true;
    ResultVal = B.createDeallocValueBuffer(InstLoc, Ty, Val);
    break;
  }
      
  case ValueKind::FunctionRefInst:
    if (parseSILFunctionRef(InstLoc, B, ResultVal))
      return true;
    break;
  case ValueKind::BuiltinInst: {
    if (P.Tok.getKind() != tok::string_literal) {
      P.diagnose(P.Tok, diag::expected_tok_in_sil_instr,"builtin name");
      return true;
    }
    StringRef Str = P.Tok.getText();
    Identifier Id = P.Context.getIdentifier(Str.substr(1, Str.size()-2));
    P.consumeToken(tok::string_literal);
    
    // Find the builtin in the Builtin module
    SmallVector<ValueDecl*, 2> foundBuiltins;
    P.Context.TheBuiltinModule->lookupMember(foundBuiltins,
                                             P.Context.TheBuiltinModule, Id,
                                             Identifier());
    if (foundBuiltins.empty()) {
      P.diagnose(P.Tok, diag::expected_tok_in_sil_instr,"builtin name");
      return true;
    }
    assert(foundBuiltins.size() == 1 && "ambiguous builtin name?!");
    
    GenericParamList *genericParams
      = cast<FuncDecl>(foundBuiltins[0])->getGenericParams();
    
    SmallVector<ParsedSubstitution, 4> parsedSubs;
    SmallVector<Substitution, 4> subs;
    if (parseApplySubstitutions(parsedSubs))
      return true;
    
    if (!parsedSubs.empty()) {
      if (!genericParams) {
        P.diagnose(P.Tok, diag::sil_substitutions_on_non_polymorphic_type);
        return true;
      }
      if (getApplySubstitutionsFromParsed(*this, genericParams,parsedSubs,subs))
        return true;
    }
    
    if (P.Tok.getKind() != tok::l_paren) {
      P.diagnose(P.Tok, diag::expected_tok_in_sil_instr, "(");
      return true;
    }
    P.consumeToken(tok::l_paren);

    SmallVector<SILValue, 4> Args;
    while (true) {
      if (P.consumeIf(tok::r_paren))
        break;

      SILValue Val;
      if (parseTypedValueRef(Val))
        return true;
      Args.push_back(Val);
      if (P.consumeIf(tok::comma))
        continue;
      if (P.consumeIf(tok::r_paren))
        break;
      P.diagnose(P.Tok, diag::expected_tok_in_sil_instr, "(' or ',");
      return true;
    }
    
    if (P.Tok.getKind() != tok::colon) {
      P.diagnose(P.Tok, diag::expected_tok_in_sil_instr, ":");
      return true;
    }
    P.consumeToken(tok::colon);
    
    SILType ResultTy;
    if (parseSILType(ResultTy))
      return true;
    
    ResultVal = B.createBuiltin(InstLoc, Id, ResultTy, subs, Args);
    break;
  }
  case ValueKind::OpenExistentialAddrInst:
  case ValueKind::OpenExistentialBoxInst:
  case ValueKind::OpenExistentialMetatypeInst:
  case ValueKind::OpenExistentialRefInst: {
    SILType Ty;
    Identifier ToToken;
    SourceLoc ToLoc;
    
    if (parseTypedValueRef(Val) ||
        parseSILIdentifier(ToToken, ToLoc,
                           diag::expected_tok_in_sil_instr, "to") ||
        parseSILType(Ty))
      return true;
    
    if (ToToken.str() != "to") {
      P.diagnose(ToLoc, diag::expected_tok_in_sil_instr, "to");
      return true;
    }

    switch (Opcode) {
    case ValueKind::OpenExistentialAddrInst:
      ResultVal = B.createOpenExistentialAddr(InstLoc, Val, Ty);
      break;

    case ValueKind::OpenExistentialMetatypeInst:
      ResultVal = B.createOpenExistentialMetatype(InstLoc, Val, Ty);
      break;

    case ValueKind::OpenExistentialRefInst:
      ResultVal = B.createOpenExistentialRef(InstLoc, Val, Ty);
      break;

    case ValueKind::OpenExistentialBoxInst:
      ResultVal = B.createOpenExistentialBox(InstLoc, Val, Ty);
      break;

    default:
      llvm_unreachable("Inner switch out of sync with outer switch");
    }
    break;
  }
#define UNARY_INSTRUCTION(ID) \
  case ValueKind::ID##Inst:                   \
    if (parseTypedValueRef(Val)) return true; \
    ResultVal = B.create##ID(InstLoc, Val);   \
    break;
  UNARY_INSTRUCTION(FixLifetime)
  UNARY_INSTRUCTION(CopyBlock)
  UNARY_INSTRUCTION(StrongPin)
  UNARY_INSTRUCTION(StrongRetain)
  UNARY_INSTRUCTION(StrongRelease)
  UNARY_INSTRUCTION(StrongRetainAutoreleased)
  UNARY_INSTRUCTION(StrongUnpin)
  UNARY_INSTRUCTION(AutoreleaseReturn)
  UNARY_INSTRUCTION(StrongRetainUnowned)
  UNARY_INSTRUCTION(UnownedRetain)
  UNARY_INSTRUCTION(UnownedRelease)
  UNARY_INSTRUCTION(DestroyAddr)
  UNARY_INSTRUCTION(AutoreleaseValue)
  UNARY_INSTRUCTION(ReleaseValue)
  UNARY_INSTRUCTION(RetainValue)
  UNARY_INSTRUCTION(Load)
  UNARY_INSTRUCTION(CondFail)
  UNARY_INSTRUCTION(DebugValue)
  UNARY_INSTRUCTION(DebugValueAddr)
#undef UNARY_INSTRUCTION

  case ValueKind::LoadWeakInst: {
    bool isTake = false;
    if (parseSILOptional(isTake, *this, "take") ||
        parseTypedValueRef(Val))
      return true;

    ResultVal = B.createLoadWeak(InstLoc, Val, IsTake_t(isTake));
    break;
  }

  case ValueKind::MarkDependenceInst: {
    SILValue Base;
    if (parseTypedValueRef(Val) ||
        parseVerbatim("on") ||
        parseTypedValueRef(Base))
      return true;

    ResultVal = B.createMarkDependence(InstLoc, Val, Base);
    break;
  }

    // Conversion instructions.
  case ValueKind::UncheckedRefCastInst:
  case ValueKind::UncheckedAddrCastInst:
  case ValueKind::UncheckedTrivialBitCastInst:
  case ValueKind::UncheckedRefBitCastInst:
  case ValueKind::UpcastInst:
  case ValueKind::AddressToPointerInst:
  case ValueKind::PointerToAddressInst:
  case ValueKind::BridgeObjectToRefInst:
  case ValueKind::BridgeObjectToWordInst:
  case ValueKind::RefToRawPointerInst:
  case ValueKind::RawPointerToRefInst:
  case ValueKind::RefToUnownedInst:
  case ValueKind::UnownedToRefInst:
  case ValueKind::RefToUnmanagedInst:
  case ValueKind::UnmanagedToRefInst:
  case ValueKind::ThinFunctionToPointerInst:
  case ValueKind::PointerToThinFunctionInst:
  case ValueKind::ThinToThickFunctionInst:
  case ValueKind::ThickToObjCMetatypeInst:
  case ValueKind::ObjCToThickMetatypeInst:
  case ValueKind::ConvertFunctionInst:
  case ValueKind::ObjCExistentialMetatypeToObjectInst:
  case ValueKind::ObjCMetatypeToObjectInst: {
    SILType Ty;
    Identifier ToToken;
    SourceLoc ToLoc;
    if (parseTypedValueRef(Val) ||
        parseSILIdentifier(ToToken, ToLoc,
                           diag::expected_tok_in_sil_instr, "to") ||
        parseSILType(Ty))
      return true;

    if (ToToken.str() != "to") {
      P.diagnose(ToLoc, diag::expected_tok_in_sil_instr, "to");
      return true;
    }

    switch (Opcode) {
    default: llvm_unreachable("Out of sync with parent switch");
    case ValueKind::UncheckedRefCastInst:
      ResultVal = B.createUncheckedRefCast(InstLoc, Val, Ty);
      break;
    case ValueKind::UncheckedAddrCastInst:
      ResultVal = B.createUncheckedAddrCast(InstLoc, Val, Ty);
      break;
    case ValueKind::UncheckedTrivialBitCastInst:
      ResultVal = B.createUncheckedTrivialBitCast(InstLoc, Val, Ty);
      break;
    case ValueKind::UncheckedRefBitCastInst:
      ResultVal = B.createUncheckedRefBitCast(InstLoc, Val, Ty);
      break;
    case ValueKind::UpcastInst:
      ResultVal = B.createUpcast(InstLoc, Val, Ty);
      break;
    case ValueKind::ConvertFunctionInst:
      ResultVal = B.createConvertFunction(InstLoc, Val, Ty);
      break;
    case ValueKind::AddressToPointerInst:
      ResultVal = B.createAddressToPointer(InstLoc, Val, Ty);
      break;
    case ValueKind::PointerToAddressInst:
      ResultVal = B.createPointerToAddress(InstLoc, Val, Ty);
      break;
    case ValueKind::BridgeObjectToRefInst:
      ResultVal = B.createBridgeObjectToRef(InstLoc, Val, Ty);
      break;
    case ValueKind::BridgeObjectToWordInst:
      ResultVal = B.createBridgeObjectToWord(InstLoc, Val);
      break;
    case ValueKind::RefToRawPointerInst:
      ResultVal = B.createRefToRawPointer(InstLoc, Val, Ty);
      break;
    case ValueKind::RawPointerToRefInst:
      ResultVal = B.createRawPointerToRef(InstLoc, Val, Ty);
      break;
    case ValueKind::RefToUnownedInst:
      ResultVal = B.createRefToUnowned(InstLoc, Val, Ty);
      break;
    case ValueKind::UnownedToRefInst:
      ResultVal = B.createUnownedToRef(InstLoc, Val, Ty);
      break;
    case ValueKind::RefToUnmanagedInst:
      ResultVal = B.createRefToUnmanaged(InstLoc, Val, Ty);
      break;
    case ValueKind::UnmanagedToRefInst:
      ResultVal = B.createUnmanagedToRef(InstLoc, Val, Ty);
      break;
    case ValueKind::ThinFunctionToPointerInst:
      ResultVal = B.createThinFunctionToPointer(InstLoc, Val, Ty);
      break;
    case ValueKind::PointerToThinFunctionInst:
      ResultVal = B.createPointerToThinFunction(InstLoc, Val, Ty);
      break;
    case ValueKind::ThinToThickFunctionInst:
      ResultVal = B.createThinToThickFunction(InstLoc, Val, Ty);
      break;
    case ValueKind::ThickToObjCMetatypeInst:
      ResultVal = B.createThickToObjCMetatype(InstLoc, Val, Ty);
      break;
    case ValueKind::ObjCToThickMetatypeInst:
      ResultVal = B.createObjCToThickMetatype(InstLoc, Val, Ty);
      break;
    case ValueKind::ObjCMetatypeToObjectInst:
      ResultVal = B.createObjCMetatypeToObject(InstLoc, Val, Ty);
      break;
    case ValueKind::ObjCExistentialMetatypeToObjectInst:
      ResultVal = B.createObjCExistentialMetatypeToObject(InstLoc, Val, Ty);
      break;
    }
    break;
  }
      
  case ValueKind::RefToBridgeObjectInst: {
    SILValue BitsVal;
    if (parseTypedValueRef(Val) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseTypedValueRef(BitsVal))
      return true;
    ResultVal = B.createRefToBridgeObject(InstLoc, Val, BitsVal);
    break;
  }
      
  // Indirect checked conversion instructions.
  case ValueKind::UnconditionalCheckedCastAddrInst:
  case ValueKind::CheckedCastAddrBranchInst: {
    CastConsumptionKind consumptionKind;
    {
      Identifier consumptionKindToken;
      SourceLoc consumptionKindLoc;
      if (parseSILIdentifier(consumptionKindToken, consumptionKindLoc,
                             diag::expected_tok_in_sil_instr,
                             "cast consumption kind") ||
          parseCastConsumptionKind(consumptionKindToken,
                                   consumptionKindLoc,
                                   consumptionKind))
        return true;
    }

    auto parseFormalTypeAndValue = [&](CanType &formalType,
                                       SILValue &value) -> bool {
      return (parseASTType(formalType) ||
              parseVerbatim("in") ||
              parseTypedValueRef(value));
    };

    CanType sourceType, targetType;
    SILValue sourceAddr, destAddr;

    if (parseFormalTypeAndValue(sourceType, sourceAddr) ||
        parseVerbatim("to") ||
        parseFormalTypeAndValue(targetType, destAddr))
      return true;

    if (Opcode == ValueKind::UnconditionalCheckedCastAddrInst) {
      ResultVal = B.createUnconditionalCheckedCastAddr(InstLoc,
                                                       consumptionKind,
                                                       sourceAddr, sourceType,
                                                       destAddr, targetType);
      break;
    }

    // The conditional cast still needs its branch destinations.
    Identifier successBBName, failureBBName;
    SourceLoc successBBLoc, failureBBLoc;
    if (P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",")
        || parseSILIdentifier(successBBName, successBBLoc,
                              diag::expected_sil_block_name)
        || P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",")
        || parseSILIdentifier(failureBBName, failureBBLoc,
                              diag::expected_sil_block_name))
      return true;

    ResultVal = B.createCheckedCastAddrBranch(InstLoc, consumptionKind,
                                              sourceAddr, sourceType,
                                              destAddr, targetType,
                                getBBForReference(successBBName, successBBLoc),
                                getBBForReference(failureBBName, failureBBLoc));
    break;
  }

  // Checked Conversion instructions.
  case ValueKind::UnconditionalCheckedCastInst:
  case ValueKind::CheckedCastBranchInst: {
    SILType ty;
    SILValue destVal;
    Identifier kindToken, toToken;
    SourceLoc kindLoc, toLoc;

    bool isExact = false;
    if (Opcode == ValueKind::CheckedCastBranchInst &&
        parseSILOptional(isExact, *this, "exact"))
      return true;
    
    if (parseTypedValueRef(Val) ||
        parseVerbatim("to") ||
        parseSILType(ty))
      return true;
    
    // An unconditional cast instruction is finished here.
    if (Opcode == ValueKind::UnconditionalCheckedCastInst) {
      ResultVal = B.createUnconditionalCheckedCast(InstLoc, Val, ty);
      break;
    }    
    // The conditional cast still needs its branch destinations.
    Identifier successBBName, failureBBName;
    SourceLoc successBBLoc, failureBBLoc;
    if (P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",")
        || parseSILIdentifier(successBBName, successBBLoc,
                              diag::expected_sil_block_name)
        || P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",")
        || parseSILIdentifier(failureBBName, failureBBLoc,
                              diag::expected_sil_block_name))
      return true;

    ResultVal = B.createCheckedCastBranch(InstLoc, isExact, Val, ty,
                                getBBForReference(successBBName, successBBLoc),
                                getBBForReference(failureBBName, failureBBLoc));
    break;
  }

  case ValueKind::MarkUninitializedInst: {
    if (P.parseToken(tok::l_square, diag::expected_tok_in_sil_instr, "["))
      return true;
    
    Identifier KindId;
    SourceLoc KindLoc = P.Tok.getLoc();
    if (P.consumeIf(tok::kw_var))
      KindId = P.Context.getIdentifier("var");
    else if (P.parseIdentifier(KindId, KindLoc,
                               diag::expected_tok_in_sil_instr, "kind"))
      return true;
    
    if (P.parseToken(tok::r_square, diag::expected_tok_in_sil_instr, "]"))
      return true;

    MarkUninitializedInst::Kind Kind;
    if (KindId.str() == "var")
      Kind = MarkUninitializedInst::Var;
    else if (KindId.str() == "rootself")
      Kind = MarkUninitializedInst::RootSelf;
    else if (KindId.str() == "derivedself")
      Kind = MarkUninitializedInst::DerivedSelf;
    else if (KindId.str() == "derivedselfonly")
      Kind = MarkUninitializedInst::DerivedSelfOnly;
    else if (KindId.str() == "delegatingself")
      Kind = MarkUninitializedInst::DelegatingSelf;
    else {
      P.diagnose(KindLoc, diag::expected_tok_in_sil_instr,
                 "var, rootself, derivedself, derivedselfonly, "
                 "or delegatingself");
      return true;
    }

    if (parseTypedValueRef(Val)) return true;
    ResultVal = B.createMarkUninitialized(InstLoc, Val, Kind);
    break;
  }
  case ValueKind::MarkFunctionEscapeInst: {
    SmallVector<SILValue, 4> OpList;
    do {
      if (parseTypedValueRef(Val)) return true;
      OpList.push_back(Val);
    } while (P.consumeIf(tok::comma));

    ResultVal = B.createMarkFunctionEscape(InstLoc, OpList);
    break;
  }

  case ValueKind::AssignInst:
  case ValueKind::StoreInst:
  case ValueKind::StoreWeakInst: {
    UnresolvedValueName from;

    SourceLoc toLoc, addrLoc;
    Identifier toToken;
    SILValue addrVal;
    bool isInit = false;
    if (parseValueName(from) ||
        parseSILIdentifier(toToken, toLoc,
                           diag::expected_tok_in_sil_instr, "to") ||
        (Opcode == ValueKind::StoreWeakInst &&
         parseSILOptional(isInit, *this, "initialization")) ||
        parseTypedValueRef(addrVal, addrLoc))
      return true;

    if (toToken.str() != "to") {
      P.diagnose(toLoc, diag::expected_tok_in_sil_instr, "to");
      return true;
    }

    if (!addrVal.getType().isAddress()) {
      P.diagnose(addrLoc, diag::sil_operand_not_address,
                 "destination", OpcodeName);
      return true;
    }

    if (Opcode == ValueKind::StoreWeakInst) {
      auto refType = addrVal.getType().getAs<WeakStorageType>();
      if (!refType) {
        P.diagnose(addrLoc, diag::sil_operand_not_weak_address,
                   "destination", OpcodeName);
        return true;
      }
      auto valueTy = SILType::getPrimitiveObjectType(refType.getReferentType());
      ResultVal = B.createStoreWeak(InstLoc,
                                    getLocalValue(from, valueTy, InstLoc),
                                    addrVal, IsInitialization_t(isInit));
      break;
    }

    SILType ValType = addrVal.getType().getObjectType();

    if (Opcode == ValueKind::StoreInst) {
      ResultVal = B.createStore(InstLoc,
                                getLocalValue(from, ValType, InstLoc),
                                addrVal);
      break;
    }

    assert(Opcode == ValueKind::AssignInst);
    ResultVal = B.createAssign(InstLoc,
                               getLocalValue(from, ValType, InstLoc),
                               addrVal);
    break;
  }
  case ValueKind::AllocStackInst:
  case ValueKind::AllocRefInst:
  case ValueKind::MetatypeInst: {
    bool IsObjC = false;
    if (Opcode == ValueKind::AllocRefInst &&
        parseSILOptional(IsObjC, *this, "objc"))
      return true;

    SILType Ty;
    if (parseSILType(Ty))
      return true;
    
    if (Opcode == ValueKind::AllocStackInst)
      ResultVal = B.createAllocStack(InstLoc, Ty);
    else if (Opcode == ValueKind::AllocRefInst) {
      ResultVal = B.createAllocRef(InstLoc, Ty, IsObjC);
    } else {
      assert(Opcode == ValueKind::MetatypeInst);
      ResultVal = B.createMetatype(InstLoc, Ty);
    }
    break;
  }
  case ValueKind::AllocRefDynamicInst: {
    SILType Ty;
    bool isObjC = false;
    if (parseSILOptional(isObjC, *this, "objc") ||
        parseTypedValueRef(Val) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseSILType(Ty))
      return true;

    ResultVal = B.createAllocRefDynamic(InstLoc, Val, Ty, isObjC);
    break;
  }

  case ValueKind::DeallocStackInst:
  case ValueKind::DeallocRefInst:
    if (parseTypedValueRef(Val))
      return true;
    if (Opcode == ValueKind::DeallocStackInst) {
      ResultVal = B.createDeallocStack(InstLoc, Val);
    } else {
      assert(Opcode == ValueKind::DeallocRefInst);
      ResultVal = B.createDeallocRef(InstLoc, Val);
    }
    break;
  case ValueKind::DeallocBoxInst:
  case ValueKind::ValueMetatypeInst:
  case ValueKind::ExistentialMetatypeInst: {
    SILType Ty;
    if (parseSILType(Ty) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseTypedValueRef(Val))
      return true;
    switch (Opcode) {
    default: llvm_unreachable("Out of sync with parent switch");
    case ValueKind::ValueMetatypeInst:
      ResultVal = B.createValueMetatype(InstLoc, Ty, Val);
      break;
    case ValueKind::ExistentialMetatypeInst:
      ResultVal = B.createExistentialMetatype(InstLoc, Ty, Val);
      break;
    case ValueKind::DeallocBoxInst:
      ResultVal = B.createDeallocBox(InstLoc, Ty, Val);
      break;
    }
    break;
  }
  case ValueKind::DeallocExistentialBoxInst: {
    CanType ConcreteTy;
    if (parseTypedValueRef(Val)
        || P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",")
        || P.parseToken(tok::sil_dollar, diag::expected_tok_in_sil_instr, "$")
        || parseASTType(ConcreteTy))
      return true;
    
    ResultVal = B.createDeallocExistentialBox(InstLoc, ConcreteTy, Val);
    break;
  }
  case ValueKind::TupleInst: {
    // Tuple instructions have two different syntaxes, one for simple tuple
    // types, one for complicated ones.
    if (P.Tok.isNot(tok::sil_dollar)) {
      // If there is no type, parse the simple form.
      if (P.parseToken(tok::l_paren, diag::expected_tok_in_sil_instr, "("))
        return true;
      
      // TODO: Check for a type here.  This is how tuples with "interesting"
      // types are described.
      
      // This form is used with tuples that have elements with no names or
      // default values.
      SmallVector<TupleTypeElt, 4> TypeElts;
      if (P.Tok.isNot(tok::r_paren)) {
        do {
          if (parseTypedValueRef(Val)) return true;
          OpList.push_back(Val);
          TypeElts.push_back(Val.getType().getSwiftRValueType());
        } while (P.consumeIf(tok::comma));
      }
      HadError |= P.parseToken(tok::r_paren,
                               diag::expected_tok_in_sil_instr,")");
      
      auto Ty = TupleType::get(TypeElts, P.Context);
      auto Ty2 = SILType::getPrimitiveObjectType(Ty->getCanonicalType());
      ResultVal = B.createTuple(InstLoc, Ty2, OpList);
      break;
    }
    
    // Otherwise, parse the fully general form.
    SILType Ty;
    if (parseSILType(Ty) ||
        P.parseToken(tok::l_paren, diag::expected_tok_in_sil_instr, "("))
      return true;
    
    TupleType *TT = Ty.getAs<TupleType>();
    if (TT == nullptr) {
      P.diagnose(OpcodeLoc, diag::expected_tuple_type_in_tuple);
      return true;
    }
    
    SmallVector<TupleTypeElt, 4> TypeElts;
    if (P.Tok.isNot(tok::r_paren)) {
      do {
        if (TypeElts.size() > TT->getFields().size()) {
          P.diagnose(P.Tok, diag::sil_tuple_inst_wrong_value_count,
                     TT->getFields().size());
          return true;
        }
        Type EltTy = TT->getFields()[TypeElts.size()].getType();
        if (parseValueRef(Val,
                 SILType::getPrimitiveObjectType(EltTy->getCanonicalType()),
                 SILFileLocation(P.Tok.getLoc())))
          return true;
        OpList.push_back(Val);
        TypeElts.push_back(Val.getType().getSwiftRValueType());
      } while (P.consumeIf(tok::comma));
    }
    HadError |= P.parseToken(tok::r_paren,
                             diag::expected_tok_in_sil_instr,")");

    if (TypeElts.size() != TT->getFields().size()) {
      P.diagnose(OpcodeLoc, diag::sil_tuple_inst_wrong_value_count,
                 TT->getFields().size());
      return true;
    }

    ResultVal = B.createTuple(InstLoc, Ty, OpList);
    break;
  }
  case ValueKind::EnumInst: {
    SILType Ty;
    SILDeclRef Elt;
    SILValue Operand;
    if (parseSILType(Ty) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseSILDeclRef(Elt))
      return true;
    
    if (P.Tok.is(tok::comma)) {
      P.consumeToken(tok::comma);
      if (parseTypedValueRef(Operand))
        return true;
    }
    
    ResultVal = B.createEnum(InstLoc, Operand,
                              cast<EnumElementDecl>(Elt.getDecl()), Ty);
    break;
  }
  case ValueKind::InitEnumDataAddrInst:
  case ValueKind::UncheckedEnumDataInst:
  case ValueKind::UncheckedTakeEnumDataAddrInst: {
    SILValue Operand;
    SILDeclRef EltRef;
    if (parseTypedValueRef(Operand) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseSILDeclRef(EltRef))
      return true;
    
    EnumElementDecl *Elt = cast<EnumElementDecl>(EltRef.getDecl());
    auto ResultTy = Operand.getType().getEnumElementType(Elt, SILMod);
    
    switch (Opcode) {
    case swift::ValueKind::InitEnumDataAddrInst:
      ResultVal = B.createInitEnumDataAddr(InstLoc, Operand, Elt, ResultTy);
      break;
    case swift::ValueKind::UncheckedTakeEnumDataAddrInst:
      ResultVal = B.createUncheckedTakeEnumDataAddr(InstLoc, Operand, Elt,
                                                    ResultTy);
      break;
    case swift::ValueKind::UncheckedEnumDataInst:
      ResultVal = B.createUncheckedEnumData(InstLoc, Operand, Elt, ResultTy);
      break;
    default:
      llvm_unreachable("switch out of sync");
    }
    break;
  }
  case ValueKind::InjectEnumAddrInst: {
    SILValue Operand;
    SILDeclRef EltRef;
    if (parseTypedValueRef(Operand) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseSILDeclRef(EltRef))
      return true;
    
    EnumElementDecl *Elt = cast<EnumElementDecl>(EltRef.getDecl());
    ResultVal = B.createInjectEnumAddr(InstLoc, Operand, Elt);
    break;
  }
  case ValueKind::TupleElementAddrInst:
  case ValueKind::TupleExtractInst: {
    Identifier ElemId;
    SourceLoc NameLoc;
    if (parseTypedValueRef(Val) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ","))
      return true;

    unsigned Field = 0;
    TupleType *TT = Val.getType().getAs<TupleType>();
    if (P.Tok.isNot(tok::integer_literal) ||
        P.Tok.getText().getAsInteger(10, Field) ||
        Field >= TT->getFields().size()) {
      P.diagnose(P.Tok, diag::sil_tuple_inst_wrong_field);
      return true;
    }
    P.consumeToken(tok::integer_literal);

    auto ResultTy = TT->getFields()[Field].getType()->getCanonicalType();
    if (Opcode == ValueKind::TupleElementAddrInst)
      ResultVal = B.createTupleElementAddr(InstLoc, Val, Field,
                                  SILType::getPrimitiveAddressType(ResultTy));
    else
      ResultVal = B.createTupleExtract(InstLoc, Val, Field,
                          SILType::getPrimitiveObjectType(ResultTy));
    break;
  }
  case ValueKind::ReturnInst: {
    if (parseTypedValueRef(Val)) return true;
    ResultVal = B.createReturn(InstLoc, Val);
    break;
  }
  case ValueKind::BranchInst: {
    Identifier BBName;
    SourceLoc NameLoc;
    if (parseSILIdentifier(BBName, NameLoc, diag::expected_sil_block_name))
      return true;

    SmallVector<SILValue, 6> Args;
    if (parseSILBBArgsAtBranch(Args))
      return true;

    // Note, the basic block here could be a reference to an undefined
    // basic block, which will be parsed later on.
    ResultVal = B.createBranch(InstLoc, getBBForReference(BBName, NameLoc),
                               Args);
    break;
  }
  case ValueKind::CondBranchInst: {
    UnresolvedValueName Cond;
    Identifier BBName, BBName2;
    SourceLoc NameLoc, NameLoc2;
    SmallVector<SILValue, 6> Args, Args2;
    if (parseValueName(Cond) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseSILIdentifier(BBName, NameLoc, diag::expected_sil_block_name) ||
        parseSILBBArgsAtBranch(Args) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseSILIdentifier(BBName2, NameLoc2,
                           diag::expected_sil_block_name) ||
        parseSILBBArgsAtBranch(Args2))
      return true;

    auto I1Ty =
      SILType::getBuiltinIntegerType(1, BB->getParent()->getASTContext());
    SILValue CondVal = getLocalValue(Cond, I1Ty, InstLoc);
    ResultVal = B.createCondBranch(InstLoc, CondVal,
                                   getBBForReference(BBName, NameLoc),
                                   Args,
                                   getBBForReference(BBName2, NameLoc2),
                                   Args2);
    break;
  }
  case ValueKind::UnreachableInst:
    ResultVal = B.createUnreachable(InstLoc);
    break;
    
  case ValueKind::ClassMethodInst:
  case ValueKind::SuperMethodInst:
  case ValueKind::DynamicMethodInst: {
    bool IsVolatile = false;
    if (parseSILOptional(IsVolatile, *this, "volatile"))
      return true;
    SILDeclRef Member;
    SILType MethodTy;
    SourceLoc TyLoc;
    SmallVector<ValueDecl *, 4> values;
    if (parseTypedValueRef(Val) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseSILDeclRef(Member, values) ||
        P.parseToken(tok::colon, diag::expected_tok_in_sil_instr, ":"))
      return true;

    // Parse the type for SILDeclRef.
    Optional<Scope> GenericsScope;
    GenericsScope.emplace(&P, ScopeKind::Generics);
    ParserResult<TypeRepr> TyR = P.parseType();
    GenericsScope.reset();
    if (TyR.isNull())
      return true;
    TypeLoc Ty = TyR.get();

    // The type can be polymorphic.
    ArchetypeBuilder builder(*P.SF.getParentModule(), P.Diags);
    if (auto fnType = dyn_cast<FunctionTypeRepr>(TyR.get())) {
      if (auto generics = fnType->getGenericParams()) {
        generics->setBuilder(&builder);
        TypeLoc TyLoc = TyR.get();
        handleGenericParams(TyLoc, &builder);
      }
    }

    if (performTypeLocChecking(Ty, false))
      return true;

    if (P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseSILType(MethodTy, TyLoc)
       )
      return true;

    // Pick the ValueDecl that has the right type.
    ValueDecl *TheDecl = nullptr;
    auto declTy = Ty.getType()->getCanonicalType();
    auto unlabeledDecl =
      declTy->getUnlabeledType(P.Context)->getCanonicalType();
    for (unsigned I = 0, E = values.size(); I < E; I++) {
      auto lookupTy = values[I]->getType()->getCanonicalType();
      auto unlabeledLookup =
        lookupTy->getUnlabeledType(P.Context)->getCanonicalType();
      if (checkASTType(unlabeledLookup, unlabeledDecl, P.Context, 0)) {
        TheDecl = values[I];
        // Update SILDeclRef to point to the right Decl.
        Member.loc = TheDecl;
        break;
      }
      if (values.size() == 1 && !TheDecl) {
        P.diagnose(TyLoc, diag::sil_member_decl_type_mismatch, declTy,
                   lookupTy);
        return true;
      }
    }
    if (!TheDecl) {
      P.diagnose(TyLoc, diag::sil_member_decl_not_found);
      return true;
    }

    switch (Opcode) {
    default: llvm_unreachable("Out of sync with parent switch");
    case ValueKind::ClassMethodInst:
      ResultVal = B.createClassMethod(InstLoc, Val, Member, MethodTy,
                                      IsVolatile);
      break;
    case ValueKind::SuperMethodInst:
      ResultVal = B.createSuperMethod(InstLoc, Val, Member, MethodTy,
                                      IsVolatile);
      break;
    case ValueKind::DynamicMethodInst:
      ResultVal = B.createDynamicMethod(InstLoc, Val, Member, MethodTy,
                                       IsVolatile);
      break;
    }
    break;
  }
  case ValueKind::WitnessMethodInst: {
    bool IsVolatile = false;
    if (parseSILOptional(IsVolatile, *this, "volatile"))
      return true;
    CanType LookupTy;
    SILDeclRef Member;
    SILType MethodTy;
    SourceLoc TyLoc;
    if (P.parseToken(tok::sil_dollar, diag::expected_tok_in_sil_instr, "$") ||
        parseASTType(LookupTy) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseSILDeclRef(Member))
      return true;
    // Optional operand.
    SILValue Operand;
    if (P.Tok.is(tok::comma)) {
      P.consumeToken(tok::comma);
      if (parseTypedValueRef(Operand))
        return true;
    }
    if (P.parseToken(tok::colon, diag::expected_tok_in_sil_instr, ":") ||
        parseSILType(MethodTy, TyLoc))
      return true;

    // If LookupTy is a non-archetype, look up its conformance.
    ProtocolDecl *proto
      = dyn_cast<ProtocolDecl>(Member.getDecl()->getDeclContext());
    if (!proto) {
      P.diagnose(TyLoc, diag::sil_witness_method_not_protocol);
      return true;
    }
    ProtocolConformance *Conformance = nullptr;
    if (!isa<ArchetypeType>(LookupTy)) {
      auto lookup = P.SF.getParentModule()->lookupConformance(
                                                      LookupTy, proto, nullptr);
      if (lookup.getInt() != ConformanceKind::Conforms) {
        P.diagnose(TyLoc, diag::sil_witness_method_type_does_not_conform);
        return true;
      }
      Conformance = lookup.getPointer();
    }
    
    ResultVal = B.createWitnessMethod(InstLoc, LookupTy, Conformance, Member,
                                        MethodTy, Operand, IsVolatile);
    break;
  }
  case ValueKind::CopyAddrInst: {
    bool IsTake = false, IsInit = false;
    UnresolvedValueName SrcLName;
    SILValue DestLVal;
    SourceLoc ToLoc, DestLoc;
    Identifier ToToken;
    if (parseSILOptional(IsTake, *this, "take") || parseValueName(SrcLName) ||
        parseSILIdentifier(ToToken, ToLoc,
                           diag::expected_tok_in_sil_instr, "to") ||
        parseSILOptional(IsInit, *this, "initialization") ||
        parseTypedValueRef(DestLVal, DestLoc))
      return true;

    if (ToToken.str() != "to") {
      P.diagnose(ToLoc, diag::expected_tok_in_sil_instr, "to");
      return true;
    }

    if (!DestLVal.getType().isAddress()) {
      P.diagnose(DestLoc, diag::sil_invalid_instr_operands);
      return true;
    }

    SILValue SrcLVal = getLocalValue(SrcLName, DestLVal.getType(), InstLoc);
    ResultVal = B.createCopyAddr(InstLoc, SrcLVal, DestLVal,
                                 IsTake_t(IsTake),
                                 IsInitialization_t(IsInit));
    break;
  }
  case ValueKind::StructInst: {
    SILType StructTy;
    if (parseSILType(StructTy) ||
        P.parseToken(tok::l_paren, diag::expected_tok_in_sil_instr, "("))
      return true;

    // Parse a list of SILValue.
    if (P.Tok.isNot(tok::r_paren)) {
      do {
        if (parseTypedValueRef(Val)) return true;
        OpList.push_back(Val);
      } while (P.consumeIf(tok::comma));
    }
    if (P.parseToken(tok::r_paren,
                     diag::expected_tok_in_sil_instr,")"))
      return true;

    ResultVal = B.createStruct(InstLoc, StructTy, OpList);
    break;
  }
  case ValueKind::StructElementAddrInst:
  case ValueKind::StructExtractInst: {
    ValueDecl *FieldV;
    SourceLoc NameLoc = P.Tok.getLoc();
    if (parseTypedValueRef(Val) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseSILDottedPath(FieldV))
      return true;
    if (!FieldV || !isa<VarDecl>(FieldV)) {
      P.diagnose(NameLoc, diag::sil_struct_inst_wrong_field);
      return true;
    }
    VarDecl *Field = cast<VarDecl>(FieldV);

    // FIXME: substitution means this type should be explicit to improve
    // performance.
    auto ResultTy = Val.getType().getFieldType(Field, SILMod);
    if (Opcode == ValueKind::StructElementAddrInst)
      ResultVal = B.createStructElementAddr(InstLoc, Val, Field,
                                            ResultTy.getAddressType());
    else
      ResultVal = B.createStructExtract(InstLoc, Val, Field,
                                        ResultTy.getObjectType());
    break;
  }
  case ValueKind::RefElementAddrInst: {
    ValueDecl *FieldV;
    SourceLoc NameLoc;
    if (parseTypedValueRef(Val) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseSILDottedPath(FieldV))
      return true;
    if (!FieldV || !isa<VarDecl>(FieldV)) {
      P.diagnose(NameLoc, diag::sil_ref_inst_wrong_field);
      return true;
    }
    VarDecl *Field = cast<VarDecl>(FieldV);
    auto ResultTy = Val.getType().getFieldType(Field, SILMod);
    ResultVal = B.createRefElementAddr(InstLoc, Val, Field, ResultTy);
    break;
  }
  case ValueKind::IsNonnullInst: {
    SourceLoc Loc;
    if (parseTypedValueRef(Val, Loc))
      return true;
    ResultVal = B.createIsNonnull(InstLoc, Val);
    break;
  }
  case ValueKind::IndexAddrInst: {
    SILValue IndexVal;
    if (parseTypedValueRef(Val) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseTypedValueRef(IndexVal))
      return true;
    ResultVal = B.createIndexAddr(InstLoc, Val, IndexVal);
    break;
  }
  case ValueKind::IndexRawPointerInst: {
    SILValue IndexVal;
    if (parseTypedValueRef(Val) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseTypedValueRef(IndexVal))
      return true;
    ResultVal = B.createIndexRawPointer(InstLoc, Val, IndexVal);
    break;
  }
  case ValueKind::ObjCProtocolInst: {
    Identifier ProtocolName;
    SILType Ty;
    if (P.parseToken(tok::pound, diag::expected_sil_constant) ||
        parseSILIdentifier(ProtocolName, diag::expected_sil_constant) ||
        P.parseToken(tok::colon, diag::expected_tok_in_sil_instr, ":") ||
        parseSILType(Ty))
      return true;
    // Find the decl for the protocol name.
    ValueDecl *VD;
    SmallVector<ValueDecl*, 4> CurModuleResults;
    // Perform a module level lookup on the first component of the
    // fully-qualified name.
    P.SF.getParentModule()->lookupValue(Module::AccessPathTy(), ProtocolName,
                                        NLKind::UnqualifiedLookup,
                                        CurModuleResults);
    assert(CurModuleResults.size() == 1);
    VD = CurModuleResults[0];
    ResultVal = B.createObjCProtocol(InstLoc, cast<ProtocolDecl>(VD), Ty);
    break;
  }
  case ValueKind::GlobalAddrInst: {
    Identifier GlobalName;
    SourceLoc IdLoc;
    SILType Ty;
    if (P.parseToken(tok::at_sign, diag::expected_sil_value_name) ||
        parseSILIdentifier(GlobalName, IdLoc, diag::expected_sil_value_name) ||
        P.parseToken(tok::colon, diag::expected_tok_in_sil_instr, ":") ||
        parseSILType(Ty))
      return true;

    // Go through list of global variables in the SILModule.
    SILGlobalVariable *global = SILMod.lookUpGlobalVariable(GlobalName.str());
    if (!global) {
      P.diagnose(IdLoc, diag::sil_global_variable_not_found, GlobalName);
      return true;
    }

    if (global->getLoweredType().getAddressType() != Ty) {
      P.diagnose(IdLoc, diag::sil_value_use_type_mismatch, GlobalName.str(),
                 global->getLoweredType().getSwiftRValueType(),
                 Ty.getSwiftRValueType());
      return true;
    }

    ResultVal = B.createGlobalAddr(InstLoc, global);
    break;
  }
  case ValueKind::SelectEnumInst:
  case ValueKind::SelectEnumAddrInst: {
    if (parseTypedValueRef(Val))
      return true;

    SmallVector<std::pair<EnumElementDecl*, UnresolvedValueName>, 4>
      CaseValueNames;
    Optional<UnresolvedValueName> DefaultValueName;
    while (P.consumeIf(tok::comma)) {
      Identifier BBName;
      SourceLoc BBLoc;
      // Parse 'default' sil-value.
     UnresolvedValueName tmp;
      if (P.consumeIf(tok::kw_default)) {
        if (parseValueName(tmp))
          return true;
        DefaultValueName = tmp;
        break;
      }

      // Parse 'case' sil-decl-ref ':' sil-value.
      if (P.consumeIf(tok::kw_case)) {
        SILDeclRef ElemRef;
        if (parseSILDeclRef(ElemRef))
          return true;
        assert(ElemRef.hasDecl() && isa<EnumElementDecl>(ElemRef.getDecl()));
        P.parseToken(tok::colon, diag::expected_tok_in_sil_instr, ":");
        parseValueName(tmp);
        CaseValueNames.push_back(std::make_pair(
                                     cast<EnumElementDecl>(ElemRef.getDecl()),
                                     tmp));
        continue;
      }

      P.diagnose(P.Tok, diag::expected_tok_in_sil_instr, "case or default");
      return true;
    }
    
    // Parse the type of the result operands.
    SILType ResultType;
    if (P.parseToken(tok::colon, diag::expected_tok_in_sil_instr, ":")
        || parseSILType(ResultType))
      return true;
    
    // Resolve the results.
    SmallVector<std::pair<EnumElementDecl*, SILValue>, 4> CaseValues;
    SILValue DefaultValue;
    if (DefaultValueName)
      DefaultValue = getLocalValue(*DefaultValueName, ResultType, InstLoc);
    for (auto &caseName : CaseValueNames)
      CaseValues.push_back(std::make_pair(caseName.first,
                          getLocalValue(caseName.second, ResultType, InstLoc)));
    
    if (Opcode == ValueKind::SelectEnumInst)
      ResultVal = B.createSelectEnum(InstLoc, Val, ResultType,
                                     DefaultValue, CaseValues);
    else
      ResultVal = B.createSelectEnumAddr(InstLoc, Val, ResultType,
                                         DefaultValue, CaseValues);
    break;
  }
      
  case ValueKind::SwitchEnumInst:
  case ValueKind::SwitchEnumAddrInst: {
    if (parseTypedValueRef(Val))
      return true;

    SmallVector<std::pair<EnumElementDecl*, SILBasicBlock*>, 4> CaseBBs;
    SILBasicBlock *DefaultBB = nullptr;
    while (P.consumeIf(tok::comma)) {
      Identifier BBName;
      SourceLoc BBLoc;
      // Parse 'default' sil-identifier.
      if (P.consumeIf(tok::kw_default)) {
        parseSILIdentifier(BBName, BBLoc, diag::expected_sil_block_name);
        DefaultBB = getBBForReference(BBName, BBLoc);
        break;
      }

      // Parse 'case' sil-decl-ref ':' sil-identifier.
      if (P.consumeIf(tok::kw_case)) {
        SILDeclRef ElemRef;
        if (parseSILDeclRef(ElemRef))
          return true;
        assert(ElemRef.hasDecl() && isa<EnumElementDecl>(ElemRef.getDecl()));
        P.parseToken(tok::colon, diag::expected_tok_in_sil_instr, ":");
        parseSILIdentifier(BBName, BBLoc, diag::expected_sil_block_name);
        CaseBBs.push_back( {cast<EnumElementDecl>(ElemRef.getDecl()),
                            getBBForReference(BBName, BBLoc)} );
        continue;
      }

      P.diagnose(P.Tok, diag::expected_tok_in_sil_instr, "case or default");
      return true;
    }
    if (Opcode == ValueKind::SwitchEnumInst)
      ResultVal = B.createSwitchEnum(InstLoc, Val, DefaultBB, CaseBBs);
    else
      ResultVal = B.createSwitchEnumAddr(InstLoc, Val, DefaultBB, CaseBBs);
    break;
  }
  case ValueKind::SwitchValueInst: {
    if (parseTypedValueRef(Val))
      return true;

    SmallVector<std::pair<SILValue, SILBasicBlock *>, 4> CaseBBs;
    SILBasicBlock *DefaultBB = nullptr;
    while (P.consumeIf(tok::comma)) {
      Identifier BBName;
      SourceLoc BBLoc;
      SILValue CaseVal;
      // Parse 'default' sil-identifier.
      if (P.consumeIf(tok::kw_default)) {
        parseSILIdentifier(BBName, BBLoc, diag::expected_sil_block_name);
        DefaultBB = getBBForReference(BBName, BBLoc);
        break;
      }

      // Parse 'case' value-ref ':' sil-identifier.
      if (P.consumeIf(tok::kw_case)) {
        if (parseValueRef(CaseVal, Val.getType(),
                          SILFileLocation(P.Tok.getLoc()))) {
          // TODO: Issue a proper error message here
          P.diagnose(P.Tok, diag::expected_tok_in_sil_instr, "reference to a value");
          return true;
        }
        auto intTy = Val.getType().getAs<BuiltinIntegerType>();
        auto functionTy = Val.getType().getAs<SILFunctionType>();
        if (!intTy && !functionTy) {
          P.diagnose(P.Tok, diag::sil_integer_literal_not_integer_type);
          return true;
        }

        if (intTy) {
          // If it is a switch on an integer type, check that all case values
          // are integer literals.
          auto *IL = dyn_cast<IntegerLiteralInst>(CaseVal);
          if (!IL) {
            P.diagnose(P.Tok, diag::sil_integer_literal_not_integer_type);
            return true;
          }
          APInt CaseValue = IL->getValue();

          if (CaseValue.getBitWidth() != intTy->getGreatestWidth())
            CaseVal = B.createIntegerLiteral(
                IL->getLoc(), Val.getType(),
                CaseValue.zextOrTrunc(intTy->getGreatestWidth()));
        }

        if (functionTy) {
          // If it is a switch on a function type, check that all case values
          // are function references.
          auto *FR = dyn_cast<FunctionRefInst>(CaseVal);
          if (!FR) {
            if (auto *CF = dyn_cast<ConvertFunctionInst>(CaseVal)) {
              FR = dyn_cast<FunctionRefInst>(CF->getOperand());
            }
          }
          if (!FR) {
            P.diagnose(P.Tok, diag::sil_integer_literal_not_integer_type);
            return true;
          }
        }

        P.parseToken(tok::colon, diag::expected_tok_in_sil_instr, ":");
        parseSILIdentifier(BBName, BBLoc, diag::expected_sil_block_name);
        CaseBBs.push_back({CaseVal, getBBForReference(BBName, BBLoc)});
        continue;
      }

      P.diagnose(P.Tok, diag::expected_tok_in_sil_instr, "case or default");
      return true;
    }
    ResultVal = B.createSwitchValue(InstLoc, Val, DefaultBB, CaseBBs);
    break;
  }
  case ValueKind::SelectValueInst: {
    if (parseTypedValueRef(Val))
      return true;

    SmallVector<std::pair<UnresolvedValueName, UnresolvedValueName>, 4>
      CaseValueAndResultNames;
    Optional<UnresolvedValueName> DefaultResultName;
    while (P.consumeIf(tok::comma)) {
      Identifier BBName;
      SourceLoc BBLoc;
      // Parse 'default' sil-value.
      UnresolvedValueName tmp;
      if (P.consumeIf(tok::kw_default)) {
        if (parseValueName(tmp))
          return true;
        DefaultResultName = tmp;
        break;
      }

      // Parse 'case' sil-decl-ref ':' sil-value.
      if (P.consumeIf(tok::kw_case)) {
        UnresolvedValueName casevalue;
        parseValueName(casevalue);
        P.parseToken(tok::colon, diag::expected_tok_in_sil_instr, ":");
        parseValueName(tmp);
        CaseValueAndResultNames.push_back(std::make_pair(
                                          casevalue,
                                          tmp));
        continue;
      }

      P.diagnose(P.Tok, diag::expected_tok_in_sil_instr, "case or default");
      return true;
    }

    if (!DefaultResultName) {
      P.diagnose(P.Tok, diag::expected_tok_in_sil_instr, "default");
      return true;
    }

    // Parse the type of the result operands.
    SILType ResultType;
    if (P.parseToken(tok::colon, diag::expected_tok_in_sil_instr, ":") ||
        parseSILType(ResultType))
      return true;

    // Resolve the results.
    SmallVector<std::pair<SILValue, SILValue>, 4> CaseValues;
    SILValue DefaultValue;
    if (DefaultResultName)
      DefaultValue = getLocalValue(*DefaultResultName, ResultType, InstLoc);
    SILType ValType = Val.getType();
    for (auto &caseName : CaseValueAndResultNames)
      CaseValues.push_back(std::make_pair(
                           getLocalValue(caseName.first, ValType, InstLoc),
                           getLocalValue(caseName.second, ResultType, InstLoc)));

    ResultVal = B.createSelectValue(InstLoc, Val, ResultType,
                                    DefaultValue, CaseValues);
    break;
  }
  case ValueKind::DeinitExistentialAddrInst: {
    if (parseTypedValueRef(Val))
      return true;
    ResultVal = B.createDeinitExistentialAddr(InstLoc, Val);
    break;
  }
  case ValueKind::InitExistentialAddrInst: {
    CanType Ty;
    if (parseTypedValueRef(Val) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        P.parseToken(tok::sil_dollar, diag::expected_tok_in_sil_instr, "$") ||
        parseASTType(Ty))
      return true;
    
    // Lower the type at the abstraction level of the existential.
    auto archetype
      = ArchetypeType::getOpened(Val.getType().getSwiftRValueType())
        ->getCanonicalType();
    
    SILType LoweredTy = SILMod.Types.getLoweredType(
                                    Lowering::AbstractionPattern(archetype), Ty)
      .getAddressType();
    
    // Collect conformances for the type.
    ArrayRef<ProtocolConformance *> conformances
      = collectExistentialConformances(P, Ty,
                                       Val.getType().getSwiftRValueType());
    
    ResultVal = B.createInitExistentialAddr(InstLoc, Val, Ty, LoweredTy,
                                        conformances);
    break;
  }
  case ValueKind::AllocExistentialBoxInst: {
    SILType ExistentialTy;
    CanType ConcreteFormalTy;
    
    if (parseSILType(ExistentialTy) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        P.parseToken(tok::sil_dollar, diag::expected_tok_in_sil_instr, "$") ||
        parseASTType(ConcreteFormalTy))
      return true;
    
    // Lower the type at the abstraction level of the existential.
    auto archetype
      = ArchetypeType::getOpened(ExistentialTy.getSwiftRValueType())
        ->getCanonicalType();
    
    SILType LoweredTy = SILMod.Types.getLoweredType(
                      Lowering::AbstractionPattern(archetype), ConcreteFormalTy)
      .getAddressType();
    
    // Collect conformances for the type.
    ArrayRef<ProtocolConformance *> conformances
      = collectExistentialConformances(P, ConcreteFormalTy,
                                       ExistentialTy.getSwiftRValueType());
    
    ResultVal = B.createAllocExistentialBox(InstLoc, ExistentialTy,
                                    ConcreteFormalTy, LoweredTy, conformances);
    
    break;
  }
  case ValueKind::InitExistentialRefInst: {
    CanType FormalConcreteTy;
    SILType ExistentialTy;
    if (parseTypedValueRef(Val) ||
        P.parseToken(tok::colon, diag::expected_tok_in_sil_instr, ":") ||
        P.parseToken(tok::sil_dollar, diag::expected_tok_in_sil_instr, "$") ||
        parseASTType(FormalConcreteTy) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseSILType(ExistentialTy))
      return true;
    
    ArrayRef<ProtocolConformance *> conformances
      = collectExistentialConformances(P, FormalConcreteTy,
                            ExistentialTy.getSwiftRValueType());

    // FIXME: Conformances in InitExistentialRefInst is currently not included
    // in SIL.rst.
    ResultVal = B.createInitExistentialRef(InstLoc, ExistentialTy,
                                           FormalConcreteTy, Val,
                                           conformances);
    break;
  }
  case ValueKind::InitExistentialMetatypeInst: {
    SILType ExistentialTy;
    if (parseTypedValueRef(Val) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseSILType(ExistentialTy))
      return true;
    
    // FIXME: We should be parsing conformances here.
    ResultVal = B.createInitExistentialMetatype(InstLoc, Val, ExistentialTy,
                                           ArrayRef<ProtocolConformance*>());
    break;
  }
  case ValueKind::DynamicMethodBranchInst: {
    SILDeclRef Member;
    Identifier BBName, BBName2;
    SourceLoc NameLoc, NameLoc2;
    if (parseTypedValueRef(Val) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseSILDeclRef(Member) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseSILIdentifier(BBName, NameLoc, diag::expected_sil_block_name) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseSILIdentifier(BBName2, NameLoc2,
                           diag::expected_sil_block_name))
      return true;

    ResultVal = B.createDynamicMethodBranch(InstLoc, Val, Member,
                                            getBBForReference(BBName, NameLoc),
                                            getBBForReference(BBName2,
                                                              NameLoc2));
    break;
  }
  case ValueKind::ProjectBlockStorageInst: {
    if (parseTypedValueRef(Val))
      return true;
    
    ResultVal = B.createProjectBlockStorage(InstLoc, Val);
    break;
  }
  case ValueKind::InitBlockStorageHeaderInst: {
    Identifier invoke, type;
    SourceLoc invokeLoc, typeLoc;
    
    SILValue invokeVal, invokeValLoc;
    
    SILType blockType;
    
    if (parseTypedValueRef(Val) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseSILIdentifier(invoke, invokeLoc,
                           diag::expected_tok_in_sil_instr, "invoke") ||
        parseTypedValueRef(invokeVal) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseSILIdentifier(type, typeLoc,
                           diag::expected_tok_in_sil_instr, "type") ||
        parseSILType(blockType))
      return true;
    
    if (invoke.str() != "invoke") {
      P.diagnose(invokeLoc, diag::expected_tok_in_sil_instr, "invoke");
      return true;
    }
    if (type.str() != "type") {
      P.diagnose(invokeLoc, diag::expected_tok_in_sil_instr, "type");
      return true;
    }
    
    ResultVal = B.createInitBlockStorageHeader(InstLoc, Val, invokeVal,
                                               blockType);
    break;
  }
  }
  
  // Store the named value if we had a name.
  if (ResultNameLoc.isValid())
    setLocalValue(ResultVal, ResultName, ResultNameLoc);
  return false;
}

bool SILParser::parseCallInstruction(SILLocation InstLoc,
                                     ValueKind Opcode, SILBuilder &B,
                                     ValueBase *&ResultVal) {
  UnresolvedValueName FnName;
  SmallVector<UnresolvedValueName, 4> ArgNames;

  if (parseValueName(FnName))
    return true;
  SmallVector<ParsedSubstitution, 4> parsedSubs;
  if (parseApplySubstitutions(parsedSubs))
    return true;
    
  if (P.parseToken(tok::l_paren, diag::expected_tok_in_sil_instr, "("))
    return true;

  if (P.Tok.isNot(tok::r_paren)) {
    do {
      UnresolvedValueName Arg;
      if (parseValueName(Arg)) return true;
      ArgNames.push_back(Arg);
    } while (P.consumeIf(tok::comma));
  }

  SILType Ty;
  SourceLoc TypeLoc;
  GenericParamList *GenericParams = nullptr;
  if (P.parseToken(tok::r_paren, diag::expected_tok_in_sil_instr, ")") ||
      P.parseToken(tok::colon, diag::expected_tok_in_sil_instr, ":") ||
      parseSILType(Ty, TypeLoc, GenericParams))
    return true;

  auto FTI = Ty.getAs<SILFunctionType>();
  if (!FTI) {
    P.diagnose(TypeLoc, diag::expected_sil_type_kind, "be a function");
    return true;
  }

  SmallVector<Substitution, 4> subs;
  if (!parsedSubs.empty()) {
    if (!GenericParams) {
      P.diagnose(TypeLoc, diag::sil_substitutions_on_non_polymorphic_type);
      return true;
    }
    if (getApplySubstitutionsFromParsed(*this, GenericParams, parsedSubs, subs))
      return true;
  }

  SILValue FnVal = getLocalValue(FnName, Ty, InstLoc);

  SILType FnTy = FnVal.getType();
  CanSILFunctionType substFTI = FTI;
  if (!subs.empty()) {
    auto silFnTy = FnTy.castTo<SILFunctionType>();
    substFTI
      = silFnTy->substGenericArgs(SILMod, P.SF.getParentModule(),
                                           subs);
    FnTy = SILType::getPrimitiveObjectType(substFTI);
  }
  
  auto ArgTys = substFTI->getParameterSILTypes();

  switch (Opcode) {
  default: llvm_unreachable("Unexpected case");
  case ValueKind::ApplyInst : {
    if (ArgTys.size() != ArgNames.size()) {
      P.diagnose(TypeLoc, diag::expected_sil_type_kind,
                 "to have the same number of arg names as arg types");
      return true;
    }
    
    unsigned ArgNo = 0;
    SmallVector<SILValue, 4> Args;
    for (auto &ArgName : ArgNames)
      Args.push_back(getLocalValue(ArgName, ArgTys[ArgNo++], InstLoc));
    
    ResultVal = B.createApply(InstLoc, FnVal, FnTy,
                              substFTI->getResult().getSILType(),
                              subs, Args);
    break;
  }
  case ValueKind::PartialApplyInst: {
    if (ArgTys.size() < ArgNames.size()) {
      P.diagnose(TypeLoc, diag::expected_sil_type_kind,
                 "have the right argument types");
      return true;
    }

    // Compute the result type of the partial_apply, based on which arguments
    // are getting applied.
    SmallVector<SILValue, 4> Args;
    unsigned ArgNo = ArgTys.size() - ArgNames.size();
    for (auto &ArgName : ArgNames)
      Args.push_back(getLocalValue(ArgName, ArgTys[ArgNo++], InstLoc));

    SILType closureTy =
      SILBuilder::getPartialApplyResultType(Ty, ArgNames.size(), SILMod, subs);
    // FIXME: Why the arbitrary order difference in IRBuilder type argument?
    ResultVal = B.createPartialApply(InstLoc, FnVal, FnTy,
                                     subs, Args, closureTy);
    break;
  }
  }
  return false;
}

bool SILParser::parseSILFunctionRef(SILLocation InstLoc,
                                    SILBuilder &B, ValueBase *&ResultVal) {
  Identifier Name;
  SILType Ty;
  SourceLoc Loc = P.Tok.getLoc();
  if (parseGlobalName(Name) ||
      P.parseToken(tok::colon, diag::expected_sil_colon_value_ref) ||
      parseSILType(Ty))
    return true;

  auto FnTy = Ty.getAs<SILFunctionType>();
  if (!FnTy || !Ty.isObject()) {
    P.diagnose(Loc, diag::expected_sil_function_type);
    return true;
  }
  
  ResultVal = B.createFunctionRef(InstLoc,
                                  getGlobalNameForReference(Name, FnTy, Loc));
  return false;
}

/// True if the current token sequence looks like the start of a SIL
/// instruction, either:
///   %name
/// or:
///   identifier | keyword
/// where identifier is not followed by a '(' or ':', which would indicate
/// a basic block.
bool SILParser::isStartOfSILInstruction() {
  if (P.Tok.is(tok::sil_local_name))
    return true;
  if (P.Tok.is(tok::identifier) || P.Tok.isKeyword()) {
    auto &peek = P.peekToken();
    return !peek.is(tok::l_paren) && !peek.is(tok::colon);
  }
  return false;
}

///   sil-basic-block:
///     sil-instruction+
///     identifier sil-bb-argument-list? ':' sil-instruction+
///   sil-bb-argument-list:
///     '(' sil-typed-valueref (',' sil-typed-valueref)+ ')'
bool SILParser::parseSILBasicBlock() {
  SILBasicBlock *BB;

  // The basic block name is optional.
  if (P.Tok.is(tok::sil_local_name)) {
    BB = getBBForDefinition(Identifier(), SourceLoc());
  } else {
    Identifier BBName;
    SourceLoc NameLoc;
    if (parseSILIdentifier(BBName, NameLoc, diag::expected_sil_block_name))
      return true;

    BB = getBBForDefinition(BBName, NameLoc);
    
    // If there is a basic block argument list, process it.
    if (P.consumeIf(tok::l_paren)) {
      do {
        SILType Ty;
        SourceLoc NameLoc;
        StringRef Name = P.Tok.getText();
        if (P.parseToken(tok::sil_local_name, NameLoc,
                         diag::expected_sil_value_name) ||
            P.parseToken(tok::colon, diag::expected_sil_colon_value_ref) ||
            parseSILType(Ty))
          return true;
        auto Arg = new (SILMod) SILArgument(BB, Ty);
        setLocalValue(Arg, Name, NameLoc);
      } while (P.consumeIf(tok::comma));
      
      if (P.parseToken(tok::r_paren, diag::sil_basicblock_arg_rparen))
        return true;
    }
    
    if (P.parseToken(tok::colon, diag::expected_sil_block_colon))
      return true;
  }
  
  // Make sure the block is at the end of the function so that forward
  // references don't affect block layout.
  F->getBlocks().remove(BB);
  F->getBlocks().push_back(BB);

  do {
    if (parseSILInstruction(BB))
      return true;
  } while (isStartOfSILInstruction());

  return false;
}

///   decl-sil:   [[only in SIL mode]]
///     'sil' sil-linkage '@' identifier ':' sil-type decl-sil-body
///   decl-sil-body:
///     '{' sil-basic-block+ '}'
bool Parser::parseDeclSIL() {
  // Inform the lexer that we're lexing the body of the SIL declaration.  Do
  // this before we consume the 'sil' token so that all later tokens are
  // properly handled.
  Lexer::SILBodyRAII Tmp(*L);

  consumeToken(tok::kw_sil);

  SILParser FunctionState(*this);

  Optional<SILLinkage> FnLinkage;
  Identifier FnName;
  SILType FnType;
  SourceLoc FnNameLoc;

  Scope S(this, ScopeKind::TopLevel);
  bool isTransparent = false;
  bool isFragile = false;
  bool isThunk = false;
  bool isGlobalInit = false;
  Inline_t inlineStrategy = InlineDefault;
  std::string Semantics;
  EffectsKind MRK = EffectsKind::Unspecified;
  if (parseSILLinkage(FnLinkage, *this) ||
      parseDeclSILOptional(&isTransparent, &isFragile, &isThunk, &isGlobalInit,
                           &inlineStrategy, &Semantics, &MRK, *this) ||
      parseToken(tok::at_sign, diag::expected_sil_function_name) ||
      parseIdentifier(FnName, FnNameLoc, diag::expected_sil_function_name) ||
      parseToken(tok::colon, diag::expected_sil_type))
    return true;
  {
    // Construct a Scope for the function body so TypeAliasDecl can be added to
    // the scope.
    Scope Body(this, ScopeKind::FunctionBody);
    GenericParamList *ContextParams;
    if (FunctionState.parseSILType(FnType, ContextParams, true/*IsFuncDecl*/))
      return true;
    auto SILFnType = FnType.getAs<SILFunctionType>();
    if (!SILFnType || !FnType.isObject()) {
      diagnose(FnNameLoc, diag::expected_sil_function_type);
      return true;
    }
  
    FunctionState.F =
      FunctionState.getGlobalNameForDefinition(FnName, SILFnType, FnNameLoc);
    FunctionState.F->setBare(IsBare);
    FunctionState.F->setTransparent(IsTransparent_t(isTransparent));
    FunctionState.F->setFragile(IsFragile_t(isFragile));
    FunctionState.F->setThunk(IsThunk_t(isThunk));
    FunctionState.F->setGlobalInit(isGlobalInit);
    FunctionState.F->setInlineStrategy(inlineStrategy);
    FunctionState.F->setEffectsKind(MRK);
    if (!Semantics.empty())
      FunctionState.F->setSemanticsAttr(Semantics);

    // Now that we have a SILFunction parse the body, if present.

    bool isDefinition = false;
    SourceLoc LBraceLoc = Tok.getLoc();
    if (consumeIf(tok::l_brace)) {
      isDefinition = true;
      
      // FIXME: Get the generic parameters from the function type. We'll want
      // to parse this from the TypeRepr when SILFunctionType loses its context
      // params.
      FunctionState.F->setContextGenericParams(ContextParams);
      
      // Parse the basic block list.
      do {
        if (FunctionState.parseSILBasicBlock())
          return true;
      } while (Tok.isNot(tok::r_brace) && Tok.isNot(tok::eof));

      SourceLoc RBraceLoc;
      parseMatchingToken(tok::r_brace, RBraceLoc, diag::expected_sil_rbrace,
                         LBraceLoc);
    }

    FunctionState.F->setLinkage(resolveSILLinkage(FnLinkage, isDefinition));
  }

  if (FunctionState.diagnoseProblems())
    return true;

  // If SIL prsing succeeded, verify the generated SIL.
  if (!FunctionState.P.Diags.hadAnyError())
    FunctionState.F->verify();

  // Link the static initializer for global variables.
  for (SILGlobalVariable &v : FunctionState.SILMod.getSILGlobals()) {
    if (v.getInitializer())
      if (FnName.str() == v.getInitializer()->getName())
        v.setInitializer(FunctionState.F);
  }

  return false;
}

///   decl-sil-stage:   [[only in SIL mode]]
///     'sil_stage' ('raw' | 'canonical')
bool Parser::parseDeclSILStage() {
  SourceLoc stageLoc = consumeToken(tok::kw_sil_stage);
  if (!Tok.is(tok::identifier)) {
    diagnose(Tok, diag::expected_sil_stage_name);
    return true;
  }
  SILStage stage;
  if (Tok.isContextualKeyword("raw")) {
    stage = SILStage::Raw;
    consumeToken();
  } else if (Tok.isContextualKeyword("canonical")) {
    stage = SILStage::Canonical;
    consumeToken();
  } else {
    diagnose(Tok, diag::expected_sil_stage_name);
    consumeToken();
    return true;
  }
  
  if (SIL->S->DidParseSILStage) {
    diagnose(stageLoc, diag::multiple_sil_stage_decls);
    return false;
  }
  
  SIL->M->setStage(stage);
  SIL->S->DidParseSILStage = true;
  return false;
}

/// decl-sil-global: [[only in SIL mode]]
///   'sil_global' sil-linkage @name : sil-type [external]
bool Parser::parseSILGlobal() {
  consumeToken(tok::kw_sil_global);
  Optional<SILLinkage> GlobalLinkage;
  Identifier GlobalName;
  SILType GlobalType;
  SourceLoc NameLoc;
  bool isFragile = false;

  // Inform the lexer that we're lexing the body of the SIL declaration.
  Lexer::SILBodyRAII Tmp(*L);
  Scope S(this, ScopeKind::TopLevel);
  if (parseSILLinkage(GlobalLinkage, *this) ||
      parseDeclSILOptional(nullptr, &isFragile, nullptr, nullptr,
                           nullptr, nullptr, nullptr, *this) ||
      parseToken(tok::at_sign, diag::expected_sil_value_name) ||
      parseIdentifier(GlobalName, NameLoc, diag::expected_sil_value_name) ||
      parseToken(tok::colon, diag::expected_sil_type))
    return true;

  SILParser State(*this);
  if (State.parseSILType(GlobalType))
    return true;

  // Non-external global variables are definitions by default.
  if (!GlobalLinkage.hasValue())
    GlobalLinkage = SILLinkage::DefaultForDefinition;

  // FIXME: check for existing global variable?
  auto *GV = SILGlobalVariable::create(*SIL->M, GlobalLinkage.getValue(),
                                       (IsFragile_t)isFragile,
                                       GlobalName.str(),GlobalType,
                                       SILFileLocation(NameLoc));

  // Parse static initializer if exists.
  if (State.P.consumeIf(tok::comma)) {
    Identifier Name;
    SILType Ty;
    SourceLoc Loc = State.P.Tok.getLoc();
    if (State.parseGlobalName(Name) ||
        State.P.parseToken(tok::colon, diag::expected_sil_colon_value_ref) ||
        State.parseSILType(Ty))
      return true;

    auto FnTy = Ty.getAs<SILFunctionType>();
    if (!FnTy || !Ty.isObject()) {
      State.P.diagnose(Loc, diag::expected_sil_function_type);
      return true;
    }

    GV->setInitializer(State.getGlobalNameForReference(Name, FnTy, Loc));
  }
  return false;
}

/// decl-sil-vtable: [[only in SIL mode]]
///   'sil_vtable' ClassName decl-sil-vtable-body
/// decl-sil-vtable-body:
///   '{' sil-vtable-entry* '}'
/// sil-vtable-entry:
///   SILDeclRef ':' SILFunctionName
bool Parser::parseSILVTable() {
  consumeToken(tok::kw_sil_vtable);
  SILParser VTableState(*this);

  // Parse the class name.
  Identifier Name;
  SourceLoc Loc;
  if (VTableState.parseSILIdentifier(Name, Loc,
                                     diag::expected_sil_value_name))
    return true;

  // Find the class decl.
  llvm::PointerUnion<ValueDecl*, Module *> Res = lookupTopDecl(*this, Name);
  assert(Res.is<ValueDecl*>() && "Class look-up should return a Decl");
  ValueDecl *VD = Res.get<ValueDecl*>();
  if (!VD) {
    diagnose(Loc, diag::sil_vtable_class_not_found, Name);
    return true;
  }

  ClassDecl *theClass = dyn_cast<ClassDecl>(VD);
  if (!theClass) {
    diagnose(Loc, diag::sil_vtable_class_not_found, Name);
    return true;
  }

  SourceLoc LBraceLoc = Tok.getLoc();
  consumeToken(tok::l_brace);

  // We need to turn on InSILBody to parse SILDeclRef.
  Lexer::SILBodyRAII Tmp(*L);
  Scope S(this, ScopeKind::TopLevel);
  // Parse the entry list.
  std::vector<SILVTable::Pair> vtableEntries;
  if (Tok.isNot(tok::r_brace)) {
    do {
      SILDeclRef Ref;
      Identifier FuncName;
      SourceLoc FuncLoc;
      if (VTableState.parseSILDeclRef(Ref))
        return true;
      SILFunction *Func = nullptr;
      if (Tok.is(tok::kw_nil)) {
        consumeToken();
      } else {
        if (parseToken(tok::colon, diag::expected_sil_vtable_colon) ||
          VTableState.parseSILIdentifier(FuncName, FuncLoc,
                                         diag::expected_sil_value_name))
        return true;
        Func = SIL->M->lookUpFunction(FuncName.str());
        if (!Func) {
          diagnose(FuncLoc, diag::sil_vtable_func_not_found, FuncName);
          return true;
        }
      }
      vtableEntries.emplace_back(Ref, Func);
    } while (Tok.isNot(tok::r_brace) && Tok.isNot(tok::eof));
  }

  SourceLoc RBraceLoc;
  parseMatchingToken(tok::r_brace, RBraceLoc, diag::expected_sil_rbrace,
                     LBraceLoc);

  SILVTable::create(*SIL->M, theClass, vtableEntries);
  return false;
}

static ProtocolDecl *parseProtocolDecl(Parser &P, SILParser &SP) {
  Identifier DeclName;
  SourceLoc DeclLoc;
  if (SP.parseSILIdentifier(DeclName, DeclLoc, diag::expected_sil_value_name))
    return nullptr;

  // Find the protocol decl. The protocol can be imported.
  llvm::PointerUnion<ValueDecl*, Module *> Res = lookupTopDecl(P, DeclName);
  assert(Res.is<ValueDecl*>() && "Protocol look-up should return a Decl");
  ValueDecl *VD = Res.get<ValueDecl*>();
  if (!VD) {
    P.diagnose(DeclLoc, diag::sil_witness_protocol_not_found, DeclName);
    return nullptr;
  }
  ProtocolDecl *proto = dyn_cast<ProtocolDecl>(VD);
  if (!proto)
    P.diagnose(DeclLoc, diag::sil_witness_protocol_not_found, DeclName);
  return proto;
}

static AssociatedTypeDecl *parseAssociatedTypeDecl(Parser &P, SILParser &SP,
           ProtocolDecl *proto) {
  Identifier DeclName;
  SourceLoc DeclLoc;
  if (SP.parseSILIdentifier(DeclName, DeclLoc, diag::expected_sil_value_name))
    return nullptr;
  // We can return multiple decls, for now, we use the first lookup result.
  // One example is two decls when searching for Generator of SequenceType:
  // one from SequenceType, the other from _Sequence_Type.
  SmallVector<ValueDecl *, 4> values;
  auto VD = lookupMember(P, proto->getType(), DeclName, DeclLoc,
                         values, true/*ExpectMultipleResults*/);
  if (!VD) {
    P.diagnose(DeclLoc, diag::sil_witness_assoc_not_found, DeclName);
    return nullptr;
  }
  return dyn_cast<AssociatedTypeDecl>(VD);
}

static NormalProtocolConformance *parseNormalProtocolConformance(Parser &P,
           SILParser &SP, Type ConformingTy, ProtocolDecl *&proto) {
  Identifier ModuleKeyword, ModuleName;
  SourceLoc Loc, KeywordLoc;
  proto = parseProtocolDecl(P, SP);
  if (!proto)
    return nullptr;
      
  if (P.parseIdentifier(ModuleKeyword, KeywordLoc,
                        diag::expected_tok_in_sil_instr, "module") ||
      SP.parseSILIdentifier(ModuleName, Loc,
                            diag::expected_sil_value_name))
    return nullptr;

  if (ModuleKeyword.str() != "module") {
    P.diagnose(KeywordLoc, diag::expected_tok_in_sil_instr, "module");
    return nullptr;
  }

  // FIXME: we currently emit _CocoaArrayType: _CocoaArrayType.
  if (ConformingTy->is<ProtocolType>() &&
      ConformingTy->getAs<ProtocolType>()->getDecl() == proto)
    return nullptr;

  // Calling lookupConformance on a BoundGenericType will return a specialized
  // conformance. We use UnboundGenericType to find the normal conformance.
  Type lookupTy = ConformingTy;
  if (auto bound = dyn_cast<BoundGenericType>(lookupTy.getPointer()))
    lookupTy = UnboundGenericType::get(bound->getDecl(), bound->getParent(),
                                       P.Context);
  auto lookup = P.SF.getParentModule()->lookupConformance(
                         lookupTy, proto, nullptr);
  if (!lookup.getPointer()) {
    P.diagnose(KeywordLoc, diag::sil_witness_protocol_conformance_not_found);
    return nullptr;
  }
  NormalProtocolConformance *theConformance =
      dyn_cast<NormalProtocolConformance>(lookup.getPointer());
  if (!theConformance) {
    P.diagnose(KeywordLoc, diag::sil_witness_protocol_conformance_not_found);
    return nullptr;
  }
  return theConformance;
}

/// Parse the substitution list for a specialized conformance.
bool SILParser::parseSpecConformanceSubstitutions(
                              SmallVectorImpl<ParsedSubstitution> &parsed) {
  // Check for an opening '<' bracket.
  if (!P.Tok.isContextualPunctuator("<"))
    return false;
 
  P.consumeToken();
 
  // Parse a list of Substitutions: Archetype = Replacement.
  do {
    SourceLoc Loc = P.Tok.getLoc();
    Substitution Sub;
    Identifier ArcheId;
    std::string ArcheStr;
    bool FirstToken = true;

    // It is possible to have Base.Element as the Archetype name.
    do {
      Identifier TmpId;
      if (parseSILIdentifier(TmpId, diag::expected_sil_type))
        return true;
      if (!FirstToken)
        ArcheStr += ".";
      ArcheStr += TmpId.str();
      FirstToken = false;
    } while (P.consumeIf(tok::period));
    ArcheId = P.Context.getIdentifier(ArcheStr);

    if (P.parseToken(tok::equal, diag::expected_tok_in_sil_instr, "="))
      return true;

    // Parse substitution as AST type.
    ParserResult<TypeRepr> TyR = P.parseType();
    if (TyR.isNull())
      return true;
    TypeLoc Ty = TyR.get();
    if (performTypeLocChecking(Ty, false))
      return true;
    parsed.push_back({Loc, ArcheId, Ty.getType()});
  } while (P.consumeIf(tok::comma));

  // Consume the closing '>'.
  if (!P.Tok.isContextualPunctuator(">")) {
    P.diagnose(P.Tok, diag::expected_tok_in_sil_instr, ">");
    return true;
  }
  P.consumeToken();
  return false;
}

/// Reconstruct AST substitutions from parsed substitutions using archetypes
/// from a BoundGenericType.
static bool getSpecConformanceSubstitutionsFromParsed(
                             Parser &P,
                             GenericParamList *gp,
                             ArrayRef<ParsedSubstitution> parses,
                             SmallVectorImpl<Substitution> &subs) {
  for (auto &parsed : parses) {
    ArchetypeType *subArchetype = nullptr;
    Type subReplacement;
    // Find the corresponding ArchetypeType.
    for (auto archetype : gp->getAllNestedArchetypes())
      if (archetype->getFullName() == parsed.name.str()) {
        subArchetype = archetype;
        break;
      }
    if (!subArchetype) {
      P.diagnose(parsed.loc, diag::sil_witness_archetype_not_found);
      return true;
    }
    subReplacement = parsed.replacement;
    SmallVector<ProtocolConformance*, 2> conformances;
    if (getConformancesForSubstitution(P, gp, subArchetype, subReplacement,
                                       parsed.loc, conformances))
      return true;

    subs.push_back({subArchetype, subReplacement,
                    P.Context.AllocateCopy(conformances)});
  }
  return false;
}

ProtocolConformance *SILParser::parseProtocolConformance(
           ProtocolDecl *&proto, GenericParamList *&generics,
           ArchetypeBuilder &builder, bool localScope) {
  // Parse generic params for the protocol conformance. We need to make sure
  // they have the right scope.
  Optional<Scope> GenericsScope;
  if (localScope)
    GenericsScope.emplace(&P, ScopeKind::Generics);

  generics = P.maybeParseGenericParams();
  if (generics) {
    generics->setBuilder(&builder);
    SmallVector<ArchetypeBuilder *, 1> builders(1, &builder);
    SmallVector<GenericParamList *, 1> gps(1, generics);
    handleSILGenericParams(P.Context, gps, &P.SF, builders);
  }

  ProtocolConformance *retVal = parseProtocolConformanceHelper(proto,
                                                               localScope);

  if (localScope) {
    GenericsScope.reset();
    if (generics)
      generics->setBuilder(nullptr);
  }
  return retVal;
}

///  protocol-conformance ::= normal-protocol-conformance
///  protocol-conformance ::=
///    generic-parameter-list? type: 'inherit' '(' protocol-conformance ')'
///  protocol-conformance ::=
///    generic-parameter-list? type: 'specialize' '<' substitution* '>'
///    '(' protocol-conformance ')'
///  normal-protocol-conformance ::=
///    generic-parameter-list? type: protocolName module ModuleName
/// Note that generic-parameter-list is already parsed before calling this.
ProtocolConformance *SILParser::parseProtocolConformanceHelper(
                                    ProtocolDecl *&proto,
                                    bool localScope) {
  // Parse AST type.
  ParserResult<TypeRepr> TyR = P.parseType();
  if (TyR.isNull())
    return nullptr;
  TypeLoc Ty = TyR.get();
  if (performTypeLocChecking(Ty, false))
    return nullptr;
  auto ConformingTy = Ty.getType();

  if (P.parseToken(tok::colon, diag::expected_sil_witness_colon))
    return nullptr;

  if (P.Tok.is(tok::identifier) && P.Tok.getText() == "specialize") {
    P.consumeToken();

    // Parse substitutions for specialized conformance.
    SmallVector<ParsedSubstitution, 4> parsedSubs;
    if (parseSpecConformanceSubstitutions(parsedSubs))
      return nullptr;

    if (P.parseToken(tok::l_paren, diag::expected_sil_witness_lparen))
      return nullptr;
    ProtocolDecl *dummy;
    ArchetypeBuilder genericBuilder(*P.SF.getParentModule(), P.Diags);
    GenericParamList *gp;
    auto genericConform = parseProtocolConformance(dummy, gp,
                                                   genericBuilder, localScope);
    if (!genericConform)
      return nullptr;
    if (P.parseToken(tok::r_paren, diag::expected_sil_witness_rparen))
      return nullptr;

    SmallVector<Substitution, 4> subs;
    if (getSpecConformanceSubstitutionsFromParsed(P, gp, parsedSubs, subs))
      return nullptr;

    auto result = P.Context.getSpecializedConformance(
      ConformingTy, genericConform, subs);
    return result;
  }

  if (P.Tok.is(tok::identifier) && P.Tok.getText() == "inherit") {
    P.consumeToken();

    if (P.parseToken(tok::l_paren, diag::expected_sil_witness_lparen))
      return nullptr;
    ArchetypeBuilder baseBuilder(*P.SF.getParentModule(), P.Diags);
    auto baseConform = parseProtocolConformance(baseBuilder);
    if (!baseConform)
      return nullptr;
    if (P.parseToken(tok::r_paren, diag::expected_sil_witness_rparen))
      return nullptr;

    return P.Context.getInheritedConformance(ConformingTy, baseConform);
  }

  auto retVal = parseNormalProtocolConformance(P, *this, ConformingTy, proto);
  return retVal;
}

/// decl-sil-witness ::= 'sil_witness_table' sil-linkage?
///                      normal-protocol-conformance decl-sil-witness-body
/// normal-protocol-conformance ::=
///   generic-parameter-list? type: protocolName module ModuleName
/// decl-sil-witness-body:
///   '{' sil-witness-entry* '}'
/// sil-witness-entry:
///   method SILDeclRef ':' @SILFunctionName
///   associated_type AssociatedTypeDeclName: Type
///   associated_type_protocol (AssocName: ProtocolName):
///                              protocol-conformance|dependent
///   base_protocol ProtocolName: protocol-conformance
bool Parser::parseSILWitnessTable() {
  consumeToken(tok::kw_sil_witness_table);
  SILParser WitnessState(*this);
  
  // Parse the linkage.
  Optional<SILLinkage> Linkage;
  parseSILLinkage(Linkage, *this);
  
  bool isFragile = false;
  if (parseDeclSILOptional(nullptr, &isFragile, nullptr, nullptr,
                           nullptr, nullptr, nullptr, *this))
    return true;

  Scope S(this, ScopeKind::TopLevel);
  // We should use WitnessTableBody. This ensures that the generic params
  // are visible.
  Optional<Scope> BodyScope;
  BodyScope.emplace(this, ScopeKind::FunctionBody);

  // Parse the protocol conformance.
  ProtocolDecl *proto;
  GenericParamList *dummy;
  ArchetypeBuilder builder(*SF.getParentModule(), Diags);
  auto conf = WitnessState.parseProtocolConformance(proto, dummy, builder,
                                                    false/*localScope*/);

  NormalProtocolConformance *theConformance = conf ?
      dyn_cast<NormalProtocolConformance>(conf) : nullptr;

  SILWitnessTable *wt = nullptr;
  if (theConformance) {
    wt = SIL->M->lookUpWitnessTable(theConformance, false).first;
    assert((!wt || wt->isDeclaration()) &&
           "Attempting to create duplicate witness table.");
  }

  // If we don't have an lbrace, then this witness table is a declaration.
  if (Tok.getKind() != tok::l_brace) {
    // Default to public external linkage.
    if (!Linkage)
      Linkage = SILLinkage::PublicExternal;
    // We ignore empty witness table without normal protocol conformance.
    if (!wt && theConformance)
      wt = SILWitnessTable::create(*SIL->M, *Linkage, theConformance);
    BodyScope.reset();
    return false;
  }

  if (!theConformance) {
    diagnose(Tok, diag::sil_witness_protocol_conformance_not_found);
    return true;
  }

  SourceLoc LBraceLoc = Tok.getLoc();
  consumeToken(tok::l_brace);

  // We need to turn on InSILBody to parse SILDeclRef.
  Lexer::SILBodyRAII Tmp(*L);
  // Parse the entry list.
  std::vector<SILWitnessTable::Entry> witnessEntries;
  if (Tok.isNot(tok::r_brace)) {
    do {
      Identifier EntryKeyword;
      SourceLoc KeywordLoc;
      if (parseIdentifier(EntryKeyword, KeywordLoc,
            diag::expected_tok_in_sil_instr,
            "method, associated_type, associated_type_protocol, base_protocol"))
        return true;

      if (EntryKeyword.str() == "base_protocol") {
        ProtocolDecl *proto = parseProtocolDecl(*this, WitnessState);
        if (!proto)
          return true;
        if (parseToken(tok::colon, diag::expected_sil_witness_colon))
          return true;
        ArchetypeBuilder builder(*SF.getParentModule(), Diags);
        ProtocolConformance *conform = WitnessState.parseProtocolConformance(
                                           builder);
        if (!conform) // Ignore this witness entry for now.
          continue;

        witnessEntries.push_back(SILWitnessTable::BaseProtocolWitness{
          proto, conform
        });
        continue;
      }

      if (EntryKeyword.str() == "associated_type_protocol") {
        if (parseToken(tok::l_paren, diag::expected_sil_witness_lparen))
          return true;
        AssociatedTypeDecl *assoc = parseAssociatedTypeDecl(*this,
                                        WitnessState, proto);
        if (!assoc)
          return true;
        if (parseToken(tok::colon, diag::expected_sil_witness_colon))
          return true;
        ProtocolDecl *proto = parseProtocolDecl(*this, WitnessState);
        if (!proto)
          return true;
        if (parseToken(tok::r_paren, diag::expected_sil_witness_rparen) ||
            parseToken(tok::colon, diag::expected_sil_witness_colon))
          return true;

        ProtocolConformance *conform = nullptr;
        if (Tok.getText() != "dependent") {
          ArchetypeBuilder builder(*SF.getParentModule(), Diags);
          conform = WitnessState.parseProtocolConformance(builder);
          if (!conform) // Ignore this witness entry for now.
            continue;
        } else {
          consumeToken();
        }

        witnessEntries.push_back(SILWitnessTable::AssociatedTypeProtocolWitness{
          assoc, proto, conform
        });
        continue;
      }

      if (EntryKeyword.str() == "associated_type") {
        AssociatedTypeDecl *assoc = parseAssociatedTypeDecl(*this,
                                        WitnessState, proto);
        if (!assoc)
          return true;
        if (parseToken(tok::colon, diag::expected_sil_witness_colon))
          return true;

        // Parse AST type.
        ParserResult<TypeRepr> TyR = parseType();
        if (TyR.isNull())
          return true;
        TypeLoc Ty = TyR.get();
        if (swift::performTypeLocChecking(Context, Ty, false, &SF))
          return true;

        witnessEntries.push_back(SILWitnessTable::AssociatedTypeWitness{
          assoc, Ty.getType()->getCanonicalType()
        });
        continue;
      }

      if (EntryKeyword.str() != "method") {
        diagnose(KeywordLoc, diag::expected_tok_in_sil_instr, "method");
        return true;
      }

      SILDeclRef Ref;
      Identifier FuncName;
      SourceLoc FuncLoc;
      if (WitnessState.parseSILDeclRef(Ref) ||
          parseToken(tok::colon, diag::expected_sil_witness_colon))
        return true;
      
      SILFunction *Func = nullptr;
      if (Tok.is(tok::kw_nil)) {
        consumeToken();
      } else {
        if (parseToken(tok::at_sign, diag::expected_sil_function_name) ||
            WitnessState.parseSILIdentifier(FuncName, FuncLoc,
                                        diag::expected_sil_value_name))
          return true;

        Func = SIL->M->lookUpFunction(FuncName.str());
        if (!Func) {
          diagnose(FuncLoc, diag::sil_witness_func_not_found, FuncName);
          return true;
        }
      }
      witnessEntries.push_back(SILWitnessTable::MethodWitness{
        Ref, Func
      });
    } while (Tok.isNot(tok::r_brace) && Tok.isNot(tok::eof));
  }

  SourceLoc RBraceLoc;
  parseMatchingToken(tok::r_brace, RBraceLoc, diag::expected_sil_rbrace,
                     LBraceLoc);
  
  // Default to public linkage.
  if (!Linkage)
    Linkage = SILLinkage::Public;

  if (!wt)
    wt = SILWitnessTable::create(*SIL->M, *Linkage, theConformance);
  wt->convertToDefinition(witnessEntries, isFragile);
  BodyScope.reset();
  return false;
}

llvm::Optional<llvm::coverage::Counter> SILParser::parseSILCoverageExpr(
    llvm::coverage::CounterExpressionBuilder &Builder) {
  if (P.Tok.is(tok::integer_literal)) {
    unsigned CounterId;
    if (parseInteger(CounterId, diag::sil_coverage_invalid_counter))
      return None;
    return llvm::coverage::Counter::getCounter(CounterId);
  }

  if (P.Tok.is(tok::identifier)) {
    Identifier Zero;
    SourceLoc Loc;
    if (parseSILIdentifier(Zero, Loc, diag::sil_coverage_invalid_counter))
      return None;
    if (Zero.str() != "zero") {
      P.diagnose(Loc, diag::sil_coverage_invalid_counter);
      return None;
    }
    return llvm::coverage::Counter::getZero();
  }

  if (P.Tok.is(tok::l_paren)) {
    P.consumeToken(tok::l_paren);
    auto LHS = parseSILCoverageExpr(Builder);
    if (!LHS)
      return None;
    Identifier Operator;
    SourceLoc Loc;
    if (P.parseAnyIdentifier(Operator, Loc,
                             diag::sil_coverage_invalid_operator))
      return None;
    if (Operator.str() != "+" && Operator.str() != "-") {
      P.diagnose(Loc, diag::sil_coverage_invalid_operator);
      return None;
    }
    auto RHS = parseSILCoverageExpr(Builder);
    if (!RHS)
      return None;
    if (P.parseToken(tok::r_paren, diag::sil_coverage_expected_rparen))
      return None;

    if (Operator.str() == "+")
      return Builder.add(*LHS, *RHS);
    return Builder.subtract(*LHS, *RHS);
  }

  P.diagnose(P.Tok, diag::sil_coverage_invalid_counter);
  return None;
}

/// decl-sil-coverage-map ::= 'sil_coverage_map' CoveredName CoverageHash
///                           decl-sil-coverage-body
/// decl-sil-coverage-body:
///   '{' sil-coverage-entry* '}'
/// sil-coverage-entry:
///   sil-coverage-loc ':' sil-coverage-expr
/// sil-coverage-loc:
///   StartLine ':' StartCol '->' EndLine ':' EndCol
/// sil-coverage-expr:
///   ...
bool Parser::parseSILCoverageMap() {
  consumeToken(tok::kw_sil_coverage_map);
  SILParser State(*this);

  // Parse the filename.
  Identifier Filename;
  SourceLoc FileLoc;
  if (State.parseSILIdentifier(Filename, FileLoc,
                               diag::expected_sil_value_name))
    return true;

  // Parse the covered name.
  Identifier FuncName;
  SourceLoc FuncLoc;
  if (State.parseSILIdentifier(FuncName, FuncLoc,
                               diag::expected_sil_value_name))
    return true;

  SILFunction *Func = SIL->M->lookUpFunction(FuncName.str());
  if (!Func) {
    diagnose(FuncLoc, diag::sil_coverage_func_not_found, FuncName);
    return true;
  }

  uint64_t Hash;
  if (State.parseInteger(Hash, diag::sil_coverage_invalid_hash))
    return true;

  if (!Tok.is(tok::l_brace)) {
    diagnose(Tok, diag::sil_coverage_expected_lbrace);
    return true;
  }
  SourceLoc LBraceLoc = Tok.getLoc();
  consumeToken(tok::l_brace);

  llvm::coverage::CounterExpressionBuilder Builder;
  std::vector<SILCoverageMap::MappedRegion> Regions;
  bool BodyHasError = false;
  if (Tok.isNot(tok::r_brace)) {
    do {
      unsigned StartLine, StartCol, EndLine, EndCol;
      if (State.parseInteger(StartLine, diag::sil_coverage_expected_loc) ||
          parseToken(tok::colon, diag::sil_coverage_expected_loc) ||
          State.parseInteger(StartCol, diag::sil_coverage_expected_loc) ||
          parseToken(tok::arrow, diag::sil_coverage_expected_arrow) ||
          State.parseInteger(EndLine, diag::sil_coverage_expected_loc) ||
          parseToken(tok::colon, diag::sil_coverage_expected_loc) ||
          State.parseInteger(EndCol, diag::sil_coverage_expected_loc)) {
        BodyHasError = true;
        break;
      }

      if (parseToken(tok::colon, diag::sil_coverage_expected_colon)) {
        BodyHasError = true;
        break;
      }

      auto Counter = State.parseSILCoverageExpr(Builder);
      if (!Counter) {
        BodyHasError = true;
        break;
      }

      Regions.emplace_back(StartLine, StartCol, EndLine, EndCol, *Counter);
    } while (Tok.isNot(tok::r_brace) && Tok.isNot(tok::eof));
  }
  if (BodyHasError)
    skipUntilDeclRBrace();

  SourceLoc RBraceLoc;
  parseMatchingToken(tok::r_brace, RBraceLoc, diag::expected_sil_rbrace,
                     LBraceLoc);

  if (!BodyHasError)
    SILCoverageMap::create(*SIL->M, Filename.str(), FuncName.str(), Hash,
                           Regions, Builder.getExpressions());
  return false;
}
