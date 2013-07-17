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

#include "swift/AST/ArchetypeBuilder.h"
#include "swift/Parse/Parser.h"
#include "swift/Parse/Lexer.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILModule.h"
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
    
    /// ForwardRefFns - This is all of the forward referenced functions with
    /// the location for where the reference is.
    llvm::DenseMap<Identifier,
                   std::pair<SILFunction*, SourceLoc>> ForwardRefFns;
    
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

    /// Construct ArchetypeType from Generic Params.
    bool handleGenericParams(GenericParamList *GenericParams);
    bool performTypeLocChecking(TypeLoc &T);
  public:

    SILParser(Parser &P) : P(P), SILMod(*P.SIL->M), TUState(*P.SIL->S) {}

    /// diagnoseProblems - After a function is fully parse, emit any diagnostics
    /// for errors and return true if there were any.
    bool diagnoseProblems();

    /// getGlobalNameForReference - Given a reference to a global name, look it
    /// up and return an appropriate SIL function.
    SILFunction *getGlobalNameForReference(Identifier Name, SILType Ty,
                                           SourceLoc Loc);
    /// getGlobalNameForDefinition - Given a definition of a global name, look
    /// it up and return an appropriate SIL function.
    SILFunction *getGlobalNameForDefinition(Identifier Name, SILType Ty,
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

      bool isMRV() const { return ResultVal != ~0U; }
    };

    /// getLocalValue - Get a reference to a local value with the specified name
    /// and type.
    SILValue getLocalValue(UnresolvedValueName Name, SILType Type);

    /// setLocalValue - When an instruction or block argument is defined, this
    /// method is used to register it and update our symbol table.
    void setLocalValue(ValueBase *Value, StringRef Name, SourceLoc NameLoc);
    
  public:
    // Parsing logic.
    bool parseSILType(SILType &Result);
    bool parseSILType(SILType &Result, SourceLoc &TypeLoc) {
      TypeLoc = P.Tok.getLoc();
      return parseSILType(Result);
    }

    bool parseSILConstant(SILConstant &Result);
    bool parseGlobalName(Identifier &Name);
    bool parseValueName(UnresolvedValueName &Name);
    bool parseValueRef(SILValue &Result, SILType Ty);
    bool parseTypedValueRef(SILValue &Result, SourceLoc &Loc);
    bool parseTypedValueRef(SILValue &Result) {
      SourceLoc Tmp;
      return parseTypedValueRef(Result, Tmp);
    }
    bool parseSILOpcode(ValueKind &Opcode, SourceLoc &OpcodeLoc,
                        StringRef &OpcodeName);
    bool parseSILInstruction(SILBasicBlock *BB);
    bool parseCallInstruction(ValueKind Opcode, SILBuilder &B,
                              ValueBase *&ResultVal);
    bool parseSILFunctionRef(SILBuilder &B, ValueBase *&ResultVal);

    bool parseSILBasicBlock();
  };
} // end anonymous namespace.

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
SILFunction *SILParser::getGlobalNameForDefinition(Identifier Name, SILType Ty,
                                                   SourceLoc Loc) {
  // Check to see if a function of this name has been forward referenced.  If so
  // complete the forward reference.
  auto It = TUState.ForwardRefFns.find(Name);
  if (It != TUState.ForwardRefFns.end()) {
    SILFunction *Fn = It->second.first;
    
    // Verify that the types match up.
    if (Fn->getLoweredType() != Ty) {
      P.diagnose(Loc, diag::sil_value_use_type_mismatch,
                 Ty.getAsString(), Fn->getLoweredType().getAsString());
      P.diagnose(It->second.second, diag::sil_prior_reference);
      Fn = new (SILMod) SILFunction(SILMod, SILLinkage::Internal, "", Ty);
    }
    
    assert(Fn->isExternalDeclaration() && "Forward defns cannot have bodies!");
    TUState.ForwardRefFns.erase(It);
    return Fn;
  }
  
  // If we don't have a forward reference, make sure the function hasn't been
  // defined already.
  if (SILMod.lookup(Name.str()) != nullptr) {
    P.diagnose(Loc, diag::sil_value_redefinition, Name.str());
    return new (SILMod) SILFunction(SILMod, SILLinkage::Internal, "", Ty);
  }

  // Otherwise, this definition is the first use of this name.
  return new (SILMod) SILFunction(SILMod, SILLinkage::Internal, Name.str(), Ty);
}



/// getGlobalNameForReference - Given a reference to a global name, look it
/// up and return an appropriate SIL function.
SILFunction *SILParser::getGlobalNameForReference(Identifier Name, SILType Ty,
                                                  SourceLoc Loc) {
  
  // Check to see if we have a function by this name already.
  if (SILFunction *FnRef = SILMod.lookup(Name.str())) {
    // If so, check for matching types.
    if (FnRef->getLoweredType() != Ty) {
      P.diagnose(Loc, diag::sil_value_use_type_mismatch,
                 Ty.getAsString(), FnRef->getLoweredType().getAsString());
      FnRef = new (SILMod) SILFunction(SILMod, SILLinkage::Internal, "", Ty);
    }
    return FnRef;
  }
  
  // If we didn't find a function, create a new one - it must be a forward
  // reference.
  auto Fn = new (SILMod) SILFunction(SILMod, SILLinkage::Internal,
                                     Name.str(), Ty);
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
  return P.parseToken(tok::sil_at_sign, diag::expected_sil_value_name) ||
         P.parseIdentifier(Name, diag::expected_sil_value_name);
}

/// getLocalValue - Get a reference to a local value with the specified name
/// and type.
SILValue SILParser::getLocalValue(UnresolvedValueName Name, SILType Type) {
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
        return new (SILMod) GlobalAddrInst(SILLocation(), nullptr, Type);
      }
    }

    EntryTy = Entry->getType(Name.isMRV() ? Name.ResultVal : 0);

    if (EntryTy != Type) {
      HadError = true;
      P.diagnose(Name.NameLoc, diag::sil_value_use_type_mismatch, Name.Name,
                 EntryTy.getAsString());
      // Make sure to return something of the requested type.
      return new (SILMod) GlobalAddrInst(SILLocation(), nullptr, Type);
    }

    return SILValue(Entry, Name.isMRV() ? Name.ResultVal : 0);
  }
  
  // Otherwise, this is a forward reference.  Create a dummy node to represent
  // it until we see a real definition.
  ForwardRefLocalValues[Name.Name] = Name.NameLoc;

  if (Name.ResultVal == ~0U) {
    Entry = new (SILMod) GlobalAddrInst(SILLocation(), nullptr, Type);
    return Entry;
  }

  // If we have multiple results, track them through ForwardMRVLocalValues.
  std::vector<SILValue> &Placeholders = ForwardMRVLocalValues[Name.Name];
  if (Placeholders.size() <= Name.ResultVal)
    Placeholders.resize(Name.ResultVal+1);

  if (!Placeholders[Name.ResultVal])
    Placeholders[Name.ResultVal] =
      new (SILMod) GlobalAddrInst(SILLocation(), nullptr, Type);
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
      P.diagnose(NameLoc, diag::sil_value_def_type_mismatch, Name,
                 Entry->getType(0).getAsString());
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
                   Entry->getType(0).getAsString());
        HadError = true;
        return;
      }

      // Validate that any forward-referenced elements have the right type, and
      // RAUW them.
      for (unsigned i = 0, e = Entries.size(); i != e; ++i) {
        if (!Entries[i]) continue;

        if (Entries[i]->getType(0) != Value->getType(i)) {
          P.diagnose(Loc, diag::sil_value_def_type_mismatch, Name,
                     Entry->getType(0).getAsString());
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
///     /*empty*/           // defaults to external linkage.
///     'internal'
///     'clang_thunk'
static bool parseSILLinkage(SILLinkage &Result, Parser &P) {
  if (P.Tok.isNot(tok::identifier)) {
    Result = SILLinkage::External;
  } else if (P.Tok.getText() == "internal") {
    Result = SILLinkage::Internal;
    P.consumeToken(tok::identifier);
  } else if (P.Tok.getText() == "clang_thunk") {
    Result = SILLinkage::ClangThunk;
    P.consumeToken(tok::identifier);
  } else {
    P.diagnose(P.Tok, diag::expected_sil_linkage_or_function);
    return true;
  }
  return false;
}

/// Parse an option attribute ('[' Expected ']')?
static bool parseSILOptional(bool &Result, Parser &P, StringRef Expected) {
  if (P.consumeIf(tok::l_square)) {
    Identifier Id;
    P.parseIdentifier(Id, diag::expected_in_attribute_list);
    if (Id.str() != Expected)
      return true;
    P.parseToken(tok::r_square, diag::expected_in_attribute_list);
    Result = true;
  }
  return false;
}

/// Construct ArchetypeType from Generic Params.
bool SILParser::handleGenericParams(GenericParamList *GenericParams) {
  ArchetypeBuilder Builder(P.Context, P.Diags);
  unsigned Index = 0;
  for (auto GP : *GenericParams) {
    auto TypeParam = GP.getAsTypeParam();
    // Do some type checking / name binding for Inherited.
    for (auto &Inherited : TypeParam->getInherited()) {
      if (performTypeLocChecking(Inherited))
        return true;
    }
    // Add the generic parameter to the builder.
    Builder.addGenericParameter(TypeParam, Index++);
  }
  // Add the requirements clause to the builder.
  for (auto &Req : GenericParams->getRequirements()) {
    if (Req.isInvalid())
      continue;

    if (Builder.addRequirement(Req))
      Req.setInvalid();
  }
  // Wire up the archetypes.
  Builder.assignArchetypes();
  for (auto GP : *GenericParams) {
    auto TypeParam = GP.getAsTypeParam();

    TypeParam->getUnderlyingTypeLoc()
      = TypeLoc::withoutLoc(Builder.getArchetype(TypeParam));
  }
  GenericParams->setAllArchetypes(
    P.Context.AllocateCopy(Builder.getAllArchetypes()));
  return false;
}

bool SILParser::performTypeLocChecking(TypeLoc &T) {
  // Do some type checking / name binding for the parsed type.
  // We have to lie and say we're done with parsing to make this happen.
  assert(P.TU->ASTStage == TranslationUnit::Parsing &&
         "Unexpected stage during parsing!");
  llvm::SaveAndRestore<Module::ASTStage_t> ASTStage(P.TU->ASTStage,
                                                    TranslationUnit::Parsed);
  return swift::performTypeLocChecking(P.TU, T);
}

///   sil-type:
///     '$' '*'? attribute-list (generic-params)? type
///
bool SILParser::parseSILType(SILType &Result) {
  if (P.parseToken(tok::sil_dollar, diag::expected_sil_type))
    return true;

  // If we have a '*', then this is an address type.
  bool isAddress = false;
  if (P.Tok.isAnyOperator() && P.Tok.getText() == "*") {
    isAddress = true;
    P.consumeToken();
  }

  // Parse attributes.
  DeclAttributes attrs;
  P.parseAttributeList(attrs);

  // Parse Generic Parameters. Generic Parameters are visible in the function
  // body.
  GenericParamList *PList = P.maybeParseGenericParams();

  TypeRepr *TyR = P.parseType(diag::expected_sil_type);
  if (!TyR)
    return true;
  
  // Apply attributes to the type.
  TyR = P.applyAttributeToType(TyR, attrs);

  TypeLoc Ty = TyR;

  if (PList)
    if (handleGenericParams(PList))
      return true;
  if (performTypeLocChecking(Ty))
    return true;

  // Build PolymorphicFunctionType if necessary.
  FunctionType *FT = dyn_cast<FunctionType>(Ty.getType().getPointer());
  if (FT && PList) {
    Type resultType = PolymorphicFunctionType::get(FT->getInput(),
                                             FT->getResult(), PList,
                                             attrs.isThin(),
                                             attrs.hasCC()
                                             ? attrs.getAbstractCC()
                                             : AbstractCC::Freestanding,
                                             P.Context);
    Ty.setType(resultType);
    // Reset attributes that are applied.
    attrs.Thin = false;
    attrs.cc = Nothing;
  }

  if (performTypeLocChecking(Ty))
    return true;

  Result = SILType::getPrimitiveType(Ty.getType()->getCanonicalType(),
                                     isAddress);
  return false;
}

/// sil-constant:
///   '#' sil-dotted-path sil-constant-kind-and-uncurry-level?
/// sil-dotted-path:
///   identifier ('.' identifier)*
/// sil-constant-kind-and-uncurry-level:
///   '!' sil-constant-kind ('.' sil-constant-uncurry-level)? ('.objc')?
///   '!' sil-constant-uncurry-level ('.objc')?
///   '!objc'
/// sil-constant-kind:
///   'func' | 'getter' | 'setter' | 'allocator' | 'initializer' | 'oneofelt' \
///   | 'destroyer' | 'globalaccessor'
bool SILParser::parseSILConstant(SILConstant &Result) {
  if (P.parseToken(tok::sil_pound, diag::expected_sil_constant))
    return true;

  // Handle sil-dotted-path.
  Identifier Id;
  SmallVector<Identifier, 4> FullName;
  do {
    if (P.parseIdentifier(Id, diag::expected_sil_constant))
      return true;
    FullName.push_back(Id);
  } while (P.consumeIf(tok::period));

  // Look up ValueDecl from FullName.
  ValueDecl *VD;
  llvm::SmallVector<ValueDecl*, 4> CurModuleResults;
  // Perform a module level lookup on the first component of the fully-qualified
  // name.
  P.TU->lookupValue(Module::AccessPathTy(), FullName[0],
                    NLKind::UnqualifiedLookup, CurModuleResults);
  assert(CurModuleResults.size() == 1);
  VD = CurModuleResults[0];
  for (unsigned I = 1, E = FullName.size(); I < E; I++) {
    // Look up members of VD for FullName[i].
    SmallVector<ValueDecl *, 4> Lookup;
    P.TU->lookupQualified(VD->getType(), FullName[I], NL_QualifiedDefault,
                          Lookup);
    assert(Lookup.size() == 1);
    VD = Lookup[0];
  }

  // Initialize Kind, uncurryLevel and IsObjC.
  SILConstant::Kind Kind = SILConstant::Kind::Func;
  unsigned uncurryLevel = 0;
  bool IsObjC = false;

  if (!P.consumeIf(tok::sil_exclamation)) {
    // Construct SILConstant.
    Result = SILConstant(VD, Kind, uncurryLevel, IsObjC);
    return false;
  }

  // Handle sil-constant-kind-and-uncurry-level.
  // ParseState indicates the value we just handled.
  // 1 means we just handled Kind, 2 means we just handled uncurryLevel.
  // We accept func|getter|setter|...|objc or an integer when ParseState is 0;
  // accept objc or an integer when ParseState is 1; accept objc when ParseState
  // is 2.
  unsigned ParseState = 0;
  do {
    if (P.Tok.is(tok::identifier)) {
      if (P.parseIdentifier(Id, diag::expected_sil_constant))
        return true;
      if (!ParseState && Id.str() == "func") {
        Kind = SILConstant::Kind::Func;
        ParseState = 1;
      } else if (!ParseState && Id.str() == "getter") {
        Kind = SILConstant::Kind::Getter;
        ParseState = 1;
      } else if (!ParseState && Id.str() == "setter") {
        Kind = SILConstant::Kind::Setter;
        ParseState = 1;
      } else if (!ParseState && Id.str() == "allocator") {
        Kind = SILConstant::Kind::Allocator;
        ParseState = 1;
      } else if (!ParseState && Id.str() == "initializer") {
        Kind = SILConstant::Kind::Initializer;
        ParseState = 1;
      } else if (!ParseState && Id.str() == "oneofelt") {
        Kind = SILConstant::Kind::OneOfElement;
        ParseState = 1;
      } else if (!ParseState && Id.str() == "destroyer") {
        Kind = SILConstant::Kind::Destroyer;
        ParseState = 1;
      } else if (!ParseState && Id.str() == "globalaccessor") {
        Kind = SILConstant::Kind::GlobalAccessor;
        ParseState = 1;
      } else if (Id.str() == "objc") {
        IsObjC = true;
        break;
      } else
        break;
    } else if (ParseState < 2 && P.Tok.is(tok::integer_literal)) {
      P.Tok.getText().getAsInteger(0, uncurryLevel);
      P.consumeToken(tok::integer_literal);
      ParseState = 2;
    } else
      break;

  } while (P.consumeIf(tok::period));

  // Construct SILConstant.
  Result = SILConstant(VD, Kind, uncurryLevel, IsObjC);
  return false;
}

/// parseValueName - Parse a value name without a type available yet.
///
///     sil-value-name:
///       sil-local-name ('#' integer_literal)?
///
bool SILParser::parseValueName(UnresolvedValueName &Result) {
  Result.Name = P.Tok.getText();

  // Parse the local-name.
  if (P.parseToken(tok::sil_local_name, Result.NameLoc,
                   diag::expected_sil_value_name))
    return true;

  // If the result value specifier is present, parse it.
  if (P.consumeIf(tok::sil_pound)) {
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
bool SILParser::parseValueRef(SILValue &Result, SILType Ty) {
  UnresolvedValueName Name;
  if (parseValueName(Name)) return true;
  Result = getLocalValue(Name, Ty);
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
  
  Result = getLocalValue(Name, Ty);
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
    .Case("address_to_pointer", ValueKind::AddressToPointerInst)
    .Case("alloc_var", ValueKind::AllocVarInst)
    .Case("alloc_ref", ValueKind::AllocRefInst)
    .Case("archetype_method", ValueKind::ArchetypeMethodInst)
    .Case("archetype_ref_to_super", ValueKind::ArchetypeRefToSuperInst)
    .Case("apply", ValueKind::ApplyInst)
    .Case("br", ValueKind::BranchInst)
    .Case("bridge_to_block", ValueKind::BridgeToBlockInst)
    .Case("builtin_zero", ValueKind::BuiltinZeroInst)
    .Case("class_method", ValueKind::ClassMethodInst)
    .Case("coerce", ValueKind::CoerceInst)
    .Case("condbranch", ValueKind::CondBranchInst)
    .Case("convert_cc", ValueKind::ConvertCCInst)
    .Case("convert_function", ValueKind::ConvertFunctionInst)
    .Case("copy_addr", ValueKind::CopyAddrInst)
    .Case("dealloc_var", ValueKind::DeallocVarInst)
    .Case("destroy_addr", ValueKind::DestroyAddrInst)
    .Case("downcast", ValueKind::DowncastInst)
    .Case("downcast_archetype_addr", ValueKind::DowncastArchetypeAddrInst)
    .Case("downcast_archetype_ref", ValueKind::DowncastArchetypeRefInst)
    .Case("downcast_existential_ref", ValueKind::DowncastExistentialRefInst)
    .Case("initialize_var", ValueKind::InitializeVarInst)
    .Case("integer_literal", ValueKind::IntegerLiteralInst)
    .Case("function_ref", ValueKind::FunctionRefInst)
    .Case("load", ValueKind::LoadInst)
    .Case("metatype", ValueKind::MetatypeInst)
    .Case("object_pointer_to_ref", ValueKind::ObjectPointerToRefInst)
    .Case("partial_apply", ValueKind::PartialApplyInst)
    .Case("pointer_to_address", ValueKind::PointerToAddressInst)
    .Case("project_downcast_existential_addr",
          ValueKind::ProjectDowncastExistentialAddrInst)
    .Case("project_existential", ValueKind::ProjectExistentialInst)
    .Case("project_existential_ref", ValueKind::ProjectExistentialRefInst)
    .Case("protocol_method", ValueKind::ProtocolMethodInst)
    .Case("raw_pointer_to_ref", ValueKind::RawPointerToRefInst)
    .Case("ref_element_addr", ValueKind::RefElementAddrInst)
    .Case("ref_to_object_pointer", ValueKind::RefToObjectPointerInst)
    .Case("ref_to_raw_pointer", ValueKind::RefToRawPointerInst)
    .Case("release", ValueKind::ReleaseInst)
    .Case("retain", ValueKind::RetainInst)
    .Case("retain_autoreleased", ValueKind::RetainAutoreleasedInst)
    .Case("return", ValueKind::ReturnInst)
    .Case("store", ValueKind::StoreInst)
    .Case("struct", ValueKind::StructInst)
    .Case("struct_element_addr", ValueKind::StructElementAddrInst)
    .Case("struct_extract", ValueKind::StructExtractInst)
    .Case("super_to_archetype_ref", ValueKind::SuperToArchetypeRefInst)
    .Case("thin_to_thick_function", ValueKind::ThinToThickFunctionInst)
    .Case("tuple", ValueKind::TupleInst)
    .Case("tuple_element_addr", ValueKind::TupleElementAddrInst)
    .Case("tuple_extract", ValueKind::TupleExtractInst)
    .Case("upcast", ValueKind::UpcastInst)
    .Case("upcast_existential", ValueKind::UpcastExistentialInst)
    .Case("upcast_existential_ref", ValueKind::UpcastExistentialRefInst)
    .Default(ValueKind::SILArgument);

  if (Opcode != ValueKind::SILArgument) {
    P.consumeToken();
    return false;
  }
  P.diagnose(OpcodeLoc, diag::expected_sil_instr_opcode);
  return true;
}

/// Find the ValueDecl given a member name and a type.
static ValueDecl *lookupMember(Parser &P, Type Ty, Identifier Name) {
  SmallVector<ValueDecl *, 4> Lookup;
  P.TU->lookupQualified(Ty, Name, NL_QualifiedDefault, Lookup);
  assert(Lookup.size() == 1);
  ValueDecl *VD = Lookup[0];
  return VD;
}

///   sil-instruction:
///     sil_local_name '=' identifier ...
bool SILParser::parseSILInstruction(SILBasicBlock *BB) {
  if (P.Tok.isNot(tok::sil_local_name)) {
    P.diagnose(P.Tok, diag::expected_sil_instr_name);
    return true;
  }

  // We require SIL instructions to be at the start of a line to assist
  // recovery.
  if (!P.Tok.isAtStartOfLine()) {
    P.diagnose(P.Tok, diag::expected_sil_instr_start_of_line);
    return true;
  }
  StringRef ResultName = P.Tok.getText();
  SourceLoc ResultNameLoc = P.Tok.getLoc();
  P.consumeToken(tok::sil_local_name);
  
  ValueKind Opcode;
  SourceLoc OpcodeLoc;
  StringRef OpcodeName;
  
  // Parse the equal and opcode name.
  if (P.parseToken(tok::equal, diag::expected_equal_in_sil_instr) ||
      parseSILOpcode(Opcode, OpcodeLoc, OpcodeName))
    return true;

  SILBuilder B(BB);
  SmallVector<SILValue, 4> OpList;
  SILValue Val;

  // Validate the opcode name, and do opcode-specific parsing logic based on the
  // opcode we find.
  ValueBase *ResultVal;
  switch (Opcode) {
  default: assert(0 && "Unreachable");
  case ValueKind::AllocBoxInst: {
    SILType Ty;
    if (parseSILType(Ty)) return true;
    ResultVal = B.createAllocBox(SILLocation(), Ty);
    break;
  }
  case ValueKind::ApplyInst:
  case ValueKind::PartialApplyInst:
    if (parseCallInstruction(Opcode, B, ResultVal))
      return true;
    break;
  case ValueKind::IntegerLiteralInst: {
    SILType Ty;
    if (parseSILType(Ty) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ","))
      return true;
    
    if (P.Tok.getKind() != tok::integer_literal) {
      P.diagnose(P.Tok, diag::expected_tok_in_sil_instr, "integer");
      return true;
    }
    
    ResultVal = B.createIntegerLiteral(SILLocation(), Ty, P.Tok.getText());
    P.consumeToken(tok::integer_literal);
    break;
  }
      
  case ValueKind::FunctionRefInst:
    if (parseSILFunctionRef(B, ResultVal))
      return true;
    break;
  case ValueKind::ProjectExistentialInst:
    if (parseTypedValueRef(Val)) return true;
    ResultVal = B.createProjectExistential(SILLocation(), Val);
    break;
  case ValueKind::ProjectExistentialRefInst:
    if (parseTypedValueRef(Val)) return true;
    ResultVal = B.createProjectExistentialRef(SILLocation(), Val);
    break;
      
  case ValueKind::RetainInst:
    if (parseTypedValueRef(Val)) return true;
    ResultVal = B.createRetainInst(SILLocation(), Val);
    break;
  case ValueKind::ReleaseInst:
    if (parseTypedValueRef(Val)) return true;
    ResultVal = B.createReleaseInst(SILLocation(), Val);
    break;
  case ValueKind::RetainAutoreleasedInst:
    if (parseTypedValueRef(Val)) return true;
    ResultVal = B.createRetainAutoreleased(SILLocation(), Val);
    break;
  case ValueKind::DestroyAddrInst:
    if (parseTypedValueRef(Val)) return true;
    ResultVal = B.createDestroyAddr(SILLocation(), Val);
    break;

  case ValueKind::LoadInst:
    if (parseTypedValueRef(Val)) return true;
    ResultVal = B.createLoad(SILLocation(), Val);
    break;

    // Conversion instructions.
  case ValueKind::RefToObjectPointerInst:
  case ValueKind::UpcastInst:
  case ValueKind::CoerceInst:
  case ValueKind::AddressToPointerInst:
  case ValueKind::PointerToAddressInst:
  case ValueKind::ObjectPointerToRefInst:
  case ValueKind::RefToRawPointerInst:
  case ValueKind::RawPointerToRefInst:
  case ValueKind::ConvertCCInst:
  case ValueKind::ThinToThickFunctionInst:
  case ValueKind::BridgeToBlockInst:
  case ValueKind::ArchetypeRefToSuperInst:
  case ValueKind::ConvertFunctionInst:
  case ValueKind::UpcastExistentialRefInst: {
    SILType Ty;
    Identifier ToToken;
    SourceLoc ToLoc;
    if (parseTypedValueRef(Val) ||
        P.parseIdentifier(ToToken,ToLoc,diag::expected_tok_in_sil_instr, "to")||
        parseSILType(Ty))
      return true;

    if (ToToken.str() != "to") {
      P.diagnose(ToLoc, diag::expected_tok_in_sil_instr, "to");
      return true;
    }

    switch (Opcode) {
    default: assert(0 && "Out of sync with parent switch");
    case ValueKind::RefToObjectPointerInst:
      ResultVal = B.createRefToObjectPointer(SILLocation(), Val, Ty);
      break;
    case ValueKind::UpcastInst:
      ResultVal = B.createUpcast(SILLocation(), Val, Ty);
      break;
    case ValueKind::ConvertFunctionInst:
      ResultVal = B.createConvertFunction(SILLocation(), Val, Ty);
      break;
    case ValueKind::CoerceInst:
      ResultVal = B.createCoerce(SILLocation(), Val, Ty);
      break;
    case ValueKind::AddressToPointerInst:
      ResultVal = B.createAddressToPointer(SILLocation(), Val, Ty);
      break;
    case ValueKind::PointerToAddressInst:
      ResultVal = B.createPointerToAddress(SILLocation(), Val, Ty);
      break;
    case ValueKind::ObjectPointerToRefInst:
      ResultVal = B.createObjectPointerToRef(SILLocation(), Val, Ty);
      break;
    case ValueKind::RefToRawPointerInst:
      ResultVal = B.createRefToRawPointer(SILLocation(), Val, Ty);
      break;
    case ValueKind::RawPointerToRefInst:
      ResultVal = B.createRawPointerToRef(SILLocation(), Val, Ty);
      break;
    case ValueKind::ConvertCCInst:
      ResultVal = B.createConvertCC(SILLocation(), Val, Ty);
      break;
    case ValueKind::ThinToThickFunctionInst:
      ResultVal = B.createThinToThickFunction(SILLocation(), Val, Ty);
      break;
    case ValueKind::BridgeToBlockInst:
      ResultVal = B.createBridgeToBlock(SILLocation(), Val, Ty);
      break;
    case ValueKind::ArchetypeRefToSuperInst:
      ResultVal = B.createArchetypeRefToSuper(SILLocation(), Val, Ty);
      break;
    case ValueKind::UpcastExistentialRefInst:
      ResultVal = B.createUpcastExistentialRef(SILLocation(), Val, Ty);
      break;
    }
    break;
  }
    // Checked Conversion instructions.
  case ValueKind::DowncastInst:
  case ValueKind::SuperToArchetypeRefInst:
  case ValueKind::DowncastArchetypeAddrInst:
  case ValueKind::DowncastArchetypeRefInst:
  case ValueKind::ProjectDowncastExistentialAddrInst:
  case ValueKind::DowncastExistentialRefInst: {
    SILType Ty;
    Identifier CheckedToken, ToToken;
    SourceLoc CheckedLoc, ToLoc;
    if (P.parseIdentifier(CheckedToken, CheckedLoc,
                          diag::expected_tok_in_sil_instr,
                          "conditional or unconditional") ||
        parseTypedValueRef(Val) ||
        P.parseIdentifier(ToToken,ToLoc,diag::expected_tok_in_sil_instr, "to")||
        parseSILType(Ty))
      return true;

    CheckedCastMode Mode;
    if (CheckedToken.str() == "conditional")
      Mode = CheckedCastMode::Conditional;
    else if (CheckedToken.str() == "unconditional")
      Mode = CheckedCastMode::Unconditional;
    else {
      P.diagnose(CheckedLoc, diag::expected_tok_in_sil_instr,
                 "conditional or unconditional");
      return true;
    }

    if (ToToken.str() != "to") {
      P.diagnose(ToLoc, diag::expected_tok_in_sil_instr, "to");
      return true;
    }

    switch (Opcode) {
    default: assert(0 && "Out of sync with parent switch");
    case ValueKind::DowncastInst:
      ResultVal = B.createDowncast(SILLocation(), Val, Ty, Mode);
      break;
    }
    break;
  }

  case ValueKind::StoreInst: {
    UnresolvedValueName From;

    SourceLoc ToLoc, AddrLoc;
    Identifier ToToken;
    SILValue AddrVal;
    if (parseValueName(From) ||
        P.parseIdentifier(ToToken, ToLoc,
                          diag::expected_tok_in_sil_instr, "to") ||
        parseTypedValueRef(AddrVal, AddrLoc))
      return true;

    if (ToToken.str() != "to") {
      P.diagnose(ToLoc, diag::expected_tok_in_sil_instr, "to");
      return true;
    }
    
    if (!AddrVal.getType().isAddress()) {
      P.diagnose(AddrLoc, diag::sil_invalid_instr_operands);
      return true;
    }

    SILValue FromVal = getLocalValue(From, AddrVal.getType().getObjectType());
    ResultVal = B.createStore(SILLocation(), FromVal, AddrVal);
    break;
  }
  case ValueKind::AllocVarInst:
  case ValueKind::AllocRefInst: {
    SILType Ty;
    if (parseSILType(Ty))
      return true;
    
    if (Opcode == ValueKind::AllocVarInst)
      ResultVal = B.createAllocVar(SILLocation(), Ty);
    else {
      assert(Opcode == ValueKind::AllocRefInst);
      ResultVal = B.createAllocRef(SILLocation(), Ty);
    }
    break;
  }
  case ValueKind::DeallocVarInst: {
    if (parseTypedValueRef(Val))
      return true;
    
    ResultVal = B.createDeallocVar(SILLocation(), Val);
    break;
  }
  case ValueKind::MetatypeInst: {
    SILType Ty;
    if (parseSILType(Ty))
      return true;
    ResultVal = B.createMetatype(SILLocation(), Ty);
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
      auto Ty2 = SILType::getPrimitiveType(Ty->getCanonicalType(), false);
      ResultVal = B.createTuple(SILLocation(), Ty2, OpList);
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
                          SILType::getPrimitiveType(EltTy->getCanonicalType())))
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

    ResultVal = B.createTuple(SILLocation(), Ty, OpList);
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

    auto ResultTy = SILType::getPrimitiveType(
                      TT->getFields()[Field].getType()->getCanonicalType(),
                      Opcode == ValueKind::TupleElementAddrInst);
    if (Opcode == ValueKind::TupleElementAddrInst)
      ResultVal = B.createTupleElementAddr(SILLocation(), Val, Field,
                                           ResultTy);
    else
      ResultVal = B.createTupleExtract(SILLocation(), Val, Field,
                                       ResultTy).getDef();
    break;
  }
  case ValueKind::ReturnInst: {
    if (parseTypedValueRef(Val)) return true;
    ResultVal = B.createReturn(SILLocation(), Val);
    break;
  }
  case ValueKind::BranchInst: {
    Identifier BBName;
    SourceLoc NameLoc;
    if (P.parseIdentifier(BBName, NameLoc, diag::expected_sil_block_name))
      return true;
    ResultVal = B.createBranch(SILLocation(),
                               getBBForReference(BBName, NameLoc));
    break;
  }
  case ValueKind::CondBranchInst: {
    UnresolvedValueName Cond;
    Identifier BBName, BBName2;
    SourceLoc NameLoc, NameLoc2;
    if (parseValueName(Cond) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        P.parseIdentifier(BBName, NameLoc, diag::expected_sil_block_name) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        P.parseIdentifier(BBName2, NameLoc2, diag::expected_sil_block_name))
      return true;
   
    auto I1Ty =
      SILType::getBuiltinIntegerType(1, BB->getParent()->getASTContext());
    SILValue CondVal = getLocalValue(Cond, I1Ty);
    ResultVal = B.createCondBranch(SILLocation(), CondVal,
                                   getBBForReference(BBName, NameLoc),
                                   getBBForReference(BBName2, NameLoc2));
    break;
  }
  case ValueKind::ProtocolMethodInst: {
    bool IsVolatile = false;
    if (parseSILOptional(IsVolatile, P, "volatile"))
      return true;
    SILConstant Member;
    SILType MethodTy;
    SourceLoc TyLoc;
    if (parseTypedValueRef(Val) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseSILConstant(Member) ||
        P.parseToken(tok::colon, diag::expected_tok_in_sil_instr, ":") ||
        parseSILType(MethodTy, TyLoc)
       )
      return true;
    ResultVal = B.createProtocolMethod(SILLocation(), Val, Member, MethodTy,
                                       IsVolatile);
    break;
  }
  case ValueKind::ClassMethodInst: {
    bool IsVolatile = false;
    if (parseSILOptional(IsVolatile, P, "volatile"))
      return true;
    SILConstant Member;
    SILType MethodTy;
    SourceLoc TyLoc;
    if (parseTypedValueRef(Val) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseSILConstant(Member) ||
        P.parseToken(tok::colon, diag::expected_tok_in_sil_instr, ":") ||
        parseSILType(MethodTy, TyLoc)
       )
      return true;
    ResultVal = B.createClassMethod(SILLocation(), Val, Member, MethodTy,
                                    IsVolatile);
    break;
  }
  case ValueKind::ArchetypeMethodInst: {
    bool IsVolatile = false;
    if (parseSILOptional(IsVolatile, P, "volatile"))
      return true;
    SILType LookupTy;
    SILConstant Member;
    SILType MethodTy;
    SourceLoc TyLoc;
    if (parseSILType(LookupTy, TyLoc) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseSILConstant(Member) ||
        P.parseToken(tok::colon, diag::expected_tok_in_sil_instr, ":") ||
        parseSILType(MethodTy, TyLoc)
       )
      return true;
    ResultVal = B.createArchetypeMethod(SILLocation(), LookupTy, Member,
                                        MethodTy, IsVolatile);
    break;
  }
  case ValueKind::CopyAddrInst: {
    bool IsTake = false, IsInit = false;
    UnresolvedValueName SrcLName;
    SILValue DestLVal;
    SourceLoc ToLoc, DestLoc;
    Identifier ToToken;
    if (parseSILOptional(IsTake, P, "take") || parseValueName(SrcLName) ||
        P.parseIdentifier(ToToken, ToLoc,
                          diag::expected_tok_in_sil_instr, "to") ||
        parseSILOptional(IsInit, P, "initialization") ||
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

    SILValue SrcLVal = getLocalValue(SrcLName, DestLVal.getType());
    ResultVal = B.createCopyAddr(SILLocation(), SrcLVal, DestLVal, IsTake,
                                IsInit);
    break;
  }
  case ValueKind::UpcastExistentialInst: {
    SILValue DestVal;
    SourceLoc SrcLoc, DestLoc, ToLoc;
    Identifier ToToken;
    bool IsTake = false;
    if (parseSILOptional(IsTake, P, "take") ||
        parseTypedValueRef(Val, SrcLoc) ||
        P.parseIdentifier(ToToken, ToLoc,
                          diag::expected_tok_in_sil_instr, "to") ||
        parseTypedValueRef(DestVal, DestLoc))
      return true;

    if (ToToken.str() != "to") {
      P.diagnose(ToLoc, diag::expected_tok_in_sil_instr, "to");
      return true;
    }
    ResultVal = B.createUpcastExistential(SILLocation(), Val, DestVal,
                                          IsTake);
    break;
  }
  case ValueKind::InitializeVarInst: {
    bool NoDefault = false;
    if (parseSILOptional(NoDefault, P, "no_default_construct") ||
        parseTypedValueRef(Val))
      return true;
    ResultVal = B.createInitializeVar(SILLocation(), Val, !NoDefault);
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

    ResultVal = B.createStruct(SILLocation(), StructTy, OpList);
    break;
  }
  case ValueKind::StructElementAddrInst:
  case ValueKind::StructExtractInst: {
    Identifier ElemId;
    SourceLoc NameLoc;
    if (parseTypedValueRef(Val) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        P.parseToken(tok::sil_at_sign, diag::expected_tok_in_sil_instr, "@") ||
        P.parseIdentifier(ElemId, NameLoc, diag::sil_struct_inst_wrong_field))
      return true;
    ValueDecl *FieldV = lookupMember(P, Val.getType().getSwiftRValueType(),
                                     ElemId);
    if (!FieldV || !isa<VarDecl>(FieldV)) {
      P.diagnose(NameLoc, diag::sil_struct_inst_wrong_field);
      return true;
    }
    VarDecl *Field = cast<VarDecl>(FieldV);
    auto ResultTy = SILType::getPrimitiveType(
                      Field->getType()->getCanonicalType(),
                      Opcode == ValueKind::StructElementAddrInst);
    if (Opcode == ValueKind::StructElementAddrInst)
      ResultVal = B.createStructElementAddr(SILLocation(), Val, Field,
                                            ResultTy);
    else
      ResultVal = B.createStructExtract(SILLocation(), Val, Field, ResultTy);
    break;
  }
  case ValueKind::BuiltinZeroInst: {
    SILType Ty;
    if (parseSILType(Ty))
      return true;
    ResultVal = B.createBuiltinZero(SILLocation(), Ty);
    break;
  }
  case ValueKind::RefElementAddrInst: {
    Identifier ElemId;
    SourceLoc NameLoc;
    if (parseTypedValueRef(Val) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        P.parseToken(tok::sil_at_sign, diag::expected_tok_in_sil_instr, "@") ||
        P.parseIdentifier(ElemId, NameLoc, diag::sil_ref_inst_wrong_field))
      return true;
    ValueDecl *FieldV = lookupMember(P, Val.getType().getSwiftRValueType(),
                                     ElemId);
    if (!FieldV || !isa<VarDecl>(FieldV)) {
      P.diagnose(NameLoc, diag::sil_ref_inst_wrong_field);
      return true;
    }
    VarDecl *Field = cast<VarDecl>(FieldV);
    auto ResultTy = SILType::getPrimitiveType(
                      Field->getType()->getCanonicalType(), true);
    ResultVal = B.createRefElementAddr(SILLocation(), Val, Field,
                                       ResultTy).getDef();
    break;
  }
  }
  
  setLocalValue(ResultVal, ResultName, ResultNameLoc);
  return false;
}

bool SILParser::parseCallInstruction(ValueKind Opcode, SILBuilder &B,
                                     ValueBase *&ResultVal) {
  UnresolvedValueName FnName;
  SmallVector<UnresolvedValueName, 4> ArgNames;

  if (parseValueName(FnName) ||
      P.parseToken(tok::l_paren, diag::expected_tok_in_sil_instr, "("))
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
  if (P.parseToken(tok::r_paren, diag::expected_tok_in_sil_instr, ")") ||
      P.parseToken(tok::colon, diag::expected_tok_in_sil_instr, ":") ||
      parseSILType(Ty, TypeLoc))
    return true;

  CanType ShTy = Ty.getSwiftType();
  if (!ShTy->is<FunctionType>() && !ShTy->is<PolymorphicFunctionType>()) {
    P.diagnose(TypeLoc, diag::expected_sil_type_kind, "be a function");
    return true;
  }

  SILFunctionTypeInfo *FTI = Ty.getFunctionTypeInfo(SILMod);

  auto ArgTys = FTI->getInputTypes();
  if (ArgTys.size() != ArgNames.size()) {
    P.diagnose(TypeLoc, diag::expected_sil_type_kind,
               " have the right argument types");
    return true;
  }


  SILValue FnVal = getLocalValue(FnName, Ty);
  unsigned ArgNo = 0;
  SmallVector<SILValue, 4> Args;
  for (auto &ArgName : ArgNames)
    Args.push_back(getLocalValue(ArgName, ArgTys[ArgNo++]));

  SILType ResTy = FTI->getResultType();
  switch (Opcode) {
  default: assert(0 && "Unexpected case");
  case ValueKind::ApplyInst:
    ResultVal = B.createApply(SILLocation(), FnVal, ResTy, Args);
    break;
  case ValueKind::PartialApplyInst:
      // FIXME: Arbitrary order difference in type argument?
    ResultVal = B.createPartialApply(SILLocation(), FnVal, Args, ResTy);
    break;
  }
  return false;
}

bool SILParser::parseSILFunctionRef(SILBuilder &B, ValueBase *&ResultVal) {
  Identifier Name;
  SILType Ty;
  SourceLoc Loc = P.Tok.getLoc();
  if (parseGlobalName(Name) ||
      P.parseToken(tok::colon, diag::expected_sil_colon_value_ref) ||
      parseSILType(Ty))
    return true;
  
  ResultVal = B.createFunctionRef(SILLocation(),
                                  getGlobalNameForReference(Name, Ty, Loc));
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
    if (P.parseIdentifier(BBName, NameLoc, diag::expected_sil_block_name))
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
        auto Arg = new (SILMod) SILArgument(Ty, BB);
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
  } while (P.Tok.is(tok::sil_local_name));

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

  SILLinkage FnLinkage;
  Identifier FnName;
  SILType FnType;
  SourceLoc FnNameLoc;

  Scope S(this, ScopeKind::TopLevel);
  if (parseSILLinkage(FnLinkage, *this) ||
      parseToken(tok::sil_at_sign, diag::expected_sil_function_name) ||
      parseIdentifier(FnName, FnNameLoc, diag::expected_sil_function_name) ||
      parseToken(tok::colon, diag::expected_sil_type))
    return true;
  {
    // Construct a Scope for the function body so TypeAliasDecl can be added to
    // the scope.
    Scope Body(this, ScopeKind::FunctionBody);
    if (FunctionState.parseSILType(FnType))
      return true;

    // TODO: Verify it is a function type.
  
    FunctionState.F =
      FunctionState.getGlobalNameForDefinition(FnName, FnType, FnNameLoc);
    FunctionState.F->setLinkage(FnLinkage);

    // Now that we have a SILFunction parse the body, if present.

    SourceLoc LBraceLoc = Tok.getLoc();
    if (consumeIf(tok::l_brace)) {
      // Parse the basic block list.
      do {
        if (FunctionState.parseSILBasicBlock())
          return true;
      } while (Tok.isNot(tok::r_brace) && Tok.isNot(tok::eof));

      SourceLoc RBraceLoc;
      parseMatchingToken(tok::r_brace, RBraceLoc, diag::expected_sil_rbrace,
                         LBraceLoc);
    } 
  }

  return FunctionState.diagnoseProblems();
}

