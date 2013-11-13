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
#include "swift/AST/NameLookup.h"
#include "swift/Parse/Parser.h"
#include "swift/Parse/Lexer.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
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

    /// @}

    // Parsing logic.
    bool parseSILType(SILType &Result);
    bool parseSILType(SILType &Result, SourceLoc &TypeLoc) {
      TypeLoc = P.Tok.getLoc();
      return parseSILType(Result);
    }

    bool parseSILDeclRef(SILDeclRef &Result);
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
  };
} // end anonymous namespace.

bool SILParser::parseSILIdentifier(Identifier &Result, SourceLoc &Loc,
                                   const Diagnostic &D) {
  switch (P.Tok.getKind()) {
  case tok::identifier:
    Result = P.Context.getIdentifier(P.Tok.getText());
    break;
  case tok::kw_init:
    Result = P.Context.getIdentifier("init");
    break;
  case tok::kw_subscript:
    Result = P.Context.getIdentifier("subscript");
    break;
  default:
    P.diagnose(P.Tok, D);
    return true;
  }

  Loc = P.Tok.getLoc();
  P.consumeToken();
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
      Fn = new (SILMod) SILFunction(SILMod, SILLinkage::Internal, "", Ty,
                                    SILFileLocation(Loc));
    }
    
    assert(Fn->isExternalDeclaration() && "Forward defns cannot have bodies!");
    TUState.ForwardRefFns.erase(It);
    return Fn;
  }
  
  // If we don't have a forward reference, make sure the function hasn't been
  // defined already.
  if (SILMod.lookup(Name.str()) != nullptr) {
    P.diagnose(Loc, diag::sil_value_redefinition, Name.str());
    return new (SILMod) SILFunction(SILMod, SILLinkage::Internal, "", Ty,
                                    SILFileLocation(Loc));
  }

  // Otherwise, this definition is the first use of this name.
  return new (SILMod) SILFunction(SILMod, SILLinkage::Internal, Name.str(), Ty,
                                  SILFileLocation(Loc));
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
      FnRef = new (SILMod) SILFunction(SILMod, SILLinkage::Internal, "", Ty,
                                       SILFileLocation(Loc));
    }
    return FnRef;
  }
  
  // If we didn't find a function, create a new one - it must be a forward
  // reference.
  auto Fn = new (SILMod) SILFunction(SILMod, SILLinkage::Internal,
                                     Name.str(), Ty, SILFileLocation(Loc));
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
        return new (SILMod) GlobalAddrInst(Loc, nullptr, Type);
      }
    }

    EntryTy = Entry->getType(Name.isMRV() ? Name.ResultVal : 0);

    if (EntryTy != Type) {
      HadError = true;
      P.diagnose(Name.NameLoc, diag::sil_value_use_type_mismatch, Name.Name,
                 EntryTy.getAsString());
      // Make sure to return something of the requested type.
      return new (SILMod) GlobalAddrInst(Loc, nullptr, Type);
    }

    return SILValue(Entry, Name.isMRV() ? Name.ResultVal : 0);
  }
  
  // Otherwise, this is a forward reference.  Create a dummy node to represent
  // it until we see a real definition.
  ForwardRefLocalValues[Name.Name] = Name.NameLoc;

  if (!Name.isMRV()) {
    Entry = new (SILMod) GlobalAddrInst(Loc, nullptr, Type);
    return Entry;
  }

  // If we have multiple results, track them through ForwardMRVLocalValues.
  std::vector<SILValue> &Placeholders = ForwardMRVLocalValues[Name.Name];
  if (Placeholders.size() <= Name.ResultVal)
    Placeholders.resize(Name.ResultVal+1);

  if (!Placeholders[Name.ResultVal])
    Placeholders[Name.ResultVal] =
      new (SILMod) GlobalAddrInst(Loc, nullptr, Type);
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
///     'thunk'
static bool parseSILLinkage(SILLinkage &Result, Parser &P) {
  if (P.Tok.isNot(tok::identifier)) {
    Result = SILLinkage::External;
  } else if (P.Tok.getText() == "internal") {
    Result = SILLinkage::Internal;
    P.consumeToken(tok::identifier);
  } else if (P.Tok.getText() == "thunk") {
    Result = SILLinkage::Thunk;
    P.consumeToken(tok::identifier);
  } else if (P.Tok.getText() == "deserialized") {
    Result = SILLinkage::Deserialized;
    P.consumeToken(tok::identifier);
  } else {
    P.diagnose(P.Tok, diag::expected_sil_linkage_or_function);
    return true;
  }
  return false;
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

/// Construct ArchetypeType from Generic Params.
bool SILParser::handleGenericParams(GenericParamList *GenericParams) {
  ArchetypeBuilder Builder(P.SF.TU, P.Diags);
  unsigned Index = 0;
  for (auto GP : *GenericParams) {
    auto TypeParam = GP.getAsTypeParam();
    // Do some type checking / name binding for Inherited.
    // FIXME: This is a hack. We shouldn't ever have to build these.
    SmallVector<ProtocolDecl *, 4> allProtocols;
    llvm::SmallPtrSet<ProtocolDecl *, 4> knownProtocols;
    for (auto &Inherited : TypeParam->getInherited()) {
      if (performTypeLocChecking(Inherited))
        return true;

      // Collect the protocols mentioned by this existential type.
      SmallVector<ProtocolDecl *, 4> protocols;
      if (Inherited.getType()->isExistentialType(protocols)) {
        for (auto proto : protocols) {
          if (knownProtocols.insert(proto))
            allProtocols.push_back(proto);
        }
      }
      // Set the superclass.
      else if (Inherited.getType()->getClassOrBoundGenericClass()) {
        TypeParam->setSuperclass(Inherited.getType());
      }
    }

    // Set this list of all protocols.
    TypeParam->setProtocols(P.Context.AllocateCopy(allProtocols));

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
    TypeParam->setArchetype(Builder.getArchetype(TypeParam));
  }
  GenericParams->setAllArchetypes(
    P.Context.AllocateCopy(Builder.getAllArchetypes()));
  return false;
}

bool SILParser::performTypeLocChecking(TypeLoc &T) {
  // Do some type checking / name binding for the parsed type.
  assert(P.SF.ASTStage == SourceFile::Parsing &&
         "Unexpected stage during parsing!");
  return swift::performTypeLocChecking(P.Context, T, /*SIL*/ true, &P.SF);
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
static ValueDecl *lookupMember(Parser &P, Type Ty, Identifier Name) {
  SmallVector<ValueDecl *, 4> Lookup;
  P.SF.TU.lookupQualified(Ty, Name, NL_QualifiedDefault, nullptr, Lookup);
  assert(Lookup.size() == 1);
  return Lookup[0];
}

///   sil-type:
///     '$' '*'? attribute-list (generic-params)? type
///
bool SILParser::parseSILType(SILType &Result) {
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

  // Handle @local_storage, which changes the SIL value category.
  if (attrs.has(TAK_local_storage)) {
    // Require '*' on local_storage values.
    if (category != SILValueCategory::Address)
      P.diagnose(attrs.getLoc(TAK_local_storage),
                 diag::sil_local_storage_non_address);
    category = SILValueCategory::LocalStorage;
    attrs.clearAttribute(TAK_local_storage);
  }

  // Parse Generic Parameters. Generic Parameters are visible in the function
  // body.
  GenericParamList *PList = P.maybeParseGenericParams();

  ParserResult<TypeRepr> TyR = P.parseType(diag::expected_sil_type);
  if (TyR.isNull())
    return true;
  
  // Apply attributes to the type.
  TypeLoc Ty = P.applyAttributeToType(TyR.get(), attrs);

  if (PList)
    if (handleGenericParams(PList))
      return true;
  if (performTypeLocChecking(Ty))
    return true;

  // Build PolymorphicFunctionType if necessary.
  FunctionType *FT = dyn_cast<FunctionType>(Ty.getType().getPointer());
  if (FT && PList) {
    auto Info = PolymorphicFunctionType::ExtInfo(attrs.hasCC()
                                                   ? attrs.getAbstractCC()
                                                   : AbstractCC::Freestanding,
                                                 attrs.has(TAK_thin),
                                                 attrs.has(TAK_noreturn));
    Type resultType = PolymorphicFunctionType::get(FT->getInput(),
                                             FT->getResult(), PList,
                                             Info,
                                             P.Context);
    Ty.setType(resultType);
    // Reset attributes that are applied.
    attrs.clearAttribute(TAK_thin);
    attrs.clearAttribute(TAK_cc);
    attrs.cc = Nothing;
  }

  if (performTypeLocChecking(Ty))
    return true;

  Result = SILType::getPrimitiveType(Ty.getType()->getCanonicalType(),
                                     category);
  return false;
}

///  sil-decl-ref ::= '#' sil-identifier ('.' sil-identifier)* sil-decl-subref?
///  sil-decl-subref ::= '!' sil-decl-subref-part ('.' sil-decl-uncurry-level)?
///                      ('.' sil-decl-lang)?
///  sil-decl-subref ::= '!' sil-decl-uncurry-level ('.' sil-decl-lang)?
///  sil-decl-subref ::= '!' sil-decl-lang
///  sil-decl-subref-part ::= 'getter'
///  sil-decl-subref-part ::= 'setter'
///  sil-decl-subref-part ::= 'allocator'
///  sil-decl-subref-part ::= 'initializer'
///  sil-decl-subref-part ::= 'enumelt'
///  sil-decl-subref-part ::= 'destroyer'
///  sil-decl-subref-part ::= 'globalaccessor'
///  sil-decl-uncurry-level ::= [0-9]+
///  sil-decl-lang ::= 'foreign'
bool SILParser::parseSILDeclRef(SILDeclRef &Result) {
  if (P.parseToken(tok::sil_pound, diag::expected_sil_constant))
    return true;

  // Handle sil-dotted-path.
  Identifier Id;
  SmallVector<Identifier, 4> FullName;
  do {
    if (parseSILIdentifier(Id, diag::expected_sil_constant))
      return true;
    FullName.push_back(Id);
  } while (P.consumeIf(tok::period));

  // Look up ValueDecl from a dotted path.
  ValueDecl *VD;
  llvm::PointerUnion<ValueDecl*, Module *> Res = lookupTopDecl(P, FullName[0]);
  if (Res.is<Module*>()) {
    assert(FullName.size() > 1 &&
           "A single module is not a full path to SILDeclRef");
    auto Mod = Res.get<Module*>();
    VD = lookupMember(P, ModuleType::get(Mod), FullName[1]);
    for (unsigned I = 2, E = FullName.size(); I < E; I++)
      VD = lookupMember(P, VD->getType(), FullName[I]);
  } else {
    VD = Res.get<ValueDecl*>();
    for (unsigned I = 1, E = FullName.size(); I < E; I++)
      VD = lookupMember(P, VD->getType(), FullName[I]);
  }

  // Initialize Kind, uncurryLevel and IsObjC.
  SILDeclRef::Kind Kind = SILDeclRef::Kind::Func;
  unsigned uncurryLevel = 0;
  bool IsObjC = false;

  if (!P.consumeIf(tok::sil_exclamation)) {
    // Construct SILDeclRef.
    Result = SILDeclRef(VD, Kind, uncurryLevel, IsObjC);
    return false;
  }

  // Handle sil-constant-kind-and-uncurry-level.
  // ParseState indicates the value we just handled.
  // 1 means we just handled Kind, 2 means we just handled uncurryLevel.
  // We accept func|getter|setter|...|foreign or an integer when ParseState is 0;
  // accept foreign or an integer when ParseState is 1; accept foreign when ParseState
  // is 2.
  unsigned ParseState = 0;
  do {
    if (P.Tok.is(tok::identifier)) {
      if (parseSILIdentifier(Id, diag::expected_sil_constant))
        return true;
      if (!ParseState && Id.str() == "func") {
        Kind = SILDeclRef::Kind::Func;
        ParseState = 1;
      } else if (!ParseState && Id.str() == "getter") {
        Kind = SILDeclRef::Kind::Getter;
        ParseState = 1;
      } else if (!ParseState && Id.str() == "setter") {
        Kind = SILDeclRef::Kind::Setter;
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
      } else if (!ParseState && Id.str() == "globalaccessor") {
        Kind = SILDeclRef::Kind::GlobalAccessor;
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
      break;

  } while (P.consumeIf(tok::period));

  // Construct SILDeclRef.
  Result = SILDeclRef(VD, Kind, uncurryLevel, IsObjC);
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
    .Case("alloc_array", ValueKind::AllocArrayInst)
    .Case("address_to_pointer", ValueKind::AddressToPointerInst)
    .Case("alloc_stack", ValueKind::AllocStackInst)
    .Case("alloc_ref", ValueKind::AllocRefInst)
    .Case("archetype_metatype", ValueKind::ArchetypeMetatypeInst)
    .Case("archetype_method", ValueKind::ArchetypeMethodInst)
    .Case("archetype_ref_to_super", ValueKind::ArchetypeRefToSuperInst)
    .Case("apply", ValueKind::ApplyInst)
    .Case("assign", ValueKind::AssignInst)
    .Case("autorelease_return", ValueKind::AutoreleaseReturnInst)
    .Case("br", ValueKind::BranchInst)
    .Case("bridge_to_block", ValueKind::BridgeToBlockInst)
    .Case("builtin_function_ref", ValueKind::BuiltinFunctionRefInst)
    .Case("builtin_zero", ValueKind::BuiltinZeroInst)
    .Case("checked_cast_br", ValueKind::CheckedCastBranchInst)
    .Case("class_metatype", ValueKind::ClassMetatypeInst)
    .Case("class_method", ValueKind::ClassMethodInst)
    .Case("coerce", ValueKind::CoerceInst)
    .Case("cond_br", ValueKind::CondBranchInst)
    .Case("cond_fail", ValueKind::CondFailInst)
    .Case("convert_function", ValueKind::ConvertFunctionInst)
    .Case("copy_addr", ValueKind::CopyAddrInst)
    .Case("copy_value", ValueKind::CopyValueInst)
    .Case("dealloc_box", ValueKind::DeallocBoxInst)
    .Case("dealloc_ref", ValueKind::DeallocRefInst)
    .Case("dealloc_stack", ValueKind::DeallocStackInst)
    .Case("deinit_existential", ValueKind::DeinitExistentialInst)
    .Case("destroy_addr", ValueKind::DestroyAddrInst)
    .Case("destroy_value", ValueKind::DestroyValueInst)
    .Case("destructive_switch_enum_addr",
          ValueKind::DestructiveSwitchEnumAddrInst)
    .Case("dynamic_method", ValueKind::DynamicMethodInst)
    .Case("dynamic_method_br", ValueKind::DynamicMethodBranchInst)
    .Case("enum", ValueKind::EnumInst)
    .Case("enum_data_addr", ValueKind::EnumDataAddrInst)
    .Case("float_literal", ValueKind::FloatLiteralInst)
    .Case("global_addr", ValueKind::GlobalAddrInst)
    .Case("index_addr", ValueKind::IndexAddrInst)
    .Case("index_raw_pointer", ValueKind::IndexRawPointerInst)
    .Case("init_existential", ValueKind::InitExistentialInst)
    .Case("init_existential_ref", ValueKind::InitExistentialRefInst)
    .Case("initialize_var", ValueKind::InitializeVarInst)
    .Case("inject_enum_addr", ValueKind::InjectEnumAddrInst)
    .Case("integer_literal", ValueKind::IntegerLiteralInst)
    .Case("is_nonnull", ValueKind::IsNonnullInst)
    .Case("function_ref", ValueKind::FunctionRefInst)
    .Case("load", ValueKind::LoadInst)
    .Case("load_weak", ValueKind::LoadWeakInst)
    .Case("mark_uninitialized", ValueKind::MarkUninitializedInst)
    .Case("mark_function_escape", ValueKind::MarkFunctionEscapeInst)
    .Case("metatype", ValueKind::MetatypeInst)
    .Case("module", ValueKind::ModuleInst)
    .Case("object_pointer_to_ref", ValueKind::ObjectPointerToRefInst)
    .Case("partial_apply", ValueKind::PartialApplyInst)
    .Case("pointer_to_address", ValueKind::PointerToAddressInst)
    .Case("project_existential", ValueKind::ProjectExistentialInst)
    .Case("project_existential_ref", ValueKind::ProjectExistentialRefInst)
    .Case("protocol_metatype", ValueKind::ProtocolMetatypeInst)
    .Case("protocol_method", ValueKind::ProtocolMethodInst)
    .Case("raw_pointer_to_ref", ValueKind::RawPointerToRefInst)
    .Case("ref_element_addr", ValueKind::RefElementAddrInst)
    .Case("ref_to_object_pointer", ValueKind::RefToObjectPointerInst)
    .Case("ref_to_raw_pointer", ValueKind::RefToRawPointerInst)
    .Case("ref_to_unowned", ValueKind::RefToUnownedInst)
    .Case("strong_release", ValueKind::StrongReleaseInst)
    .Case("strong_retain", ValueKind::StrongRetainInst)
    .Case("strong_retain_autoreleased", ValueKind::StrongRetainAutoreleasedInst)
    .Case("strong_retain_unowned", ValueKind::StrongRetainUnownedInst)
    .Case("return", ValueKind::ReturnInst)
    .Case("store", ValueKind::StoreInst)
    .Case("store_weak", ValueKind::StoreWeakInst)
    .Case("string_literal", ValueKind::StringLiteralInst)
    .Case("struct", ValueKind::StructInst)
    .Case("struct_element_addr", ValueKind::StructElementAddrInst)
    .Case("struct_extract", ValueKind::StructExtractInst)
    .Case("super_method", ValueKind::SuperMethodInst)
    .Case("switch_int", ValueKind::SwitchIntInst)
    .Case("switch_enum", ValueKind::SwitchEnumInst)
    .Case("thin_to_thick_function", ValueKind::ThinToThickFunctionInst)
    .Case("tuple", ValueKind::TupleInst)
    .Case("tuple_element_addr", ValueKind::TupleElementAddrInst)
    .Case("tuple_extract", ValueKind::TupleExtractInst)
    .Case("unconditional_checked_cast", ValueKind::UnconditionalCheckedCastInst)
    .Case("unowned_retain", ValueKind::UnownedRetainInst)
    .Case("unowned_release", ValueKind::UnownedReleaseInst)
    .Case("unowned_to_ref", ValueKind::UnownedToRefInst)
    .Case("unreachable", ValueKind::UnreachableInst)
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

namespace {
struct ParsedSubstitution {
  SourceLoc loc;
  Identifier name;
  SILType replacement;
};
}

/// Parse the substitution list for an apply instruction.
bool parseApplySubstitutions(SILParser &SP,
                             SmallVectorImpl<ParsedSubstitution> &parsed) {
  // Check for an opening '<' bracket.
  if (!SP.P.Tok.isContextualPunctuator("<"))
    return false;
  
  SP.P.consumeToken();
  
  // Parse a list of Substitutions: Archetype = Replacement.
  do {
    SourceLoc Loc = SP.P.Tok.getLoc();
    Substitution Sub;
    SILType Replace;
    Identifier ArcheId;
    if (SP.parseSILIdentifier(ArcheId, diag::expected_sil_type) ||
        SP.P.parseToken(tok::equal, diag::expected_tok_in_sil_instr, "=") ||
        SP.parseSILType(Replace))
      return true;
    
    parsed.push_back({Loc, ArcheId, Replace});
  } while (SP.P.consumeIf(tok::comma));
  
  // Consume the closing '>'.
  if (!SP.P.Tok.isContextualPunctuator(">")) {
    SP.P.diagnose(SP.P.Tok, diag::expected_tok_in_sil_instr, ">");
    return true;
  }
  
  return false;
}

/// Reconstruct AST substitutions from parsed substitutions using archetypes
/// from a PolymorphicFunctionType.
bool getApplySubstitutionsFromParsed(
                             SILParser &SP,
                             PolymorphicFunctionType *pft,
                             ArrayRef<ParsedSubstitution> parses,
                             SmallVectorImpl<Substitution> &subs) {
  // Find the corresponding ArchetypeType for ArcheId in PTy.
  ArrayRef<ArchetypeType *> allArchetypes = pft->getAllArchetypes();
  for (auto &parsed : parses) {
    Substitution sub{nullptr, nullptr, nullptr};
    for (auto archetype : allArchetypes)
      if (archetype->getName() == parsed.name) {
        sub.Archetype = archetype;
        break;
      }
    if (!sub.Archetype) {
      SP.P.diagnose(parsed.loc, diag::sil_apply_archetype_not_found);
      return true;
    }
    sub.Replacement = parsed.replacement.getSwiftType();
    subs.push_back(sub);
  }
  return false;
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

  /// Parse a checked cast kind.
  auto parseCastKind = [&](Identifier name, SourceLoc loc) -> CheckedCastKind {
    auto kind = llvm::StringSwitch<CheckedCastKind>(name.str())
      .Case("downcast", CheckedCastKind::Downcast)
      .Case("super_to_archetype", CheckedCastKind::SuperToArchetype)
      .Case("archetype_to_archetype", CheckedCastKind::ArchetypeToArchetype)
      .Case("archetype_to_concrete", CheckedCastKind::ArchetypeToConcrete)
      .Case("existential_to_archetype", CheckedCastKind::ExistentialToArchetype)
      .Case("existential_to_concrete", CheckedCastKind::ExistentialToConcrete)
      .Default(CheckedCastKind::Unresolved);
    
    if (kind == CheckedCastKind::Unresolved)
      P.diagnose(loc, diag::expected_tok_in_sil_instr, "checked cast kind");
    return kind;
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
  case ValueKind::AllocArrayInst: {
    SILType Ty;
    if (parseSILType(Ty) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseTypedValueRef(Val))
      return true;
    ResultVal = B.createAllocArray(InstLoc, Ty, Val);
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
    
    APInt value(intTy->getBitWidth(), 0);
    bool error = P.Tok.getText().getAsInteger(0, value);
    assert(!error && "integer_literal token did not parse as APInt?!");
    (void)error;
    
    if (Negative)
      value = -value; 
    if (value.getBitWidth() != intTy->getBitWidth())
      value = value.zextOrTrunc(intTy->getBitWidth());
    
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
    if (P.Tok.getKind() != tok::string_literal) {
      P.diagnose(P.Tok, diag::expected_tok_in_sil_instr, "string");
      return true;
    }
   
    // We should remove '"' from token.
    StringRef Str = P.Tok.getText();
    if (Str.size() < 2 || Str[0] != '"' || Str[Str.size()-1] != '"') {
      P.diagnose(P.Tok, diag::expected_tok_in_sil_instr, "string");
      return true;
    }
    ResultVal = B.createStringLiteral(InstLoc, Str.substr(1, Str.size()-2));
    P.consumeToken(tok::string_literal);
    break;
  }
      
  case ValueKind::FunctionRefInst:
    if (parseSILFunctionRef(InstLoc, B, ResultVal))
      return true;
    break;
  case ValueKind::BuiltinFunctionRefInst: {
    SILType Ty;
    SILDeclRef FuncRef;
    if (parseSILDeclRef(FuncRef) ||
        P.parseToken(tok::colon, diag::expected_tok_in_sil_instr, ":") ||
        parseSILType(Ty))
      return true;
    ResultVal = B.createBuiltinFunctionRef(InstLoc,
                      cast<FuncDecl>(FuncRef.getDecl()), Ty);
    break;
  }
  case ValueKind::ProjectExistentialInst:
  case ValueKind::ProjectExistentialRefInst: {
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

    if (Opcode == ValueKind::ProjectExistentialInst)
      ResultVal = B.createProjectExistential(InstLoc, Val, Ty);
    else
      ResultVal = B.createProjectExistentialRef(InstLoc, Val, Ty);
    break;
  }
#define UNARY_INSTRUCTION_HELPER(ID, CREATOR) \
  case ValueKind::ID##Inst:                   \
    if (parseTypedValueRef(Val)) return true; \
    ResultVal = B.CREATOR(InstLoc, Val);      \
    break;
#define UNARY_INSTRUCTION(ID) UNARY_INSTRUCTION_HELPER(ID, create##ID)
  UNARY_INSTRUCTION_HELPER(StrongRetain, createStrongRetainInst)
  UNARY_INSTRUCTION_HELPER(StrongRelease, createStrongReleaseInst)
  UNARY_INSTRUCTION(StrongRetainAutoreleased)
  UNARY_INSTRUCTION(AutoreleaseReturn)
  UNARY_INSTRUCTION(StrongRetainUnowned)
  UNARY_INSTRUCTION(UnownedRetain)
  UNARY_INSTRUCTION(UnownedRelease)
  UNARY_INSTRUCTION(DestroyAddr)
  UNARY_INSTRUCTION(DestroyValue)
  UNARY_INSTRUCTION(CopyValue)
  UNARY_INSTRUCTION(Load)
  UNARY_INSTRUCTION(CondFail)
#undef UNARY_INSTRUCTION
#undef UNARY_INSTRUCTION_HELPER

  case ValueKind::LoadWeakInst: {
    bool isTake = false;
    if (parseSILOptional(isTake, *this, "take") ||
        parseTypedValueRef(Val))
      return true;

    ResultVal = B.createLoadWeak(InstLoc, Val, IsTake_t(isTake));
    break;
  }
      
    // Conversion instructions.
  case ValueKind::RefToObjectPointerInst:
  case ValueKind::UpcastInst:
  case ValueKind::CoerceInst:
  case ValueKind::AddressToPointerInst:
  case ValueKind::PointerToAddressInst:
  case ValueKind::ObjectPointerToRefInst:
  case ValueKind::RefToRawPointerInst:
  case ValueKind::RawPointerToRefInst:
  case ValueKind::RefToUnownedInst:
  case ValueKind::UnownedToRefInst:
  case ValueKind::ThinToThickFunctionInst:
  case ValueKind::BridgeToBlockInst:
  case ValueKind::ArchetypeRefToSuperInst:
  case ValueKind::ConvertFunctionInst:
  case ValueKind::UpcastExistentialRefInst: {
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
    default: assert(0 && "Out of sync with parent switch");
    case ValueKind::RefToObjectPointerInst:
      ResultVal = B.createRefToObjectPointer(InstLoc, Val, Ty);
      break;
    case ValueKind::UpcastInst:
      ResultVal = B.createUpcast(InstLoc, Val, Ty);
      break;
    case ValueKind::ConvertFunctionInst:
      ResultVal = B.createConvertFunction(InstLoc, Val, Ty);
      break;
    case ValueKind::CoerceInst:
      ResultVal = B.createCoerce(InstLoc, Val, Ty);
      break;
    case ValueKind::AddressToPointerInst:
      ResultVal = B.createAddressToPointer(InstLoc, Val, Ty);
      break;
    case ValueKind::PointerToAddressInst:
      ResultVal = B.createPointerToAddress(InstLoc, Val, Ty);
      break;
    case ValueKind::ObjectPointerToRefInst:
      ResultVal = B.createObjectPointerToRef(InstLoc, Val, Ty);
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
    case ValueKind::ThinToThickFunctionInst:
      ResultVal = B.createThinToThickFunction(InstLoc, Val, Ty);
      break;
    case ValueKind::BridgeToBlockInst:
      ResultVal = B.createBridgeToBlock(InstLoc, Val, Ty);
      break;
    case ValueKind::ArchetypeRefToSuperInst:
      ResultVal = B.createArchetypeRefToSuper(InstLoc, Val, Ty);
      break;
    case ValueKind::UpcastExistentialRefInst:
      ResultVal = B.createUpcastExistentialRef(InstLoc, Val, Ty);
      break;
    }
    break;
  }
  // Checked Conversion instructions.
  case ValueKind::UnconditionalCheckedCastInst:
  case ValueKind::CheckedCastBranchInst: {
    SILType Ty;
    Identifier KindToken, ToToken;
    SourceLoc KindLoc, ToLoc;
    
    if (parseSILIdentifier(KindToken, KindLoc,
                           diag::expected_tok_in_sil_instr, "checked cast kind")
        || parseTypedValueRef(Val)
        || parseSILIdentifier(ToToken, ToLoc,
                              diag::expected_tok_in_sil_instr, "to")
        || parseSILType(Ty))
      return true;
    
    CheckedCastKind CastKind = parseCastKind(KindToken, KindLoc);
    if (CastKind == CheckedCastKind::Unresolved)
      return true;
    
    // An unconditional cast instruction is finished here.
    if (Opcode == ValueKind::UnconditionalCheckedCastInst) {
      ResultVal = B.createUnconditionalCheckedCast(InstLoc, CastKind, Val, Ty);
      break;
    }
    
    // The conditional cast still needs its branch destinations.
    Identifier SuccessBBName, FailureBBName;
    SourceLoc SuccessBBLoc, FailureBBLoc;
    if (P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",")
        || parseSILIdentifier(SuccessBBName, SuccessBBLoc,
                              diag::expected_sil_block_name)
        || P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",")
        || parseSILIdentifier(FailureBBName, FailureBBLoc,
                              diag::expected_sil_block_name))
      return true;
    
    ResultVal = B.createCheckedCastBranch(InstLoc, CastKind, Val, Ty,
                                getBBForReference(SuccessBBName, SuccessBBLoc),
                                getBBForReference(FailureBBName, FailureBBLoc));
    break;
  }

  case ValueKind::MarkUninitializedInst:
    if (parseTypedValueRef(Val)) return true;
    ResultVal = B.createMarkUninitialized(InstLoc, Val);
    break;
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
    SILType Ty;
    if (parseSILType(Ty))
      return true;
    
    if (Opcode == ValueKind::AllocStackInst)
      ResultVal = B.createAllocStack(InstLoc, Ty);
    else if (Opcode == ValueKind::AllocRefInst) {
      ResultVal = B.createAllocRef(InstLoc, Ty);
    } else {
      assert(Opcode == ValueKind::MetatypeInst);
      ResultVal = B.createMetatype(InstLoc, Ty);
    }
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
  case ValueKind::ArchetypeMetatypeInst:
  case ValueKind::ClassMetatypeInst:
  case ValueKind::ProtocolMetatypeInst: {
    SILType Ty;
    if (parseSILType(Ty) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseTypedValueRef(Val))
      return true;
    switch (Opcode) {
    default: assert(0 && "Out of sync with parent switch");
    case ValueKind::ArchetypeMetatypeInst:
      ResultVal = B.createArchetypeMetatype(InstLoc, Ty, Val);
      break;
    case ValueKind::ClassMetatypeInst:
      ResultVal = B.createClassMetatype(InstLoc, Ty, Val);
      break;
    case ValueKind::ProtocolMetatypeInst:
      ResultVal = B.createProtocolMetatype(InstLoc, Ty, Val);
      break;
    case ValueKind::DeallocBoxInst:
      ResultVal = B.createDeallocBox(InstLoc, Ty, Val);
      break;
    }
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
  case ValueKind::EnumDataAddrInst: {
    SILValue Operand;
    SILDeclRef EltRef;
    if (parseTypedValueRef(Operand) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseSILDeclRef(EltRef))
      return true;
    
    EnumElementDecl *Elt = cast<EnumElementDecl>(EltRef.getDecl());
    
    auto OperandTy = Operand.getType().getSwiftRValueType();
    auto ResultTy = OperandTy->getTypeOfMember(Elt->getModuleContext(),
                                               Elt,
                                               nullptr,
                                               Elt->getArgumentType())
      ->getCanonicalType();
    ResultVal = B.createEnumDataAddr(InstLoc, Operand, Elt,
                                    SILType::getPrimitiveAddressType(ResultTy));
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
    
  case ValueKind::ProtocolMethodInst:
  case ValueKind::ClassMethodInst:
  case ValueKind::SuperMethodInst:
  case ValueKind::DynamicMethodInst: {
    bool IsVolatile = false;
    if (parseSILOptional(IsVolatile, *this, "volatile"))
      return true;
    SILDeclRef Member;
    SILType MethodTy;
    SourceLoc TyLoc;
    if (parseTypedValueRef(Val) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseSILDeclRef(Member) ||
        P.parseToken(tok::colon, diag::expected_tok_in_sil_instr, ":") ||
        parseSILType(MethodTy, TyLoc)
       )
      return true;
    switch (Opcode) {
    default: assert(0 && "Out of sync with parent switch");
    case ValueKind::ProtocolMethodInst:
      ResultVal = B.createProtocolMethod(InstLoc, Val, Member, MethodTy,
                                         IsVolatile);
      break;
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
  case ValueKind::ArchetypeMethodInst: {
    bool IsVolatile = false;
    if (parseSILOptional(IsVolatile, *this, "volatile"))
      return true;
    SILType LookupTy;
    SILDeclRef Member;
    SILType MethodTy;
    SourceLoc TyLoc;
    if (parseSILType(LookupTy, TyLoc) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseSILDeclRef(Member) ||
        P.parseToken(tok::colon, diag::expected_tok_in_sil_instr, ":") ||
        parseSILType(MethodTy, TyLoc)
       )
      return true;
    ResultVal = B.createArchetypeMethod(InstLoc, LookupTy, Member,
                                        MethodTy, IsVolatile);
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
  case ValueKind::UpcastExistentialInst: {
    SILValue DestVal;
    SourceLoc SrcLoc, DestLoc, ToLoc;
    Identifier ToToken;
    bool IsTake = false;
    if (parseSILOptional(IsTake, *this, "take") ||
        parseTypedValueRef(Val, SrcLoc) ||
        parseSILIdentifier(ToToken, ToLoc,
                           diag::expected_tok_in_sil_instr, "to") ||
        parseTypedValueRef(DestVal, DestLoc))
      return true;

    if (ToToken.str() != "to") {
      P.diagnose(ToLoc, diag::expected_tok_in_sil_instr, "to");
      return true;
    }
    ResultVal = B.createUpcastExistential(InstLoc, Val, DestVal,
                                          IsTake_t(IsTake));
    break;
  }
  case ValueKind::InitializeVarInst: {
    bool NoDefault = false;
    if (parseSILOptional(NoDefault, *this, "no_default_construct") ||
        parseTypedValueRef(Val))
      return true;
    ResultVal = B.createInitializeVar(InstLoc, Val, !NoDefault);
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
    Identifier ElemId;
    SourceLoc NameLoc;
    if (parseTypedValueRef(Val) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        P.parseToken(tok::sil_pound, diag::expected_sil_constant) ||
        parseSILIdentifier(ElemId, NameLoc, diag::expected_sil_constant))
      return true;
    ValueDecl *FieldV = lookupMember(P, Val.getType().getSwiftRValueType(),
                                     ElemId);
    if (!FieldV || !isa<VarDecl>(FieldV)) {
      P.diagnose(NameLoc, diag::sil_struct_inst_wrong_field);
      return true;
    }
    VarDecl *Field = cast<VarDecl>(FieldV);

    // FIXME: substitution means this type should be explicit to improve
    // performance.
    auto ResultTy = Val.getType().getSwiftRValueType()
      ->getTypeOfMember(Field->getModuleContext(),
                        Field, nullptr)->getCanonicalType();
    if (Opcode == ValueKind::StructElementAddrInst)
      ResultVal = B.createStructElementAddr(InstLoc, Val, Field,
                                 SILType::getPrimitiveAddressType(ResultTy));
    else
      ResultVal = B.createStructExtract(InstLoc, Val, Field,
                                  SILType::getPrimitiveObjectType(ResultTy));
    break;
  }
  case ValueKind::BuiltinZeroInst: {
    SILType Ty;
    if (parseSILType(Ty))
      return true;
    ResultVal = B.createBuiltinZero(InstLoc, Ty);
    break;
  }
  case ValueKind::RefElementAddrInst: {
    Identifier ElemId;
    SourceLoc NameLoc;
    if (parseTypedValueRef(Val) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        P.parseToken(tok::sil_pound, diag::expected_sil_constant) ||
        parseSILIdentifier(ElemId, NameLoc, diag::expected_sil_constant))
      return true;
    ValueDecl *FieldV = lookupMember(P, Val.getType().getSwiftRValueType(),
                                     ElemId);
    if (!FieldV || !isa<VarDecl>(FieldV)) {
      P.diagnose(NameLoc, diag::sil_ref_inst_wrong_field);
      return true;
    }
    VarDecl *Field = cast<VarDecl>(FieldV);
    CanType FieldTy = Val.getType().getSwiftRValueType()
      ->getTypeOfMember(Field->getModuleContext(), Field, nullptr)
      ->getCanonicalType();
    auto ResultTy = SILType::getPrimitiveAddressType(FieldTy);
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
  case ValueKind::GlobalAddrInst: {
    Identifier GlobalName;
    SILType Ty;
    if (P.parseToken(tok::sil_pound, diag::expected_sil_constant) ||
        parseSILIdentifier(GlobalName, diag::expected_sil_constant) ||
        P.parseToken(tok::colon, diag::expected_tok_in_sil_instr, ":") ||
        parseSILType(Ty))
      return true;
    // Find VarDecl for GlobalName.
    ValueDecl *VD;
    SmallVector<ValueDecl*, 4> CurModuleResults;
    // Perform a module level lookup on the first component of the
    // fully-qualified name.
    P.SF.TU.lookupValue(Module::AccessPathTy(), GlobalName,
                        NLKind::UnqualifiedLookup, CurModuleResults);
    assert(CurModuleResults.size() == 1);
    VD = CurModuleResults[0];
    ResultVal = B.createGlobalAddr(InstLoc, cast<VarDecl>(VD), Ty);
    break;
  }
  case ValueKind::SwitchEnumInst:
  case ValueKind::DestructiveSwitchEnumAddrInst: {
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
      ResultVal = B.createDestructiveSwitchEnumAddr(
                                             InstLoc, Val, DefaultBB, CaseBBs);
    break;
  }
  case ValueKind::SwitchIntInst: {
    if (parseTypedValueRef(Val))
      return true;

    SmallVector<std::pair<APInt, SILBasicBlock*>, 4> CaseBBs;
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

      // Parse 'case' int-literal ':' sil-identifier.
      if (P.consumeIf(tok::kw_case)) {
        // Parse int-literal.
        if (P.Tok.getKind() != tok::integer_literal) {
          P.diagnose(P.Tok, diag::expected_tok_in_sil_instr, "integer");
          return true;
        }
        auto intTy = Val.getType().getAs<BuiltinIntegerType>();
        if (!intTy) {
          P.diagnose(P.Tok, diag::sil_integer_literal_not_integer_type);
          return true;
        }

        APInt value(intTy->getBitWidth(), 0);
        bool error = P.Tok.getText().getAsInteger(0, value);
        assert(!error && "integer_literal token did not parse as APInt?!");
        (void)error;

        if (value.getBitWidth() != intTy->getBitWidth())
          value = value.zextOrTrunc(intTy->getBitWidth());
        P.consumeToken(tok::integer_literal);

        P.parseToken(tok::colon, diag::expected_tok_in_sil_instr, ":");
        parseSILIdentifier(BBName, BBLoc, diag::expected_sil_block_name);
        CaseBBs.push_back( {value, getBBForReference(BBName, BBLoc)} );
        continue;
      }

      P.diagnose(P.Tok, diag::expected_tok_in_sil_instr, "case or default");
      return true;
    }
    ResultVal = B.createSwitchInt(InstLoc, Val, DefaultBB, CaseBBs);
    break;
  }
  case ValueKind::DeinitExistentialInst: {
    if (parseTypedValueRef(Val))
      return true;
    ResultVal = B.createDeinitExistential(InstLoc, Val);
    break;
  }
  case ValueKind::InitExistentialInst: {
    SILType Ty;
    if (parseTypedValueRef(Val) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseSILType(Ty))
      return true;
    // FIXME: Conformances in InitExistentialInst is currently not included in
    // SIL.rst.
    ResultVal = B.createInitExistential(InstLoc, Val, Ty,
                  ArrayRef<ProtocolConformance*>());
    break;
  }
  case ValueKind::InitExistentialRefInst: {
    SILType Ty;
    if (parseTypedValueRef(Val) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        parseSILType(Ty))
      return true;
    // FIXME: Conformances in InitExistentialRefInst is currently not included
    // in SIL.rst.
    ResultVal = B.createInitExistentialRef(InstLoc, Ty, Val,
                  ArrayRef<ProtocolConformance*>());
    break;
  }
  case ValueKind::ModuleInst: {
    Identifier ModuleName;
    SourceLoc ModuleLoc;
    // Parse reference to a module.
    if (P.parseToken(tok::sil_pound, diag::expected_sil_constant) ||
        parseSILIdentifier(ModuleName, ModuleLoc, diag::expected_sil_constant))
      return true;
    llvm::PointerUnion<ValueDecl*, Module *> Res = lookupTopDecl(P, ModuleName);
    assert(Res.is<Module*>() && "Expect a module name in ModuleInst");
    auto Mod = Res.get<Module*>();
    ResultVal = B.createModule(InstLoc,
                  SILType::getPrimitiveObjectType(
                    ModuleType::get(Mod)->getCanonicalType()));
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

  bool Transparent = false;
  if ((Opcode == ValueKind::ApplyInst &&
       parseSILOptional(Transparent, *this, "transparent")) ||
      parseValueName(FnName))
    return true;
  SmallVector<ParsedSubstitution, 4> parsedSubs;
  if (parseApplySubstitutions(*this, parsedSubs))
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
  if (P.parseToken(tok::r_paren, diag::expected_tok_in_sil_instr, ")") ||
      P.parseToken(tok::colon, diag::expected_tok_in_sil_instr, ":") ||
      parseSILType(Ty, TypeLoc))
    return true;

  CanType ShTy = Ty.getSwiftType();
  if (!ShTy->is<FunctionType>() && !ShTy->is<PolymorphicFunctionType>()) {
    P.diagnose(TypeLoc, diag::expected_sil_type_kind, "be a function");
    return true;
  }

  SILFunctionType *FTI = Ty.getFunctionTypeInfo(SILMod);
  
  auto ArgTys = FTI->getParameterSILTypes();
  
  SmallVector<Substitution, 4> subs;
  if (!parsedSubs.empty()) {
    PolymorphicFunctionType *pft = Ty.getAs<PolymorphicFunctionType>();
    if (!pft) {
      P.diagnose(TypeLoc, diag::sil_substitutions_on_non_polymorphic_type);
      return true;
    }
    if (getApplySubstitutionsFromParsed(*this, pft, parsedSubs, subs))
      return true;
  }
  switch (Opcode) {
  default: assert(0 && "Unexpected case");
  case ValueKind::ApplyInst : {
    if (ArgTys.size() != ArgNames.size()) {
      P.diagnose(TypeLoc, diag::expected_sil_type_kind,
                 "have the right argument types");
      return true;
    }

    SILValue FnVal = getLocalValue(FnName, Ty, InstLoc);
    unsigned ArgNo = 0;
    SmallVector<SILValue, 4> Args;
    for (auto &ArgName : ArgNames)
      Args.push_back(getLocalValue(ArgName, ArgTys[ArgNo++], InstLoc));

    SILType FnTy = FnVal.getType();
    if (!subs.empty()) {
      FnTy = SILType::getPrimitiveObjectType(
         FnTy.castTo<PolymorphicFunctionType>()
           ->substGenericArgs(&P.SF.TU, subs)->getCanonicalType());
    }
    
    ResultVal = B.createApply(InstLoc, FnVal, FnTy,
                              FTI->getResult().getSILType(), subs, Args,
                              Transparent);
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
    SILValue FnVal = getLocalValue(FnName, Ty, InstLoc);
    SmallVector<SILValue, 4> Args;
    unsigned ArgNo = ArgTys.size() - ArgNames.size();
    for (auto &ArgName : ArgNames)
      Args.push_back(getLocalValue(ArgName, ArgTys[ArgNo++], InstLoc));

    SILType FnTy = FnVal.getType();
    if (!subs.empty()) {
      FnTy = SILType::getPrimitiveObjectType(
         FnTy.castTo<PolymorphicFunctionType>()
           ->substGenericArgs(&P.SF.TU, subs)->getCanonicalType());
    }
    
    SILType closureTy =
      SILBuilder::getPartialApplyResultType(Ty, ArgNames.size(), SILMod);
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
  
  ResultVal = B.createFunctionRef(InstLoc,
                                  getGlobalNameForReference(Name, Ty, Loc));
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

  SILLinkage FnLinkage;
  Identifier FnName;
  SILType FnType;
  SourceLoc FnNameLoc;

  Scope S(this, ScopeKind::TopLevel);
  bool isTransparent = false;
  if (parseSILLinkage(FnLinkage, *this) ||
      parseSILOptional(isTransparent, FunctionState, "transparent") ||
      parseToken(tok::at_sign, diag::expected_sil_function_name) ||
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
    FunctionState.F->setTransparent(IsTransparent_t(isTransparent));
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

  if (FunctionState.diagnoseProblems())
    return true;

  // If SIL prsing succeeded, verify the generated SIL.
  if (!FunctionState.P.Diags.hadAnyError())
    FunctionState.F->verify();

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

/// decl-sil-vtable: [[only in SIL mode]]
///   'sil_vtable' ClassName decl-sil-vtable-body
/// decl-sil-vtable-body:
///   '{' sil-vtable-entry+ '}'
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
  SmallVector<ValueDecl*, 4> CurModuleResults;
  SF.TU.lookupValue(Module::AccessPathTy(), Name,
                    NLKind::UnqualifiedLookup, CurModuleResults);
  if (CurModuleResults.size() != 1) {
    diagnose(Loc, diag::sil_vtable_class_not_found, Name);
    return true;
  }
  ClassDecl *theClass = dyn_cast<ClassDecl>(CurModuleResults[0]);
  if (!theClass) {
    diagnose(Loc, diag::sil_vtable_class_not_found, Name);
    return true;
  }

  SourceLoc LBraceLoc = Tok.getLoc();
  if (consumeIf(tok::l_brace)) {
    // We need to turn on InSILBody to parse SILDeclRef.
    Lexer::SILBodyRAII Tmp(*L);
    // Parse the entry list.
    std::vector<SILVTable::Pair> vtableEntries;
    do {
      SILDeclRef Ref;
      Identifier FuncName;
      SourceLoc FuncLoc;
      if (VTableState.parseSILDeclRef(Ref) ||
          parseToken(tok::colon, diag::expected_sil_vtable_colon) ||
          VTableState.parseSILIdentifier(FuncName, FuncLoc,
                                         diag::expected_sil_value_name))
        return true;
      SILFunction *Func = SIL->M->lookup(FuncName.str());
      if (!Func) {
        diagnose(FuncLoc, diag::sil_vtable_func_not_found, FuncName);
        return true;
      }
      vtableEntries.emplace_back(Ref, Func);
    } while (Tok.isNot(tok::r_brace) && Tok.isNot(tok::eof));

    SourceLoc RBraceLoc;
    parseMatchingToken(tok::r_brace, RBraceLoc, diag::expected_sil_rbrace,
                       LBraceLoc);

    SILVTable::create(*SIL->M, theClass, vtableEntries);
  }
  return false;
}
