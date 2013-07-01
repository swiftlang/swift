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

#include "Parser.h"
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
    llvm::StringMap<SILValue> LocalValues;
    llvm::StringMap<SourceLoc> ForwardRefLocalValues;
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

    /// getLocalValue - Get a reference to a local value with the specified name
    /// and type.
    SILValue getLocalValue(StringRef Name, SILType Type, SourceLoc NameLoc);
    
    /// setLocalValue - When an instruction or block argument is defined, this
    /// method is used to register it and update our symbol table.
    void setLocalValue(SILValue Value, StringRef Name, SourceLoc NameLoc);
    
  public:
    // Parsing logic.
    bool parseSILType(SILType &Result);
    bool parseSILType(SILType &Result, SourceLoc &TypeLoc) {
      TypeLoc = P.Tok.getLoc();
      return parseSILType(Result);
    }

    bool parseGlobalName(Identifier &Name);

    struct UnresolvedValueName {
      StringRef Name;
      SourceLoc NameLoc;
    };

    bool parseValueName(UnresolvedValueName &Name);
    SILValue getLocalValue(UnresolvedValueName Name, SILType Type) {
      return getLocalValue(Name.Name, Type, Name.NameLoc);
    }
    bool parseValueRef(SILValue &Result, SILType Ty);
    bool parseTypedValueRef(SILValue &Result, SourceLoc &Loc);
    bool parseTypedValueRef(SILValue &Result) {
      SourceLoc Tmp;
      return parseTypedValueRef(Result, Tmp);
    }
    bool parseAllocKind(AllocKind &Result);
    bool parseSILOpcode(ValueKind &Opcode, SourceLoc &OpcodeLoc,
                        StringRef &OpcodeName);
    bool parseSILInstruction(SILBasicBlock *BB);
    bool parseCallInstruction(ValueKind Opcode, SILBuilder &B,
                              SILValue &ResultVal);
    bool parseSILFunctionRef(SILBuilder &B, SILValue &ResultVal);

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
SILValue SILParser::getLocalValue(StringRef Name, SILType Type,
                                  SourceLoc NameLoc) {
  SILValue &Entry = LocalValues[Name];
  
  // This is a forward reference.  Create a dummy node to represent
  // it until we see a real definition.
  if (!Entry) {
    ForwardRefLocalValues[Name] = NameLoc;
    Entry = new (SILMod) GlobalAddrInst(SILLocation(), nullptr, Type);
    return Entry;
  }
  
  // If this value is already defined, check it.
  if (Entry.getType() != Type) {
    HadError = true;
    P.diagnose(NameLoc, diag::sil_value_use_type_mismatch, Name,
               Entry.getType().getAsString());
    // Make sure to return something of the requested type.
    return new (SILMod) GlobalAddrInst(SILLocation(), nullptr, Type);
  }

  return Entry;
}

/// setLocalValue - When an instruction or block argument is defined, this
/// method is used to register it and update our symbol table.
void SILParser::setLocalValue(SILValue Value, StringRef Name,
                              SourceLoc NameLoc) {
  SILValue &Entry = LocalValues[Name];

  if (!Entry) {
    Entry = Value;
    return;
  }
  
  // If this value was already defined, it is either a redefinition, or a
  // specification for a forward referenced value.
  if (!ForwardRefLocalValues.erase(Name)) {
    P.diagnose(NameLoc, diag::sil_value_redefinition, Name);
    HadError = true;
    return;
  }
  
  // If the forward reference was of the wrong type, diagnose this now.
  if (Entry.getType() != Value.getType()) {
    P.diagnose(NameLoc, diag::sil_value_def_type_mismatch, Name,
               Entry.getType().getAsString());
    HadError = true;
  } else {
    Entry.replaceAllUsesWith(Value);
  }
  Entry = Value;
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


///   sil-type:
///     '$' '*'? type-annotation
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

  TypeLoc Ty;
  if (P.parseTypeAnnotation(Ty, diag::expected_sil_type))
    return true;

  // If we successfully parsed the type, do some type checking / name binding
  // of it.
  {
    // We have to lie and say we're done with parsing to make this happen.
    assert(P.TU->ASTStage == TranslationUnit::Parsing &&
           "Unexpected stage during parsing!");
    llvm::SaveAndRestore<Module::ASTStage_t> ASTStage(P.TU->ASTStage,
                                                      TranslationUnit::Parsed);
    if (performTypeLocChecking(P.TU, Ty))
      return true;
  }

  Result = SILType::getPrimitiveType(Ty.getType()->getCanonicalType(),
                                     isAddress);
  return false;
}

/// parseValueName - Parse a value name without a type available yet.
///
///     sil-value-name:
///       sil-local-name
///
bool SILParser::parseValueName(UnresolvedValueName &Result) {
  Result.Name = P.Tok.getText();

  return P.parseToken(tok::sil_local_name, Result.NameLoc,
                      diag::expected_sil_value_name);
}

/// parseValueRef - Parse a value, given a contextual type.
///
///     sil-value-ref:
///       sil-local-name
///
bool SILParser::parseValueRef(SILValue &Result, SILType Ty) {
  SourceLoc NameLoc;
  StringRef Name = P.Tok.getText();
  if (P.parseToken(tok::sil_local_name, NameLoc,diag::expected_sil_value_name))
    return true;
  
  Result = getLocalValue(Name, Ty, NameLoc);
  return false;
}

/// parseTypedValueRef - Parse a type/value reference pair.
///
///    sil-typed-valueref:
///       sil-value-ref ':' sil-type
///
bool SILParser::parseTypedValueRef(SILValue &Result, SourceLoc &Loc) {
  SILType Ty;
  StringRef Name = P.Tok.getText();
  if (P.parseToken(tok::sil_local_name, Loc, diag::expected_sil_value_name)||
      P.parseToken(tok::colon, diag::expected_sil_colon_value_ref) ||
      parseSILType(Ty))
    return true;
  
  Result = getLocalValue(Name, Ty, Loc);
  return false;
}


/// parseAllocKind - Parse an allocation specifier.
///    sil-alloc-kind:
///      'heap'
///      'pseudo'
///      'stack'
bool SILParser::parseAllocKind(AllocKind &Result) {
  Identifier Id;
  SourceLoc Loc;
  if (P.parseIdentifier(Id, Loc, diag::sil_expected_allocation_kind))
    return true;
  if (Id.str() == "heap")
    Result = AllocKind::Heap;
  else if (Id.str() == "pseudo")
    Result = AllocKind::Pseudo;
  else if (Id.str() == "stack")
    Result = AllocKind::Stack;
  else {
    P.diagnose(Loc, diag::sil_expected_allocation_kind);
    return true;
  }
    
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
    .Case("address_to_pointer", ValueKind::AddressToPointerInst)
    .Case("alloc_var", ValueKind::AllocVarInst)
    .Case("alloc_ref", ValueKind::AllocRefInst)
    .Case("archetype_ref_to_super", ValueKind::ArchetypeRefToSuperInst)
    .Case("apply", ValueKind::ApplyInst)
    .Case("br", ValueKind::BranchInst)
    .Case("bridge_to_block", ValueKind::BridgeToBlockInst)
    .Case("coerce", ValueKind::CoerceInst)
    .Case("condbranch", ValueKind::CondBranchInst)
    .Case("convert_cc", ValueKind::ConvertCCInst)
    .Case("convert_function", ValueKind::ConvertFunctionInst)
    .Case("dealloc_var", ValueKind::DeallocVarInst)
    .Case("destroy_addr", ValueKind::DestroyAddrInst)
    .Case("downcast", ValueKind::DowncastInst)
    .Case("downcast_archetype_addr", ValueKind::DowncastArchetypeAddrInst)
    .Case("downcast_archetype_ref", ValueKind::DowncastArchetypeRefInst)
    .Case("downcast_existential_ref", ValueKind::DowncastExistentialRefInst)
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
    .Case("raw_pointer_to_ref", ValueKind::RawPointerToRefInst)
    .Case("ref_to_object_pointer", ValueKind::RefToObjectPointerInst)
    .Case("ref_to_raw_pointer", ValueKind::RefToRawPointerInst)
    .Case("release", ValueKind::ReleaseInst)
    .Case("retain", ValueKind::RetainInst)
    .Case("retain_autoreleased", ValueKind::RetainAutoreleasedInst)
    .Case("return", ValueKind::ReturnInst)
    .Case("store", ValueKind::StoreInst)
    .Case("super_to_archetype_ref", ValueKind::SuperToArchetypeRefInst)
    .Case("thin_to_thick_function", ValueKind::ThinToThickFunctionInst)
    .Case("tuple", ValueKind::TupleInst)
    .Case("upcast", ValueKind::UpcastInst)
    .Case("upcast_existential_ref", ValueKind::UpcastExistentialRefInst)
    .Default(ValueKind::SILArgument);

  if (Opcode != ValueKind::SILArgument) {
    P.consumeToken();
    return false;
  }
  P.diagnose(OpcodeLoc, diag::expected_sil_instr_opcode);
  return true;
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
  SILValue ResultVal;
  switch (Opcode) {
  default: assert(0 && "Unreachable");
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
    SourceLoc FromNameLoc, ToLoc, AddrLoc;
    StringRef FromName = P.Tok.getText();
    Identifier ToToken;
    SILValue AddrVal;
    if (P.parseToken(tok::sil_local_name, FromNameLoc,
                     diag::expected_sil_value_name) ||
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
    
    SILValue FromVal =
      getLocalValue(FromName, AddrVal.getType().getObjectType(), FromNameLoc);
    ResultVal = B.createStore(SILLocation(), FromVal, AddrVal);
    break;
  }
  case ValueKind::AllocVarInst:
  case ValueKind::AllocRefInst: {
    AllocKind Kind;
    SILType Ty;
    if (parseAllocKind(Kind) ||
        parseSILType(Ty))
      return true;
    
    if (Opcode == ValueKind::AllocVarInst)
      ResultVal = B.createAllocVar(SILLocation(), Kind, Ty);
    else {
      assert(Opcode == ValueKind::AllocRefInst);
      ResultVal = B.createAllocRef(SILLocation(), Kind, Ty);
    }
    break;
  }
  case ValueKind::DeallocVarInst: {
    AllocKind Kind;
    if (parseAllocKind(Kind) ||
        parseTypedValueRef(Val))
      return true;
    
    ResultVal = B.createDeallocVar(SILLocation(), Kind, Val);
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
    SourceLoc CondNameLoc;
    StringRef CondName = P.Tok.getText();
    Identifier BBName, BBName2;
    SourceLoc NameLoc, NameLoc2;
    if (P.parseToken(tok::sil_local_name, CondNameLoc,
                     diag::expected_sil_value_name) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        P.parseIdentifier(BBName, NameLoc, diag::expected_sil_block_name) ||
        P.parseToken(tok::comma, diag::expected_tok_in_sil_instr, ",") ||
        P.parseIdentifier(BBName2, NameLoc2, diag::expected_sil_block_name))
      return true;
   
    auto I1Ty =
      SILType::getBuiltinIntegerType(1, BB->getParent()->getASTContext());
    SILValue CondVal =
      getLocalValue(CondName, I1Ty, CondNameLoc);

    ResultVal = B.createCondBranch(SILLocation(), CondVal,
                                   getBBForReference(BBName, NameLoc),
                                   getBBForReference(BBName2, NameLoc2));
    break;
  }
  }
  
  setLocalValue(ResultVal, ResultName, ResultNameLoc);
  return false;
}

bool SILParser::parseCallInstruction(ValueKind Opcode, SILBuilder &B,
                                     SILValue &ResultVal) {
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

bool SILParser::parseSILFunctionRef(SILBuilder &B, SILValue &ResultVal) {
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

  if (parseSILLinkage(FnLinkage, *this) ||
      parseToken(tok::sil_at_sign, diag::expected_sil_function_name) ||
      parseIdentifier(FnName, FnNameLoc, diag::expected_sil_function_name) ||
      parseToken(tok::colon, diag::expected_sil_type) ||
      FunctionState.parseSILType(FnType))
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

  return FunctionState.diagnoseProblems();
}

