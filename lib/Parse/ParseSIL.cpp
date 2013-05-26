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
// SILParser
//===----------------------------------------------------------------------===//

namespace {
  class SILParser {
  public:
    Parser &P;
    SILModule &SILMod;
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

    SILParser(Parser &P) : P(P), SILMod(*P.SIL) {}

    /// diagnoseProblems - After a function is fully parse, emit any diagnostics
    /// for errors and return true if there were any.
    bool diagnoseProblems();
    
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

/// getBBForDefinition - Return the SILBasicBlock for a definition of the
/// specified block.
SILBasicBlock *SILParser::getBBForDefinition(Identifier Name,
                                                          SourceLoc Loc) {
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

  // FIXME: Stop using TypeConverter when SILType for functions doesn't contain
  // SILTypes itself.
  Result = SILMod.Types.getLoweredType(Ty.getType(), 0);

  // If this is an address type, apply the modifier.
  if (isAddress)
    Result = Result.getAddressType();
  return false;
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
    .Case("integer_literal", ValueKind::IntegerLiteralInst)
    .Case("retain", ValueKind::RetainInst)
    .Case("release", ValueKind::ReleaseInst)
    .Case("retain_autoreleased", ValueKind::RetainAutoreleasedInst)
    .Case("load", ValueKind::LoadInst)
    .Case("store", ValueKind::StoreInst)
    .Case("alloc_var", ValueKind::AllocVarInst)
    .Case("dealloc_var", ValueKind::DeallocVarInst)
    .Case("metatype", ValueKind::MetatypeInst)
    .Case("tuple", ValueKind::TupleInst)
    .Case("return", ValueKind::ReturnInst)
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
  case ValueKind::RetainInst:
    if (parseTypedValueRef(Val)) return true;
    B.createRetainInst(SILLocation(), Val);
    break;
  case ValueKind::ReleaseInst:
    if (parseTypedValueRef(Val)) return true;
    ResultVal = B.createReleaseInst(SILLocation(), Val);
    break;
  case ValueKind::RetainAutoreleasedInst:
    if (parseTypedValueRef(Val)) return true;
    ResultVal = B.createRetainAutoreleased(SILLocation(), Val);
    break;
  case ValueKind::LoadInst:
    if (parseTypedValueRef(Val)) return true;
    ResultVal = B.createLoad(SILLocation(), Val);
    break;
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
  case ValueKind::AllocVarInst: {
    AllocKind Kind;
    SILType Ty;
    if (parseAllocKind(Kind) ||
        parseSILType(Ty))
      return true;
    
    ResultVal = B.createAllocVar(SILLocation(), Kind, Ty);
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
      // FIXME: Stop using TypeConverter
      auto Ty2 = SILMod.Types.getLoweredType(Ty, 0);
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
  }
  
  setLocalValue(ResultVal, ResultName, ResultNameLoc);
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

  if (parseSILLinkage(FnLinkage, *this) ||
      parseToken(tok::sil_at_sign, diag::expected_sil_function_name) ||
      parseIdentifier(FnName, diag::expected_sil_function_name) ||
      parseToken(tok::colon, diag::expected_sil_type) ||
      FunctionState.parseSILType(FnType))
    return true;

  // TODO: Verify it is a function type.
  FunctionState.F = new (*SIL) SILFunction(*SIL, FnLinkage,
                                           FnName.str(), FnType);


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

