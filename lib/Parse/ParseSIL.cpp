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
#include "swift/SIL/SILModule.h"
#include "swift/Subsystems.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/ADT/StringSwitch.h"
using namespace swift;

//===----------------------------------------------------------------------===//
// SILParserFunctionState
//===----------------------------------------------------------------------===//

namespace {
  class SILParserFunctionState {
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
  public:

    SILParserFunctionState(Parser &P, SILFunction *F)
      : P(P), SILMod(F->getModule()), F(F) {}

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


    
  public:
    // Parsing logic.
    bool parseSILInstruction(SILBasicBlock *BB);
    bool parseSILBasicBlock();
  };
} // end anonymous namespace.

/// diagnoseProblems - After a function is fully parse, emit any diagnostics
/// for errors and return true if there were any.
bool SILParserFunctionState::diagnoseProblems() {
  // Check for any uses of basic blocks that were not defined.
  if (!UndefinedBlocks.empty()) {
    // FIXME: These are going to come out in nondeterminstic order.
    for (auto Entry : UndefinedBlocks)
      P.diagnose(Entry.second.first, diag::undefined_basicblock_use,
                 Entry.second.second);

    HadError = true;
  }

  return HadError;
}

/// getBBForDefinition - Return the SILBasicBlock for a definition of the
/// specified block.
SILBasicBlock *SILParserFunctionState::getBBForDefinition(Identifier Name,
                                                          SourceLoc Loc) {
  SILBasicBlock *&BB = BlocksByName[Name];
  // If the block has never been named yet, just create it.
  if (BB == nullptr)
    return BB = new (SILMod) SILBasicBlock(F);

  // If it already exists, it was either a forward reference or a redefinition.
  // If it is a forward reference, it should be in our undefined set.
  if (!UndefinedBlocks.erase(BB)) {
    // If we have a redefinition, return a new BB to avoid inserting
    // instructions after the terminator.
    P.diagnose(Loc, diag::basicblock_redefinition, Name);
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
SILBasicBlock *SILParserFunctionState::getBBForReference(Identifier Name,
                                                         SourceLoc Loc) {
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
///     '$' sil-type-attributes? '*'? type
///   sil-type-attributes:
///     '[' sil-type-attribute (',' sil-type-attribute)* ']'
///   sil-type-attribute:
///     'sil_sret'
///     'sil_uncurry' '=' integer_literal
///
bool Parser::parseSILType(SILType &Result) {
  bool IsSRet = false;
  unsigned UncurryLevel = 0;

  // If we have sil-type-attribute list, parse it.
  if (Tok.is(tok::l_square) && peekToken().is(tok::identifier) &&
      peekToken().getText().startswith("sil_")) {
    SourceLoc LeftLoc = Tok.getLoc(), RightLoc;

    // The attribute list is always guaranteed to have at least one attribute.
    do {
      consumeToken();

      SourceLoc AttrTokenLoc = Tok.getLoc();
      Identifier AttrToken;
      if (parseIdentifier(AttrToken,
                          diag::expected_identifier_sil_type_attributes))
        return true;

      if (AttrToken.str() == "sil_sret") {
        IsSRet = true;
        consumeToken(tok::identifier);
      } else if (AttrToken.str() == "sil_uncurry") {
        if (parseToken(tok::identifier, diag::malformed_sil_uncurry_attribute)||
            parseToken(tok::equal, diag::malformed_sil_uncurry_attribute))
          return true;
        if (Tok.isNot(tok::integer_literal) ||
            Tok.getText().getAsInteger(10, UncurryLevel)) {
          diagnose(Tok, diag::malformed_sil_uncurry_attribute);
          return true;
        }

        consumeToken(tok::integer_literal);
      } else {
        diagnose(AttrTokenLoc, diag::unknown_sil_type_attribute);
        return true;
      }

      // Continue parsing the next token.
    } while (Tok.is(tok::comma));

    if (parseMatchingToken(tok::r_square, RightLoc,
                           diag::expected_bracket_sil_type_attributes, LeftLoc))
      return true;
  }

  // If we have a '*', then this is an address type.
  bool isAddress = false;
  if (Tok.isAnyOperator() && Tok.getText() == "*") {
    isAddress = true;
    consumeToken();
  }

  TypeLoc Ty;
  if (parseToken(tok::sil_dollar, diag::expected_sil_type) ||
      parseType(Ty, diag::expected_sil_type))
    return true;

  // If we successfully parsed the type, do some type checking / name binding
  // of it.
  {
    // We have to lie and say we're done with parsing to make this happen.
    assert(TU->ASTStage == TranslationUnit::Parsing &&
           "Unexpected stage during parsing!");
    llvm::SaveAndRestore<Module::ASTStage_t> ASTStage(TU->ASTStage,
                                                      TranslationUnit::Parsed);
    if (performTypeLocChecking(TU, Ty))
      return true;
  }

  // FIXME: Stop using TypeConverter when SILType for functions doesn't contain
  // SILTypes itself.
  (void)IsSRet;
  Result = SIL->Types.getLoweredType(Ty.getType(), UncurryLevel);

  // If this is an address type, apply the modifier.
  if (isAddress)
    Result = Result.getAddressType();
  return false;
}


///   sil-instruction:
///     sil_local_name '=' identifier ...
bool SILParserFunctionState::parseSILInstruction(SILBasicBlock *BB) {
  if (P.Tok.isNot(tok::sil_local_name)) {
    P.diagnose(P.Tok, diag::expected_sil_instr_name);
    return true;
  }
  P.consumeToken(tok::sil_local_name);
  
  // Eat away, nom nom nom.
  while (P.Tok.isNot(tok::r_brace) && P.Tok.isNot(tok::eof) &&
         !P.Tok.isAtStartOfLine())
    P.consumeToken();

  return false;
}


///   sil-basic-block:
///     identifier /*TODO: argument list*/ ':' sil-instruction+
bool SILParserFunctionState::parseSILBasicBlock() {
  Identifier BBName;
  SourceLoc NameLoc = P.Tok.getLoc();

  if (P.parseIdentifier(BBName, diag::expected_sil_block_name) ||
      P.parseToken(tok::colon, diag::expected_sil_block_colon))
    return true;

  SILBasicBlock *BB = getBBForDefinition(BBName, NameLoc);

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

  SILLinkage FnLinkage;
  Identifier FnName;
  SILType FnType;

  if (parseSILLinkage(FnLinkage, *this) ||
      parseToken(tok::sil_at_sign, diag::expected_sil_function_name) ||
      parseIdentifier(FnName, diag::expected_sil_function_name) ||
      parseToken(tok::colon, diag::expected_sil_type) ||
      parseSILType(FnType))
    return true;

  // TODO: Verify it is a function type.
  SILFunction *Fn = new (*SIL) SILFunction(*SIL, FnLinkage,
                                           FnName.str(), FnType);

  SILParserFunctionState FunctionState(*this, Fn);

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

