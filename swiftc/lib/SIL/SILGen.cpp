#include "swiftc/SIL/SILGen.h"
#include "swiftc/SIL/SILType.h"
#include "swiftc/AST/Type.h"

using namespace swiftc;

SILGen::SILGen(DiagnosticEngine& diags)
    : Diags(diags), Module(std::make_unique<SILModule>()),
      CurrentFunction(nullptr), CurrentBlock(nullptr) {}

std::unique_ptr<SILModule> SILGen::generateSIL(std::vector<std::unique_ptr<Decl>>& decls) {
  for (const auto& decl : decls) {
    generateDecl(decl.get());
  }
  
  return std::move(Module);
}

void SILGen::generateDecl(Decl* decl) {
  switch (decl->getKind()) {
  case NodeKind::FuncDecl:
    generateFuncDecl(static_cast<FuncDecl*>(decl));
    break;
  
  case NodeKind::VarDecl:
    // Global variables would be handled here
    break;
  
  case NodeKind::ClassDecl:
  case NodeKind::StructDecl:
    // Type declarations would be handled here
    break;
  
  default:
    // Skip other declarations for now
    break;
  }
}

void SILGen::generateFuncDecl(FuncDecl* funcDecl) {
  // Create SIL function
  auto silFunc = std::make_unique<SILFunction>(funcDecl->getName());
  CurrentFunction = silFunc.get();
  
  // Create entry basic block
  CurrentBlock = createBasicBlock("entry");
  
  // Generate SIL for function body
  if (funcDecl->getBody()) {
    generateStmt(funcDecl->getBody());
  }
  
  // Add implicit return if needed
  if (CurrentBlock && CurrentBlock->getInstructions().empty()) {
    // TODO: Check if we need a return value
    // For now, just add a simple return
    // emitInst<ReturnInst>(SourceLoc(), SILValue());
  }
  
  Module->addFunction(std::move(silFunc));
  CurrentFunction = nullptr;
  CurrentBlock = nullptr;
}

SILValue SILGen::generateStmt(Stmt* stmt) {
  switch (stmt->getKind()) {
  case NodeKind::ExprStmt: {
    auto exprStmt = static_cast<ExprStmt*>(stmt);
    return generateExpr(exprStmt->getExpression());
  }
  
  case NodeKind::ReturnStmt: {
    auto returnStmt = static_cast<ReturnStmt*>(stmt);
    SILValue returnValue;
    
    if (returnStmt->hasValue()) {
      returnValue = generateExpr(returnStmt->getValue());
    }
    
    emitInst<ReturnInst>(returnStmt->getStartLoc(), returnValue);
    return SILValue();
  }
  
  case NodeKind::IfStmt: {
    auto ifStmt = static_cast<IfStmt*>(stmt);
    
    // Generate condition
    SILValue condition = generateExpr(ifStmt->getCondition());
    
    // Create basic blocks
    SILBasicBlock* thenBlock = createBasicBlock("if.then");
    SILBasicBlock* elseBlock = ifStmt->hasElse() ? createBasicBlock("if.else") : nullptr;
    SILBasicBlock* contBlock = createBasicBlock("if.cont");
    
    // Emit conditional branch
    // TODO: Implement conditional branch instruction
    
    // Generate then block
    CurrentBlock = thenBlock;
    generateStmt(ifStmt->getThenStmt());
    
    // Generate else block if present
    if (ifStmt->hasElse()) {
      CurrentBlock = elseBlock;
      generateStmt(ifStmt->getElseStmt());
    }
    
    CurrentBlock = contBlock;
    return SILValue();
  }
  
  default:
    return SILValue();
  }
}

SILValue SILGen::generateExpr(Expr* expr) {
  switch (expr->getKind()) {
  case NodeKind::IntegerLiteralExpr: {
    auto intLit = static_cast<IntegerLiteralExpr*>(expr);
    auto inst = emitInst<IntegerLiteralInst>(expr->getStartLoc(), intLit->getValue());
    return SILValue(inst);
  }
  
  case NodeKind::StringLiteralExpr: {
    // TODO: Implement string literal instruction
    return SILValue();
  }
  
  case NodeKind::BooleanLiteralExpr: {
    auto boolLit = static_cast<BooleanLiteralExpr*>(expr);
    // TODO: Implement boolean literal instruction
    return SILValue();
  }
  
  case NodeKind::IdentifierExpr: {
    auto identExpr = static_cast<IdentifierExpr*>(expr);
    // TODO: Look up variable and emit load instruction
    return SILValue();
  }
  
  case NodeKind::BinaryOperatorExpr: {
    auto binExpr = static_cast<BinaryOperatorExpr*>(expr);
    
    SILValue lhs = generateExpr(binExpr->getLHS());
    SILValue rhs = generateExpr(binExpr->getRHS());
    
    // TODO: Emit appropriate binary operation instruction
    return SILValue();
  }
  
  case NodeKind::CallExpr: {
    auto callExpr = static_cast<CallExpr*>(expr);
    
    SILValue callee = generateExpr(callExpr->getCallee());
    
    std::vector<SILValue> arguments;
    for (const auto& arg : callExpr->getArguments()) {
      arguments.push_back(generateExpr(arg.get()));
    }
    
    // TODO: Emit apply instruction
    return SILValue();
  }
  
  default:
    return SILValue();
  }
}

SILBasicBlock* SILGen::createBasicBlock(StringRef label) {
  auto block = std::make_unique<SILBasicBlock>(label);
  SILBasicBlock* result = block.get();
  
  if (CurrentFunction) {
    CurrentFunction->addBasicBlock(std::move(block));
  }
  
  return result;
}