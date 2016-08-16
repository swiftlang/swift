//
//  MscrStaticAnalyzer.hpp
//  swift
//
//  Class used to handle the retrieval of static metrics.
//
//  Created by Marcel de Siqueira Campos Rebouças on 3/30/16.
//
//

#ifndef MscrStaticAnalyzer_hpp
#define MscrStaticAnalyzer_hpp

#include "swift/Basic/QuotedString.h"
#include "swift/AST/AST.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/TypeVisitor.h"
#include "swift/Basic/STLExtras.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/Support/raw_ostream.h"

#include <iostream>
#include <fstream>

using namespace std;

class MscrStaticAnalyzer
{
public:
    
    // Method used to get the instance of this Singleton.
    static MscrStaticAnalyzer* Instance();
    
    // Method used to handle function declarations.
    void handleFuncDecl(llvm::raw_ostream &OS, swift::FuncDecl *FD);
    // Method used to handle class declarations.
    void handleClassDecl(llvm::raw_ostream &OS, swift::ClassDecl *CD);
    // Method used to handle variable (and constants) declarations.
    void handleVarDecl(llvm::raw_ostream &OS, swift::VarDecl *VD);
    // Method used to handle parameter declarations.
    void handleFuncParameter(llvm::raw_ostream &OS, swift::ParamDecl *P);
    // Method used to handle if lets.
    void handleIfLet(llvm::raw_ostream &OS, swift::OptionalSomePattern *P);
    // Method used to handle if statements.
    void handleIfStmt(llvm::raw_ostream &OS, swift::IfStmt *S);
    // Method used to handle guard statements.
    void handleGuardStmt(llvm::raw_ostream &OS, swift::GuardStmt *S);
    
private:
    
    MscrStaticAnalyzer();
    
    ~MscrStaticAnalyzer();
    
    // static  initialisation
    static bool g_initialised;
    
    // dynamic initialisation
    static MscrStaticAnalyzer g_instance;
    ofstream outputFile;
    
    void writeToOutput(const char* data);
    
};

#endif /* MscrStaticAnalyzer_hpp */
