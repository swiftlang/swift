//
//  MscrStaticAnalyzer.cpp
//  swift
//
//  Created by Marcel de Siqueira Campos Rebouças on 3/30/16.
//
//

#include "swift/AST/MscrStaticAnalyzer.hpp"

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
using namespace swift;

// static  initialisation
bool MscrStaticAnalyzer::g_initialised;

// dynamic initialisation
MscrStaticAnalyzer MscrStaticAnalyzer::g_instance;
ofstream outputFile;


//===----------------------------------------------------------------------===//
//===                           Singleton Methods                          ===//
//===----------------------------------------------------------------------===//

// Singleton constructor.
MscrStaticAnalyzer::MscrStaticAnalyzer()
{
    g_initialised = true;
    
    // open and clear the file
    outputFile.open("build-output.txt", std::ofstream::out | std::ofstream::trunc);
    outputFile << "Starting build output file! \n";
    outputFile.close();
}

MscrStaticAnalyzer::~MscrStaticAnalyzer()
{
    g_initialised = false;
}

// Method used to get the instance of this Singleton.
MscrStaticAnalyzer* MscrStaticAnalyzer::Instance()
{
    return g_initialised ? &g_instance : 0;
}

//===----------------------------------------------------------------------===//
//===                           Output File Methods                        ===//
//===----------------------------------------------------------------------===//

void MscrStaticAnalyzer::writeToOutput(const char* data)
{
    outputFile.open("example2.txt", std::ios_base::app);
    outputFile << data;
    outputFile.close();
}


//===----------------------------------------------------------------------===//
//===                       Component Handler Methods                      ===//
//===----------------------------------------------------------------------===//



// Method used to handle function declarations.
void MscrStaticAnalyzer::handleFuncDecl(llvm::raw_ostream &OS, swift::FuncDecl *FD)
{
    if (!(FD->isImplicit())) {
        
        writeToOutput("Found func decl.\n");
        OS << "\n";
        OS << "Found new explicit method! ";
        OS << "Name: " << FD->getFullName();
        OS << "\n;";
        
        // for each parameter in the function
        for (auto pl : FD->getParameterLists()) {
            for (auto P : *pl) {
                handleFuncParameter(OS, P);
            }
        }
    }
}

// Method used to handle function declarations.
void MscrStaticAnalyzer::handleFuncParameter(llvm::raw_ostream &OS, swift::ParamDecl *P)
{
    writeToOutput("Found parameter!! \n");
    OS << "Found parameter!! ";
    
    if (P->getFullName())
        OS << '\"' << P->getFullName() << '\"';
    else
        OS << "'anonname=" << (const void*)P << '\'';
    
    
    if (!P->getArgumentName().empty())
        OS << " apiName=" << P->getArgumentName();
    
    OS << " type=";
    if (P->hasType()) {
        OS << '\'';
        P->getType().print(OS);
        OS << '\'';
    } else
        OS << "<null type>";
    
    if (!P->isLet())
        OS << " mutable";
    
    if (P->isVariadic())
        OS << " variadic";
    
    switch (P->getDefaultArgumentKind()) {
        case DefaultArgumentKind::None: break;
        case DefaultArgumentKind::Column:
            OS << "#column";
            break;
        case DefaultArgumentKind::DSOHandle:
            OS << "#dsohandle";
            break;
        case DefaultArgumentKind::File:
            OS << "#file";
            break;
        case DefaultArgumentKind::Function:
            OS << "#function";
            break;
        case DefaultArgumentKind::Inherited:
            OS << "inherited";
            break;
        case DefaultArgumentKind::Line:
            OS << "#line";
            break;
        case DefaultArgumentKind::Nil:
            OS << "nil";
            break;
        case DefaultArgumentKind::EmptyArray:
            OS << "[]";
            break;
        case DefaultArgumentKind::EmptyDictionary:
            OS << "[:]";
            break;
        case DefaultArgumentKind::Normal:
            OS << "normal";
            break;
    }
}

// Method used to handle class declarations.
void MscrStaticAnalyzer::handleClassDecl(llvm::raw_ostream &OS, swift::ClassDecl *CD)
{
    writeToOutput("Found new class decl.\n");
    OS << "\n";
    OS << "Found new class decl! ";
    OS << "Name: " << CD->getFullName();
    OS << "\n;";
}

// Method used to handle variable (and constant) declarations.
void MscrStaticAnalyzer::handleVarDecl(llvm::raw_ostream &OS, swift::VarDecl *VD) {
    writeToOutput("Found new var decl.\n");
    
    OS << "\n";
    OS << "Found new var decl! ";
    OS << "Name: " << VD->getFullName();
    OS << "Type: " << VD->getType();
    
    if (VD->hasAccessibility()) {
        OS << " access=";
        switch (VD->getFormalAccess()) {
            case Accessibility::Private:
                OS << "private";
                break;
            case Accessibility::Internal:
                OS << "internal";
                break;
            case Accessibility::Public:
                OS << "public";
                break;
        }
    }
    OS << "\n;";
    
}

// Method used to handle if lets.
void MscrStaticAnalyzer::handleIfLet(llvm::raw_ostream &OS, swift::OptionalSomePattern *P) {
    writeToOutput("Found new IfLet.\n");
    OS << "\n";
    OS << "Found new IfLet! ";
    OS << "\n";
}

// Method used to handle if statements.
void MscrStaticAnalyzer::handleIfStmt(llvm::raw_ostream &OS, swift::IfStmt *S) {
    writeToOutput("Found If Statement! \n");
    OS << "\n";
    OS << "Found If Statement! ";
    OS << "\n";
}

// Method used to handle guard statements.
void MscrStaticAnalyzer::handleGuardStmt(llvm::raw_ostream &OS, swift::GuardStmt *S) {
    writeToOutput("Found Guard Stmt.\n");
    OS << "\n";
    OS << "Found Guard Statement! ";
    OS << "\n";
}