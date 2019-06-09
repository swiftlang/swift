//===--- ParseableInterfaceSupport.cpp - swiftinterface files ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Frontend/XCTestMethodsEmitter.h"
#include "swift/Basic/JSONSerialization.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"

using namespace swift;

// Copied from lib/Index/IndexSymbol.cpp
static NominalTypeDecl *getNominalParent(const ValueDecl *D) {
  return D->getDeclContext()->getSelfNominalTypeDecl();
}
static bool isUnitTestCase(const ClassDecl *D) {
  if (!D)
    return false;
  
  return D->walkSuperclasses([D](ClassDecl *SuperD) {
    if (SuperD != D && // Do not treate XCTestCase itself as a test.
        SuperD->getNameStr() == "XCTestCase")
      return TypeWalker::Action::Stop; // Found test; stop and return true.
    return TypeWalker::Action::Continue;
  });
}
static bool isUnitTest(const ValueDecl *D) {
  if (!D->hasName())
    return false;
  
  // A 'test candidate' is:
  // 1. An instance method...
  auto FD = dyn_cast<FuncDecl>(D);
  if (!FD)
    return false;
  if (!D->isInstanceMember())
    return false;
  
  // 2. ...on a class or extension (not a struct) subclass of XCTestCase...
  auto parentNTD = getNominalParent(D);
  if (!parentNTD)
    return false;
  if (!isa<ClassDecl>(parentNTD))
    return false;
  if (!isUnitTestCase(cast<ClassDecl>(parentNTD)))
    return false;
  
  // 3. ...that returns void...
  Type RetTy = FD->getResultInterfaceType();
  if (RetTy && !RetTy->isVoid())
    return false;
  
  // 4. ...takes no parameters...
  if (FD->getParameters()->size() != 0)
    return false;
  
  // 5. ...is of at least 'internal' access (unless we can use
  //    Objective-C reflection)...
  if (!D->getASTContext().LangOpts.EnableObjCInterop &&
      (D->getFormalAccess() < AccessLevel::Internal ||
       parentNTD->getFormalAccess() < AccessLevel::Internal))
    return false;
  
  // 6. ...and starts with "test".
  if (FD->getName().str().startswith("test"))
    return true;
  
  return false;
}

namespace {
  struct XCTestClass {
    std::string Name;
    std::vector<std::string> Methods;
  };
} // namespace

namespace swift {
namespace json {

template <> struct ObjectTraits<XCTestClass> {
  static void mapping(Output &out, XCTestClass &value) {
    out.mapRequired("testClass", value.Name);
    out.mapRequired("testMethods", value.Methods);
  }
};

} // namespace json
} // namespace swift

bool swift::emitXCTestMethods(raw_ostream &out, ModuleDecl *M) {
  assert(M);

  std::vector<XCTestClass> xctestClasses;
  SmallVector<Decl *, 16> topLevelDecls;
  M->getTopLevelDecls(topLevelDecls);

  for (const Decl *D : topLevelDecls) {
    auto CD = dyn_cast<ClassDecl>(D);
    if (CD && isUnitTestCase(CD)) {
        std::vector<std::string> Methods;
        for (auto member : CD->getMembers()) {
          auto VD = dyn_cast<ValueDecl>(member);
          if (VD && isUnitTest(VD)) {
            SmallString<32> scratch;
            VD->getFullName().getString(scratch);
            Methods.push_back(std::string(scratch.str()));
          }
        }

        XCTestClass klass = XCTestClass();
        SmallString<32> scratch;
        CD->getFullName().getString(scratch);
        klass.Name = std::string(scratch.str());
        klass.Methods = Methods;

        xctestClasses.push_back(klass);
    }
  }

  swift::json::Output Out(out);
  Out << xctestClasses;

  return false;
}
