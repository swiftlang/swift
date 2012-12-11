//===--- ImportDecl.cpp - Import Clang Declarations -----------------------===//
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
//
// This file implements support for importing Clang declarations into Swift.
//
//===----------------------------------------------------------------------===//

#include "ImporterImpl.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/AST/DeclVisitor.h"
#include "llvm/ADT/StringExtras.h"

using namespace swift;

namespace {
  /// \brief Convert Clang declarations into the corresponding Swift
  /// declarations.
  class SwiftDeclConverter
    : public clang::DeclVisitor<SwiftDeclConverter, Decl *>
  {
    ClangImporter::Implementation &Impl;

  public:
    explicit SwiftDeclConverter(ClangImporter::Implementation &impl)
      : Impl(impl) { }

    Decl *VisitDecl(clang::Decl *decl) {
      return nullptr;
    }

    Decl *VisitTranslationUnitDecl(clang::TranslationUnitDecl *decl) {
      // Note: translation units are handled specially by importDeclContext.
      return nullptr;
    }

    Decl *VisitNamespaceDecl(clang::NamespaceDecl *decl) {
      // FIXME: Implement once Swift has namespaces.
      return nullptr;
    }

    Decl *VisitUsingDirectiveDecl(clang::UsingDirectiveDecl *decl) {
      // Never imported.
      return nullptr;
    }

    Decl *VisitNamespaceAliasDecl(clang::NamespaceAliasDecl *decl) {
      // FIXME: Implement once Swift has namespaces.
      return nullptr;
    }

    Decl *VisitLabelDecl(clang::LabelDecl *decl) {
      // Labels are function-local, and therefore never imported.
      return nullptr;
    }

    Decl *VisitTypedefNameDecl(clang::TypedefNameDecl *decl){
      auto name = Impl.importName(decl->getDeclName());
      if (name.empty())
        return nullptr;

      auto dc = Impl.importDeclContext(decl->getDeclContext());
      if (!dc)
        return nullptr;

      Type type;

      // If this is the Objective-C BOOL type, map it to ObjCBool.
      auto &clangContext = Impl.getClangASTContext();
      if (name.str() == "BOOL" &&
          clangContext.hasSameType(decl->getUnderlyingType(),
                                   clangContext.ObjCBuiltinBoolTy)) {
        type = Impl.getNamedSwiftType("ObjCBool");
      } else {
        type = Impl.importType(decl->getUnderlyingType());
      }

      if (!type)
        return nullptr;

      auto loc = Impl.importSourceLoc(decl->getLocation());
      return new (Impl.SwiftContext) TypeAliasDecl(
                                      Impl.importSourceLoc(decl->getLocStart()),
                                      name,
                                      loc,
                                      TypeLoc(type, loc),
                                      dc,
                                      { });
    }

    Decl *
    VisitUnresolvedUsingTypenameDecl(clang::UnresolvedUsingTypenameDecl *decl) {
      // Note: only occurs in templates.
      return nullptr;
    }

    Decl *VisitEnumDecl(clang::EnumDecl *decl) {
      decl = decl->getDefinition();
      if (!decl)
        return nullptr;
      
      Identifier name;
      if (decl->getDeclName())
        name = Impl.importName(decl->getDeclName());
      else if (decl->getTypedefNameForAnonDecl())
        name =Impl.importName(decl->getTypedefNameForAnonDecl()->getDeclName());

      if (name.empty())
        return nullptr;

      auto dc = Impl.importDeclContext(decl->getDeclContext());
      if (!dc)
        return nullptr;

      // Create the oneof declaration and record it.
      auto result = new (Impl.SwiftContext)
                      OneOfDecl(Impl.importSourceLoc(decl->getLocStart()),
                                name,
                                Impl.importSourceLoc(decl->getLocation()),
                                { }, nullptr, dc);
      Impl.ImportedDecls[decl->getCanonicalDecl()] = result;

      // Import each of the enumerators.
      SmallVector<Decl *, 4> members;
      for (auto ec = decl->enumerator_begin(), ecEnd = decl->enumerator_end();
           ec != ecEnd; ++ec) {
        auto ood = Impl.importDecl(*ec);
        if (!ood)
          continue;

        members.push_back(ood);
      }

      // FIXME: Source range isn't totally accurate because Clang lacks the
      // location of the '{'.
      result->setMembers(Impl.SwiftContext.AllocateCopy(members),
                         Impl.importSourceRange(clang::SourceRange(
                                                  decl->getLocation(),
                                                  decl->getRBraceLoc())));

      return result;
    }

    Decl *VisitRecordDecl(clang::RecordDecl *decl) {
      // FIXME: Skip unions for now. We can't properly map them to oneofs,
      // because they aren't descriminated in any way. We could map them to
      // structs, but that would make them very, very unsafe to use.
      if (decl->isUnion())
        return nullptr;

      // FIXME: Skip Microsoft __interfaces.
      if (decl->isInterface())
        return nullptr;

      // The types of anonymous structs or unions are never imported; their
      // fields are dumped directly into the enclosing class.
      if (decl->isAnonymousStructOrUnion())
        return nullptr;

      // FIXME: Figure out how to deal with incomplete types, since that
      // notion doesn't exist in Swift.
      decl = decl->getDefinition();
      if (!decl)
        return nullptr;

      Identifier name;
      if (decl->getDeclName())
        name = Impl.importName(decl->getDeclName());
      else if (decl->getTypedefNameForAnonDecl())
        name =Impl.importName(decl->getTypedefNameForAnonDecl()->getDeclName());

      if (name.empty())
        return nullptr;

      auto dc = Impl.importDeclContext(decl->getDeclContext());
      if (!dc)
        return nullptr;

      // Create the struct declaration and record it.
      auto result = new (Impl.SwiftContext)
                      StructDecl(Impl.importSourceLoc(decl->getLocStart()),
                                 name,
                                 Impl.importSourceLoc(decl->getLocation()),
                                 { }, nullptr, dc);
      Impl.ImportedDecls[decl->getCanonicalDecl()] = result;

      // FIXME: Figure out what to do with base classes in C++. One possible
      // solution would be to turn them into members and add conversion
      // functions.

      // Import each of the members.
      SmallVector<Decl *, 4> members;
      for (auto m = decl->decls_begin(), mEnd = decl->decls_end();
           m != mEnd; ++m) {
        auto nd = dyn_cast<clang::NamedDecl>(*m);
        if (!nd)
          continue;

        // Skip anonymous structs or unions; they'll be dealt with via the
        // IndirectFieldDecls.
        if (auto field = dyn_cast<clang::FieldDecl>(nd))
          if (field->isAnonymousStructOrUnion())
            continue;

        auto member = Impl.importDecl(nd);
        if (!member)
          continue;

        members.push_back(member);
      }

      // FIXME: Source range isn't totally accurate because Clang lacks the
      // location of the '{'.
      result->setMembers(Impl.SwiftContext.AllocateCopy(members),
                         Impl.importSourceRange(clang::SourceRange(
                                                  decl->getLocation(),
                                                  decl->getRBraceLoc())));

      return result;
    }

    Decl *VisitClassTemplateSpecializationDecl(
                 clang::ClassTemplateSpecializationDecl *decl) {
      // FIXME: We could import specializations, but perhaps only as unnamed
      // structural types.
      return nullptr;
    }

    Decl *VisitClassTemplatePartialSpecializationDecl(
                 clang::ClassTemplatePartialSpecializationDecl *decl) {
      // Note: templates are not imported.
      return nullptr;
    }

    Decl *VisitTemplateTypeParmDecl(clang::TemplateTypeParmDecl *decl) {
      // Note: templates are not imported.
      return nullptr;
    }

    Decl *VisitEnumConstantDecl(clang::EnumConstantDecl *decl) {
      auto name = Impl.importName(decl->getDeclName());
      if (name.empty())
        return nullptr;

      auto dc = Impl.importDeclContext(decl->getDeclContext());
      if (!dc)
        return nullptr;

      auto result
        = new (Impl.SwiftContext)
            OneOfElementDecl(Impl.importSourceLoc(decl->getLocation()),
                             name, TypeLoc(), dc);

      // Give the oneof element the appropriate type.
      auto oneof = cast<OneOfDecl>(dc);
      auto argTy = MetaTypeType::get(oneof->getDeclaredType(),
                                     Impl.SwiftContext);
      result->overwriteType(FunctionType::get(argTy, oneof->getDeclaredType(),
                                              Impl.SwiftContext));
      return result;
    }

    Decl *
    VisitUnresolvedUsingValueDecl(clang::UnresolvedUsingValueDecl *decl) {
      // Note: templates are not imported.
      return nullptr;
    }

    Decl *VisitIndirectFieldDecl(clang::IndirectFieldDecl *decl) {
      // Check whether the context of any of the fields in the chain is a
      // union. If so, don't import this field.
      for (auto f = decl->chain_begin(), fEnd = decl->chain_end(); f != fEnd;
           ++f) {
        if (auto record = dyn_cast<clang::RecordDecl>((*f)->getDeclContext())) {
          if (record->isUnion())
            return nullptr;
        }
      }

      auto name = Impl.importName(decl->getDeclName());
      if (name.empty())
        return nullptr;

      auto type = Impl.importType(decl->getType());
      if (!type)
        return nullptr;

      auto dc = Impl.importDeclContext(decl->getDeclContext());
      if (!dc)
        return nullptr;

      // Map this indirect field to a Swift variable.
      return new (Impl.SwiftContext)
               VarDecl(Impl.importSourceLoc(decl->getLocStart()),
                       name, type, dc);
    }

    /// \brief Set the declaration context of each variable within the given
    /// patterns to \p dc.
    static void setVarDeclContexts(ArrayRef<Pattern *> patterns,
                                   DeclContext *dc) {
      for (auto pattern : patterns) {
        auto pat = pattern->getSemanticsProvidingPattern();
        if (auto named = dyn_cast<NamedPattern>(pat))
          named->getDecl()->setDeclContext(dc);
        if (auto tuple = dyn_cast<TuplePattern>(pat)) {
          for (auto elt : tuple->getFields())
            setVarDeclContexts(elt.getPattern(), dc);
        }
      }
    }

    Decl *VisitFunctionDecl(clang::FunctionDecl *decl) {
      // FIXME: We can't IRgen inline functions, so don't import them.
      if (decl->isInlined() || decl->hasAttr<clang::AlwaysInlineAttr>()) {
        return nullptr;
      }

      auto dc = Impl.importDeclContext(decl->getDeclContext());
      if (!dc)
        return nullptr;

      // Import the function type. If we have parameters, make sure their names
      // get into the resulting function type.
      SmallVector<Pattern *, 4> argPatterns;
      SmallVector<Pattern *, 4> bodyPatterns;
      Type type;
      if (decl->param_size())
        type = Impl.importFunctionType(
                 decl->getType()->getAs<clang::FunctionType>()->getResultType(),
                 { decl->param_begin(), decl->param_size() },
                 decl->isVariadic(), argPatterns, bodyPatterns);
      else
        type = Impl.importType(decl->getType());

      if (!type)
        return nullptr;

      auto resultTy = type->castTo<FunctionType>()->getResult();
      auto loc = Impl.importSourceLoc(decl->getLocation());

      auto name = Impl.importName(decl->getDeclName());
      if (name.empty())
        return nullptr;

      // FIXME: Poor location info.
      auto funcExpr = FuncExpr::create(Impl.SwiftContext, loc,
                                       argPatterns, bodyPatterns,
                                       TypeLoc(resultTy, loc),
                                       nullptr, dc);
      funcExpr->setType(type);
      auto nameLoc = Impl.importSourceLoc(decl->getLocation());
      auto result = new (Impl.SwiftContext) FuncDecl(SourceLoc(), loc,
                                                     name, nameLoc,
                                                     /*GenericParams=*/0,
                                                     type, funcExpr,
                                                     dc);
      setVarDeclContexts(argPatterns, funcExpr);
      setVarDeclContexts(bodyPatterns, funcExpr);
      return result;
    }

    Decl *VisitCXXMethodDecl(clang::CXXMethodDecl *decl) {
      // FIXME: Import C++ member functions as methods.
      return nullptr;
    }

    Decl *VisitFieldDecl(clang::FieldDecl *decl) {
      // Fields are imported as variables.
      auto name = Impl.importName(decl->getDeclName());
      if (name.empty())
        return nullptr;

      auto type = Impl.importType(decl->getType());
      if (!type)
        return nullptr;

      auto dc = Impl.importDeclContext(decl->getDeclContext());
      if (!dc)
        return nullptr;

      auto result = new (Impl.SwiftContext)
                      VarDecl(Impl.importSourceLoc(decl->getLocation()),
                              name, type, dc);

      // Handle attributes.
      if (decl->hasAttr<clang::IBOutletAttr>())
        result->getMutableAttrs().IBOutlet = true;
      // FIXME: Handle IBOutletCollection.

      return result;
    }

    Decl *VisitObjCIvarDecl(clang::ObjCIvarDecl *decl) {
      // FIXME: Deal with fact that a property and an ivar can have the same
      // name.
      return VisitFieldDecl(decl);
    }

    Decl *VisitObjCAtDefsFieldDecl(clang::ObjCAtDefsFieldDecl *decl) {
      // @defs is an anachronism; ignore it.
      return nullptr;
    }

    Decl *VisitVarDecl(clang::VarDecl *decl) {
      // FIXME: Swift does not have static variables in structs/classes yet.
      if (decl->getDeclContext()->isRecord())
        return nullptr;

      // Variables are imported as... variables.
      auto name = Impl.importName(decl->getDeclName());
      if (name.empty())
        return nullptr;

      auto type = Impl.importType(decl->getType());
      if (!type)
        return nullptr;

      auto dc = Impl.importDeclContext(decl->getDeclContext());
      if (!dc)
        return nullptr;

      return new (Impl.SwiftContext)
               VarDecl(Impl.importSourceLoc(decl->getLocation()),
                       name, type, dc);
    }

    Decl *VisitImplicitParamDecl(clang::ImplicitParamDecl *decl) {
      // Parameters are never directly imported.
      return nullptr;
    }

    Decl *VisitParmVarDecl(clang::ParmVarDecl *decl) {
      // Parameters are never directly imported.
      return nullptr;
    }

    Decl *
    VisitNonTypeTemplateParmDecl(clang::NonTypeTemplateParmDecl *decl) {
      // Note: templates are not imported.
      return nullptr;
    }

    Decl *VisitTemplateDecl(clang::TemplateDecl *decl) {
      // Note: templates are not imported.
      return nullptr;
    }

    Decl *VisitUsingDecl(clang::UsingDecl *decl) {
      // Using declarations are not imported.
      return nullptr;
    }

    Decl *VisitUsingShadowDecl(clang::UsingShadowDecl *decl) {
      // Using shadow declarations are not imported; rather, name lookup just
      // looks through them.
      return nullptr;
    }

    Decl *VisitObjCMethodDecl(clang::ObjCMethodDecl *decl) {
      auto loc = Impl.importSourceLoc(decl->getLocStart());
      auto dc = Impl.importDeclContext(decl->getDeclContext());
      if (!dc)
        return nullptr;

      // The name of the method is the first part of the selector.
      auto name
        = Impl.importName(decl->getSelector().getIdentifierInfoForSlot(0));
      if (name.empty())
        return nullptr;

      // Figure out the type of the container.
      auto containerTy = dc->getDeclaredTypeOfContext();
      assert(containerTy && "Method in non-type context?");

      // Add the implicit 'this' parameter patterns.
      SmallVector<Pattern *, 4> argPatterns;
      SmallVector<Pattern *, 4> bodyPatterns;
      auto thisTy = containerTy;
      if (decl->isClassMethod())
        thisTy = MetaTypeType::get(thisTy, Impl.SwiftContext);
      auto thisName = Impl.SwiftContext.getIdentifier("this");
      auto thisVar = new (Impl.SwiftContext) VarDecl(SourceLoc(), thisName,
                                                     thisTy,
                                                     Impl.firstClangModule);
      Pattern *thisPat = new (Impl.SwiftContext) NamedPattern(thisVar);
      thisPat->setType(thisVar->getType());
      thisPat
        = new (Impl.SwiftContext) TypedPattern(thisPat,
                                               TypeLoc(thisTy, loc));
      thisPat->setType(thisVar->getType());
      argPatterns.push_back(thisPat);
      bodyPatterns.push_back(thisPat);
      
      // Import the type that this method will have.
      auto type = Impl.importFunctionType(decl->getResultType(),
                                          { decl->param_begin(),
                                            decl->param_size() },
                                          decl->isVariadic(),
                                          argPatterns,
                                          bodyPatterns,
                                          decl->getSelector());
      if (!type)
        return nullptr;

      auto resultTy = type->castTo<FunctionType>()->getResult();

      // Add the 'this' parameter to the function type.
      type = FunctionType::get(thisTy, type, Impl.SwiftContext);

      // FIXME: Related result type? Not so important when we get constructors
      // working.

      // FIXME: Poor location info.
      auto nameLoc = Impl.importSourceLoc(decl->getLocation());
      auto funcExpr = FuncExpr::create(Impl.SwiftContext, loc,
                                       argPatterns, bodyPatterns,
                                       TypeLoc(resultTy, loc),
                                       nullptr, dc);
      funcExpr->setType(type);

      auto result = new (Impl.SwiftContext) FuncDecl(SourceLoc(), loc,
                                                     name, nameLoc,
                                                     /*GenericParams=*/0,
                                                     type, funcExpr, dc);

      setVarDeclContexts(argPatterns, funcExpr);
      setVarDeclContexts(bodyPatterns, funcExpr);

      // Mark this as an Objective-C method.
      result->getMutableAttrs().ObjC = true;

      // Mark class methods as static.
      if (decl->isClassMethod())
        result->setStatic();

      // If this method overrides another method, mark it as such.
      // FIXME: We'll eventually have to deal with having multiple overrides
      // in Swift.
      if (decl->isOverriding()) {
        SmallVector<const clang::ObjCMethodDecl *, 2> overridden;
        decl->getOverriddenMethods(overridden);
        clang::ObjCMethodDecl *superResult = nullptr;
        clang::ObjCMethodDecl *categoryResult = nullptr;
        for (auto ov : overridden) {
          if (isa<clang::ObjCInterfaceDecl>(ov->getDeclContext()))
            superResult = const_cast<clang::ObjCMethodDecl *>(ov);
          else if (isa<clang::ObjCCategoryDecl>(ov->getDeclContext()))
            categoryResult = const_cast<clang::ObjCMethodDecl *>(ov);
        }

        if (superResult)
          result->setOverriddenDecl(
            cast_or_null<FuncDecl>(Impl.importDecl(superResult)));
        else if (categoryResult)
          result->setOverriddenDecl(
            cast_or_null<FuncDecl>(Impl.importDecl(categoryResult)));
      }

      // Handle attributes.
      if (decl->hasAttr<clang::IBActionAttr>())
        result->getMutableAttrs().IBAction = true;

      return result;
    }

  private:
    /// \brief Given an imported method, try to import it as some kind of
    /// special declaration, e.g., a constructor or subscript.
    Decl *importSpecialMethod(Decl *decl) {
      // Only consider Objective-C methods...
      auto objcMethod
        = dyn_cast_or_null<clang::ObjCMethodDecl>(decl->getClangDecl());
      if (!objcMethod)
        return nullptr;

      switch (objcMethod->getMethodFamily()) {
      case clang::OMF_None:
        // Check for one of the subscripting selectors.
        if (objcMethod->isInstanceMethod() &&
            (objcMethod->getSelector() == Impl.objectAtIndexedSubscript ||
             objcMethod->getSelector() == Impl.setObjectAtIndexedSubscript ||
             objcMethod->getSelector() == Impl.objectForKeyedSubscript ||
             objcMethod->getSelector() == Impl.setObjectForKeyedSubscript))
          return importSubscript(decl, objcMethod);
          
        return nullptr;

      case clang::OMF_new:
      case clang::OMF_alloc:
      case clang::OMF_autorelease:
      case clang::OMF_copy:
      case clang::OMF_dealloc:
      case clang::OMF_finalize:
      case clang::OMF_mutableCopy:
      case clang::OMF_performSelector:
      case clang::OMF_release:
      case clang::OMF_retain:
      case clang::OMF_retainCount:
      case clang::OMF_self:
        // None of these methods have special consideration.
        return nullptr;

      case clang::OMF_init:
        // An init instance method can be a constructor.
        if (objcMethod->isInstanceMethod())
          return importConstructor(decl, objcMethod);
        return nullptr;
      }
    }

    /// \brief Given an imported method, try to import it as a constructor.
    ///
    /// Objective-C methods in the 'init' and 'new' family are imported as
    /// constructors in Swift, enabling the 'new' syntax, e.g.,
    ///
    /// \code
    /// new NSArray(1024) // same as NSArray.alloc.initWithCapacity:1024
    /// \endcode
    ConstructorDecl *importConstructor(Decl *decl,
                                       clang::ObjCMethodDecl *objcMethod) {
      // Figure out the type of the container.
      auto dc = decl->getDeclContext();
      auto containerTy = dc->getDeclaredTypeOfContext();
      assert(containerTy && "Method in non-type context?");

      // Make sure that NSObject is a supertype of the container.
      // FIXME: This is a hack because we don't have a suitable 'top' type for
      // Objective-C classes.
      auto checkTy = containerTy;
      do {
        auto classDecl = checkTy->getClassOrBoundGenericClass();
        if (!classDecl) {
          return nullptr;
        }

        if (classDecl->getName().str() == "NSObject")
          break;

        checkTy = classDecl->getBaseClass();
        if (!checkTy)
          return nullptr;
      } while (true);

      // Only methods in the 'init' and 'new' family can become constructors.
      FuncDecl *alloc = nullptr;
      switch (objcMethod->getMethodFamily()) {
      case clang::OMF_alloc:
      case clang::OMF_autorelease:
      case clang::OMF_copy:
      case clang::OMF_dealloc:
      case clang::OMF_finalize:
      case clang::OMF_mutableCopy:
      case clang::OMF_None:
      case clang::OMF_performSelector:
      case clang::OMF_release:
      case clang::OMF_retain:
      case clang::OMF_retainCount:
      case clang::OMF_self:
        llvm_unreachable("Caller did not filter non-constructor methods");

      case clang::OMF_init: {
        // Make sure we have a usable 'alloc' method. Otherwise, we can't
        // build this constructor anyway.
        // FIXME: Can we do this for protocol methods as well? Do we want to?
        auto interface = objcMethod->getClassInterface();
        if (!interface)
          return nullptr;

        // Form the Objective-C selector for alloc.
        auto &clangContext = Impl.getClangASTContext();
        auto allocId = &clangContext.Idents.get("alloc");
        auto allocSel = clangContext.Selectors.getNullarySelector(allocId);

        // Find the 'alloc' class method.
        auto allocMethod = interface->lookupClassMethod(allocSel);
        if (!allocMethod)
          return nullptr;

        // Import the 'alloc' class method.
        alloc = cast_or_null<FuncDecl>(Impl.importDecl(allocMethod));
        if (!alloc)
          return nullptr;
        break;
      }

      case clang::OMF_new:
        assert(objcMethod->isClassMethod() && "Caller did not filter inputs");
        break;
      }

      // FIXME: Hack.
      auto loc = decl->getLoc();
      auto name = Impl.SwiftContext.getIdentifier("constructor");

      // Add the implicit 'this' parameter patterns.
      SmallVector<Pattern *, 4> argPatterns;
      SmallVector<Pattern *, 4> bodyPatterns;
      auto thisTy = containerTy;
      auto thisMetaTy = MetaTypeType::get(containerTy, Impl.SwiftContext);
      auto thisName = Impl.SwiftContext.getIdentifier("this");
      auto thisMetaVar = new (Impl.SwiftContext) VarDecl(SourceLoc(), thisName,
                                                         thisMetaTy,
                                                         Impl.firstClangModule);
      Pattern *thisPat = new (Impl.SwiftContext) NamedPattern(thisMetaVar);
      thisPat->setType(thisMetaTy);
      thisPat
        = new (Impl.SwiftContext) TypedPattern(thisPat,
                                               TypeLoc(thisMetaTy, loc));
      thisPat->setType(thisMetaTy);

      argPatterns.push_back(thisPat);
      bodyPatterns.push_back(thisPat);

      // Import the type that this method will have.
      auto type = Impl.importFunctionType(objcMethod->getResultType(),
                                          { objcMethod->param_begin(),
                                            objcMethod->param_size() },
                                          objcMethod->isVariadic(),
                                          argPatterns,
                                          bodyPatterns,
                                          objcMethod->getSelector(),
                                          /*isConstructor=*/true);
      assert(type && "Type has already been successfully converted?");

      // A constructor returns an object of the type, not 'id'.
      // This is effectively implementing related-result-type semantics.
      // FIXME: Perhaps actually check whether the routine has a related result
      // type?
      type = FunctionType::get(type->castTo<FunctionType>()->getInput(),
                               thisTy, Impl.SwiftContext);

      // Add the 'this' parameter to the function type.
      type = FunctionType::get(thisMetaTy, type, Impl.SwiftContext);

      VarDecl *thisVar = new (Impl.SwiftContext) VarDecl(SourceLoc(),
                                                          thisName, thisTy, dc);

      // Create the actual constructor.
      // FIXME: Losing body patterns here.
      auto result = new (Impl.SwiftContext) ConstructorDecl(name, loc,
                                                            argPatterns.back(),
                                                            thisVar,
                                                            /*GenericParams=*/0,
                                                            dc);
      result->getMutableAttrs().AllocatesThis = true;
      result->setType(type);
      thisVar->setDeclContext(result);
      setVarDeclContexts(argPatterns, result);
      setVarDeclContexts(bodyPatterns, result);

      // Create the body of the constructor, which will call the appropriate
      // underlying method (and 'alloc', if needed).
      // FIXME: Use the 'this' of metaclass type rather than a metatype
      // expression.
      Expr* initExpr = new (Impl.SwiftContext) MetatypeExpr(nullptr, loc,
                                                            thisMetaTy);

      if (objcMethod->getMethodFamily() == clang::OMF_init) {
        // For an 'init' method, we need to call alloc first.
        Expr *allocRef
          = new (Impl.SwiftContext) DeclRefExpr(alloc, loc,
                                                alloc->getTypeOfReference());

        auto allocCall = new (Impl.SwiftContext) DotSyntaxCallExpr(allocRef,
                                                                   loc,
                                                                   initExpr);
        auto emptyTuple = new (Impl.SwiftContext) TupleExpr(loc, {}, nullptr,
                                                            loc);
        initExpr = new (Impl.SwiftContext) CallExpr(allocCall, emptyTuple);

        // Cast the result of the alloc call to the (metatype) 'this'.
        auto toTypeRef = new (Impl.SwiftContext) MetatypeExpr(nullptr, loc,
                                                              thisMetaTy);
        initExpr = new (Impl.SwiftContext) CallExpr(toTypeRef, initExpr);
      }

      // Form a reference to the actual method.
      auto func = cast<FuncDecl>(decl);
      auto funcRef
        = new (Impl.SwiftContext) DeclRefExpr(func, loc,
                                              func->getTypeOfReference());
      initExpr = new (Impl.SwiftContext) DotSyntaxCallExpr(funcRef, loc,
                                                           initExpr);

      // Form the call arguments.
      SmallVector<Expr *, 2> callArgs;
      auto tuple = dyn_cast<TuplePattern>(argPatterns[1]);
      if (!tuple) {
        // FIXME: We don't want this to be the case. We should always ensure
        // that the body has names, even if the interface does not.
        return nullptr;
      }

      for (auto elt : tuple->getFields()) {
        auto named = dyn_cast<NamedPattern>(
                       elt.getPattern()->getSemanticsProvidingPattern());
        if (!named) {
          // FIXME: We don't want this to be the case. Can we fake up names
          // in the body parameters so this doesn't happen?
          return nullptr;
        }

        // Create a reference to this parameter.
        Expr *ref = new (Impl.SwiftContext) DeclRefExpr(named->getDecl(),
                                                        loc,
                                                        named->getType());

        // If the parameter is [byref], take its address.
        if (named->getDecl()->getType()->is<LValueType>())
          ref = new (Impl.SwiftContext) AddressOfExpr(loc, ref,
                                                      ref->getType());

        callArgs.push_back(ref);
      }

      // Form the method call.
      Expr *callArg;

      if (callArgs.size() == 1) {
        callArg = callArgs[0];
      } else {
        auto callArgsCopy = Impl.SwiftContext.AllocateCopy(callArgs);
        callArg = new (Impl.SwiftContext) TupleExpr(loc, callArgsCopy,
                                                    nullptr, loc);
      }

      initExpr = new (Impl.SwiftContext) CallExpr(initExpr, callArg);

      // Cast the result of the alloc call to the (metatype) 'this'.
      auto toTypeRef = new (Impl.SwiftContext) MetatypeExpr(nullptr, loc,
                                                            thisMetaTy);
      initExpr = new (Impl.SwiftContext) CallExpr(toTypeRef, initExpr);

      // Form the assignment statement.
      auto refThis
        = new (Impl.SwiftContext) DeclRefExpr(thisVar, loc,
                                              thisVar->getTypeOfReference());
      auto assign = new (Impl.SwiftContext) AssignStmt(refThis, loc, initExpr);

      // Set the body of the constructor.
      result->setBody(BraceStmt::create(Impl.SwiftContext, loc,
                                        BraceStmt::ExprStmtOrDecl(assign),
                                        loc));

      // Inform the context that we have external definitions.
      Impl.firstClangModule->addExternalDefinition(result);
      
      return result;
    }

    /// \brief Retrieve the single variable described in the given pattern.
    ///
    /// This routine assumes that the pattern is something very simple
    /// like (x : type) or (x).
    VarDecl *getSingleVar(Pattern *pattern) {
      pattern = pattern->getSemanticsProvidingPattern();
      if (auto tuple = dyn_cast<TuplePattern>(pattern)) {
        pattern = tuple->getFields()[0].getPattern()
                    ->getSemanticsProvidingPattern();
      }

      return cast<NamedPattern>(pattern)->getDecl();
    }

    /// \brief Build a thunk for an Objective-C getter.
    ///
    /// \param getter The Objective-C getter method.
    ///
    /// \param indices If non-null, the indices for a subscript getter. Null
    /// indicates that we're generating a getter thunk for a property getter.
    ///
    /// \returns The getter thunk.
    FuncDecl *buildGetterThunk(FuncDecl *getter, Pattern *indices) {
      auto &context = Impl.SwiftContext;
      auto loc = getter->getLoc();

      // Figure out the element type, by looking through 'this' and the normal
      // parameters.
      auto elementTy
        = getter->getType()->castTo<FunctionType>()->getResult()
            ->castTo<FunctionType>()->getResult();

      // Form the argument patterns.
      llvm::SmallVector<Pattern *, 3> getterArgs;

      // 'this'
      getterArgs.push_back(
        getter->getBody()->getBodyParamPatterns()[0]->clone(context));

      // index, for subscript operations.
      if (indices) {
        // Clone the indices for the thunk.
        indices = indices->clone(context);
        auto pat = TuplePattern::create(context, loc, TuplePatternElt(indices),
                                        loc);
        pat->setType(TupleType::get(TupleTypeElt(indices->getType(),
                                                 indices->getBoundName()),
                                    context));
        getterArgs.push_back(pat);
      }

      // empty tuple
      getterArgs.push_back(TuplePattern::create(context, loc, { }, loc));
      getterArgs.back()->setType(TupleType::getEmpty(context));

      // Form the type of the getter.
      auto getterType = elementTy;
      for (auto it = getterArgs.rbegin(), itEnd = getterArgs.rend();
           it != itEnd; ++it) {
        getterType = FunctionType::get((*it)->getType(),
                                       getterType,
                                       context);
      }

      // Create the getter body.
      auto funcExpr = FuncExpr::create(context, getter->getLoc(),
                                       getterArgs,
                                       getterArgs,
                                       TypeLoc(elementTy, loc),
                                       nullptr,
                                       getter->getDeclContext());
      funcExpr->setType(getterType);
      setVarDeclContexts(getterArgs, funcExpr);

      // Create the getter thunk.
      auto thunk = new (context) FuncDecl(SourceLoc(), getter->getLoc(),
                                          Identifier(), SourceLoc(), nullptr,
                                          getterType, funcExpr,
                                          getter->getDeclContext());

      // Create the body of the thunk, which calls the Objective-C getter.
      auto thisVar = getSingleVar(getterArgs[0]);

      auto thisRef = new (context) DeclRefExpr(thisVar, loc,
                                               thisVar->getTypeOfReference());
      auto getterRef
        = new (context) DeclRefExpr(getter, loc,
                                    getter->getTypeOfReference());

      // First, bind 'this' to the method.
      Expr *call = new (context) DotSyntaxCallExpr(getterRef, loc, thisRef);

      // Call the method itself.
      if (indices) {
        // For a subscript, pass the index.
        auto indexVar = getSingleVar(getterArgs[1]);
        auto indexRef
          = new (context) DeclRefExpr(indexVar, loc,
                                      indexVar->getTypeOfReference());
        call = new (context) CallExpr(call, indexRef);
      } else {
        // For a property, call with no arguments.
        auto emptyTuple = new (context) TupleExpr(loc, { }, nullptr, loc);
        call = new (context) CallExpr(call, emptyTuple);
      }

      // Create the return statement.
      auto ret = new (context) ReturnStmt(loc, call);

      // Finally, set the body.
      funcExpr->setBody(BraceStmt::create(context, loc,
                                          BraceStmt::ExprStmtOrDecl(ret),
                                          loc));

      // Register this thunk as an external definition.
      Impl.firstClangModule->addExternalDefinition(thunk);
      
      return thunk;
    }

    /// \brief Build a thunk for an Objective-C setter.
    ///
    /// \param setter The Objective-C setter method.
    ///
    /// \param indices If non-null, the indices for a subscript setter. Null
    /// indicates that we're generating a setter thunk for a property setter.
    ///
    /// \returns The getter thunk.
    FuncDecl *buildSetterThunk(FuncDecl *setter, Pattern *indices) {
      auto &context = Impl.SwiftContext;
      auto loc = setter->getLoc();
      auto tuple = cast<TuplePattern>(
                     setter->getBody()->getBodyParamPatterns()[1]);

      // Objective-C subscript setters are imported with a function type
      // such as:
      //
      //   (this) -> (value, index) -> ()
      //
      // while Swift subscript setters are curried as
      //
      //   (this) -> (index)(value) -> ()
      //
      // Build a setter thunk with the latter signature that maps to the
      // former.
      //
      // Property setters are similar, but don't have indices.

      // Form the argument patterns.
      llvm::SmallVector<Pattern *, 3> setterArgs;

      // 'this'
      setterArgs.push_back(
        setter->getBody()->getBodyParamPatterns()[0]->clone(context));

      // index, for subscript operations.
      if (indices) {
        // Clone the indices for the thunk.
        indices = indices->clone(context);
        auto pat = TuplePattern::create(context, loc, TuplePatternElt(indices),
                                        loc);
        pat->setType(TupleType::get(TupleTypeElt(indices->getType(),
                                                 indices->getBoundName()),
                                    context));
        setterArgs.push_back(pat);
      }

      // value
      auto valuePattern = tuple->getFields()[0].getPattern()->clone(context);
      setterArgs.push_back(TuplePattern::create(context, loc,
                                                TuplePatternElt(valuePattern),
                                                loc));
      setterArgs.back()->setType(
        TupleType::get(TupleTypeElt(valuePattern->getType(),
                                    valuePattern->getBoundName()),
                       context));

      // Form the type of the setter.
      auto setterType = TupleType::getEmpty(context);
      for (auto it = setterArgs.rbegin(), itEnd = setterArgs.rend();
           it != itEnd; ++it) {
        setterType = FunctionType::get((*it)->getType(),
                                       setterType,
                                       context);
      }


      // Create the setter body.
      auto funcExpr = FuncExpr::create(context, setter->getLoc(),
                                       setterArgs,
                                       setterArgs,
                                       TypeLoc(TupleType::getEmpty(context),
                                               loc),
                                       nullptr,
                                       setter->getDeclContext());
      funcExpr->setType(setterType);
      setVarDeclContexts(setterArgs, funcExpr);

      // Create the setter thunk.
      auto thunk = new (context) FuncDecl(SourceLoc(), setter->getLoc(),
                                          Identifier(), SourceLoc(), nullptr,
                                          setterType, funcExpr,
                                          setter->getDeclContext());

      // Create the body of the thunk, which calls the Objective-C setter.
      auto thisVar = getSingleVar(setterArgs[0]);
      auto valueVar = getSingleVar(setterArgs.back());

      auto thisRef = new (context) DeclRefExpr(thisVar, loc,
                                               thisVar->getTypeOfReference());
      auto valueRef
        = new (context) DeclRefExpr(valueVar, loc,
                                    valueVar->getTypeOfReference());
      auto setterRef
        = new (context) DeclRefExpr(setter, loc,
                                    setter->getTypeOfReference());

      // First, bind 'this' to the method.
      Expr *call = new (context) DotSyntaxCallExpr(setterRef, loc, thisRef);

      // Next, call the Objective-C setter.
      Expr *callArgs;
      if (indices) {
        // For subscript setters, we have both value and index.
        auto indexVar = getSingleVar(setterArgs[1]);
        auto indexRef
          = new (context) DeclRefExpr(indexVar, loc,
                                      indexVar->getTypeOfReference());

        Expr *callArgsArray[2] = { valueRef, indexRef };
        callArgs
          = new (context) TupleExpr(loc,
                                    context.AllocateCopy(
                                      MutableArrayRef<Expr*>(callArgsArray)),
                                  nullptr, loc);
      } else {
        callArgs = valueRef;
      }
      call = new (context) CallExpr(call, callArgs);

      // Finally, set the body.
      funcExpr->setBody(BraceStmt::create(context, loc,
                                          BraceStmt::ExprStmtOrDecl(call),
                                          loc));

      // Register this thunk as an external definition.
      Impl.firstClangModule->addExternalDefinition(thunk);

      return thunk;
    }
    
    /// \brief Given either the getter or setter for a subscript operation,
    /// create the Swift subscript declaration.
    SubscriptDecl *importSubscript(Decl *decl,
                                   clang::ObjCMethodDecl *objcMethod) {
      assert(objcMethod->isInstanceMethod() && "Caller must filter");

      // Make sure we have a usable 'alloc' method. Otherwise, we can't
      // build this constructor anyway.
      // FIXME: Can we do this for protocol methods as well? Do we want to?
      auto interface = objcMethod->getClassInterface();
      if (!interface)
        return nullptr;

      FuncDecl *getter = nullptr, *setter = nullptr;
      if (objcMethod->getSelector() == Impl.objectAtIndexedSubscript) {
        getter = cast<FuncDecl>(decl);

        // Find the setter
        if (auto objcSetter = interface->lookupInstanceMethod(
                                Impl.setObjectAtIndexedSubscript)) {
          setter = cast_or_null<FuncDecl>(Impl.importDecl(objcSetter));

          // Don't allow static setters.
          if (setter && setter->isStatic())
            setter = nullptr;
        }
      } else if (objcMethod->getSelector() == Impl.setObjectAtIndexedSubscript){
        setter = cast<FuncDecl>(decl);

        // Find the getter.
        if (auto objcGetter = interface->lookupInstanceMethod(
                                Impl.objectAtIndexedSubscript)) {
          getter = cast_or_null<FuncDecl>(Impl.importDecl(objcGetter));

          // Don't allow static getters.
          if (getter && getter->isStatic())
            return nullptr;
        }

        // FIXME: Swift doesn't have write-only subscripting.
        if (!getter)
          return nullptr;
      } else if (objcMethod->getSelector() == Impl.objectForKeyedSubscript) {
        getter = cast<FuncDecl>(decl);

        // Find the setter
        if (auto objcSetter = interface->lookupInstanceMethod(
                                Impl.setObjectForKeyedSubscript)) {
          setter = cast_or_null<FuncDecl>(Impl.importDecl(objcSetter));

          // Don't allow static setters.
          if (setter && setter->isStatic())
            setter = nullptr;
        }
      } else if (objcMethod->getSelector() == Impl.setObjectForKeyedSubscript) {
        setter = cast<FuncDecl>(decl);

        // Find the getter.
        if (auto objcGetter = interface->lookupInstanceMethod(
                                Impl.objectForKeyedSubscript)) {
          getter = cast_or_null<FuncDecl>(Impl.importDecl(objcGetter));

          // Don't allow static getters.
          if (getter && getter->isStatic())
            return nullptr;
        }

        // FIXME: Swift doesn't have write-only subscripting.
        if (!getter)
          return nullptr;

      } else {
        llvm_unreachable("Unknown getter/setter selector");
      }

      // Check whether we've already created a subscript operation for
      // this getter/setter pair.
      if (Impl.Subscripts[{getter, setter}])
        return nullptr;

      // Compute the element type, looking through the implicit 'this'
      // parameter and the normal function parameters.
      auto elementTy
        = getter->getType()->castTo<AnyFunctionType>()->getResult()
            ->castTo<AnyFunctionType>()->getResult();

      // Check the form of the getter.
      FuncDecl *getterThunk = nullptr;
      auto &context = Impl.SwiftContext;
      {
        auto tuple = dyn_cast<TuplePattern>(
                       getter->getBody()->getArgParamPatterns()[1]);
        if (tuple && tuple->getFields().size() != 1)
          return nullptr;

        auto indices = tuple->getFields()[0].getPattern();
        getterThunk = buildGetterThunk(getter, indices);
      }

      // Check the form of the setter.
      FuncDecl *setterThunk = nullptr;
      if (setter) {
        auto tuple = dyn_cast<TuplePattern>(
                       setter->getBody()->getBodyParamPatterns()[1]);
        if (!tuple)
          return nullptr;

        if (tuple->getFields().size() != 2)
          return nullptr;

        // The setter must accept elements of the same type as the getter
        // returns.
        // FIXME: Adjust C++ references?
        auto setterElementTy = tuple->getFields()[0].getPattern()->getType();
        if (!elementTy->isEqual(setterElementTy))
          return nullptr;

        auto indices = tuple->getFields()[1].getPattern();
        setterThunk = buildSetterThunk(setter, indices);
      }

      // Build the subscript declaration.
      auto loc = decl->getLoc();
      auto dc = decl->getDeclContext();
      auto argPatterns
        = getterThunk->getBody()->getArgParamPatterns()[1]->clone(context);
      auto subscript
        = new (context) SubscriptDecl(
                          context.getIdentifier("__subscript"),
                          decl->getLoc(),
                          argPatterns,
                          decl->getLoc(),
                          TypeLoc(elementTy, loc),
                          SourceRange(), getterThunk, setterThunk, dc);
      setVarDeclContexts(argPatterns, subscript->getDeclContext());

      subscript->setType(FunctionType::get(subscript->getIndices()->getType(),
                                           subscript->getElementType(),
                                           context));
      getterThunk->makeGetter(subscript);
      if (setterThunk)
        setterThunk->makeSetter(subscript);

      // FIXME: Figure out overriding.

      // Note that we've created this subscript.
      Impl.Subscripts[{getter, setter}] = subscript;
      return subscript;
    }

  public:
    Decl *VisitObjCCategoryDecl(clang::ObjCCategoryDecl *decl) {
      // Objective-C categories and extensions map to Swift extensions.

      // Find the Swift class being extended.
      auto objcClass
        = cast_or_null<ClassDecl>(Impl.importDecl(decl->getClassInterface()));
      if (!objcClass)
        return nullptr;

      auto dc = Impl.importDeclContext(decl->getDeclContext());
      if (!dc)
        return nullptr;

      // FIXME: Import protocols, add them to 'inherited' list.
      
      // Create the extension declaration and record it.
      auto loc = Impl.importSourceLoc(decl->getLocStart());
      auto result
        = new (Impl.SwiftContext)
            ExtensionDecl(loc, TypeLoc(objcClass->getDeclaredType(), loc),
                          { }, dc);
      Impl.ImportedDecls[decl->getCanonicalDecl()] = result;

      // Import each of the members.
      SmallVector<Decl *, 4> members;
      for (auto m = decl->decls_begin(), mEnd = decl->decls_end();
           m != mEnd; ++m) {
        auto nd = dyn_cast<clang::NamedDecl>(*m);
        if (!nd)
          continue;

        auto member = Impl.importDecl(nd);
        if (!member)
          continue;

        // If this member is a method that is a getter or setter for a property
        // that was imported, don't add it to the list of members so it won't
        // be found by name lookup. This eliminates the ambiguity between
        // property names and getter names (by choosing to only have a
        // variable).
        if (auto objcMethod = dyn_cast<clang::ObjCMethodDecl>(nd)) {
          if (auto property = objcMethod->findPropertyDecl())
            if (Impl.importDecl(
                  const_cast<clang::ObjCPropertyDecl *>(property)))
              continue;

          // If there is a special declaration associated with this member,
          // add it now.
          if (auto special = importSpecialMethod(member)) {
            members.push_back(special);
          }
        }

        members.push_back(member);
      }

      // FIXME: Source range isn't accurate.
      result->setMembers(Impl.SwiftContext.AllocateCopy(members),
                         Impl.importSourceRange(clang::SourceRange(
                                                  decl->getLocation(),
                                                  decl->getLocEnd())));

      return result;
    }

    // FIXME: ObjCProtocolDecl

    Decl *VisitObjCInterfaceDecl(clang::ObjCInterfaceDecl *decl) {
      // FIXME: Figure out how to deal with incomplete types, since that
      // notion doesn't exist in Swift.
      decl = decl->getDefinition();
      if (!decl)
        return nullptr;

      auto name = Impl.importName(decl->getDeclName());
      if (name.empty())
        return nullptr;

      auto dc = Impl.importDeclContext(decl->getDeclContext());
      if (!dc)
        return nullptr;

      // FIXME: Import the protocols that this class conforms to. There's
      // a minor, annoying problem here because those protocols might mention
      // this class before we've had a chance to build it (due to forward
      // declarations). The same issue occurs with the superclass...

      // Create the class declaration and record it.
      auto result = new (Impl.SwiftContext)
                      ClassDecl(Impl.importSourceLoc(decl->getLocStart()),
                                name,
                                Impl.importSourceLoc(decl->getLocation()),
                                { }, nullptr, dc);
      Impl.ImportedDecls[decl->getCanonicalDecl()] = result;

      // If this Objective-C class has a supertype, import it.
      if (auto objcSuper = decl->getSuperClass()) {
        auto super = cast_or_null<ClassDecl>(Impl.importDecl(objcSuper));
        if (!super)
          return nullptr;

        TypeLoc superTy(super->getDeclaredType(),
                        Impl.importSourceRange(decl->getSuperClassLoc()));
        result->setBaseClassLoc(superTy);
      }

      // Note that this is an Objective-C class.
      result->getMutableAttrs().ObjC = true;
      
      // Import each of the members.
      SmallVector<Decl *, 4> members;
      for (auto m = decl->decls_begin(), mEnd = decl->decls_end();
           m != mEnd; ++m) {
        auto nd = dyn_cast<clang::NamedDecl>(*m);
        if (!nd)
          continue;

        auto member = Impl.importDecl(nd);
        if (!member)
          continue;

        // If this member is a method that is a getter or setter for a property
        // that was imported, don't add it to the list of members so it won't
        // be found by name lookup. This eliminates the ambiguity between
        // property names and getter names (by choosing to only have a
        // variable).
        if (auto objcMethod = dyn_cast<clang::ObjCMethodDecl>(nd)) {
          if (auto property = objcMethod->findPropertyDecl())
            if (Impl.importDecl(
                  const_cast<clang::ObjCPropertyDecl *>(property)))
              continue;

          // If there is a special declaration associated with this member,
          // add it now.
          if (auto special = importSpecialMethod(member)) {
            members.push_back(special);
          }
        }

        members.push_back(member);
      }

      // FIXME: Source range isn't accurate.
      result->setMembers(Impl.SwiftContext.AllocateCopy(members),
                         Impl.importSourceRange(clang::SourceRange(
                                                  decl->getLocation(),
                                                  decl->getLocEnd())));

      return result;
    }

    Decl *VisitObjCImplDecl(clang::ObjCImplDecl *decl) {
      // Implementations of Objective-C classes and categories are not
      // reflected into Swift.
      return nullptr;
    }

    Decl *VisitObjCPropertyDecl(clang::ObjCPropertyDecl *decl) {
      // Properties are imported as variables.
      auto dc = Impl.importDeclContext(decl->getDeclContext());
      if (!dc)
        return nullptr;

      auto name = Impl.importName(decl->getDeclName());
      if (name.empty())
        return nullptr;

      // Check whether there is a function with the same name as this
      // property. If so, suppress the property; the user will have to use
      // the methods directly, to avoid ambiguities.
      auto containerTy = dc->getDeclaredTypeInContext();
      VarDecl *overridden = nullptr;
      MemberLookup lookup(containerTy, name, *Impl.firstClangModule);
      for (const auto &result : lookup.Results) {
        if (isa<FuncDecl>(result.D))
          return nullptr;

        if (auto var = dyn_cast<VarDecl>(result.D))
          overridden = var;
      }

      auto type = Impl.importType(decl->getType());
      if (!type)
        return nullptr;

      // Import the getter.
      auto getter
        = cast_or_null<FuncDecl>(Impl.importDecl(decl->getGetterMethodDecl()));
      if (!getter && decl->getGetterMethodDecl())
        return nullptr;

      // Import the setter, if there is one.
      auto setter
        = cast_or_null<FuncDecl>(Impl.importDecl(decl->getSetterMethodDecl()));
      if (!setter && decl->getSetterMethodDecl())
        return nullptr;
      
      auto result = new (Impl.SwiftContext)
                      VarDecl(Impl.importSourceLoc(decl->getLocation()),
                              name, type, dc);

      // Build thunks.
      FuncDecl *getterThunk = buildGetterThunk(getter, nullptr);
      getterThunk->makeGetter(result);

      FuncDecl *setterThunk = nullptr;
      if (setter) {
        setterThunk = buildSetterThunk(setter, nullptr);
        setterThunk->makeSetter(result);
      }

      // Turn this into a property.
      // FIXME: Fake locations for '{' and '}'?
      result->setProperty(Impl.SwiftContext, SourceLoc(),
                          getterThunk, setterThunk,
                          SourceLoc());

      // Handle attributes.
      if (decl->hasAttr<clang::IBOutletAttr>())
        result->getMutableAttrs().IBOutlet = true;
      // FIXME: Handle IBOutletCollection.

      if (overridden) {
        result->setOverriddenDecl(overridden);
      }

      return result;
    }

    Decl *
    VisitObjCCompatibleAliasDecl(clang::ObjCCompatibleAliasDecl *decl) {
      // Like C++ using declarations, name lookup simply looks through
      // Objective-C compatibility aliases. They are not imported directly.
      return nullptr;
    }

    Decl *VisitLinkageSpecDecl(clang::LinkageSpecDecl *decl) {
      // Linkage specifications are not imported.
      return nullptr;
    }

    Decl *VisitObjCPropertyImplDecl(clang::ObjCPropertyImplDecl *decl) {
      // @synthesize and @dynamic are not imported, since they are not part
      // of the interface to a class.
      return nullptr;
    }

    Decl *VisitFileScopeAsmDecl(clang::FileScopeAsmDecl *decl) {
      return nullptr;
    }

    Decl *VisitAccessSpecDecl(clang::AccessSpecDecl *decl) {
      return nullptr;
    }

    Decl *VisitFriendDecl(clang::FriendDecl *decl) {
      // Friends are not imported; Swift has a different access control
      // mechanism.
      return nullptr;
    }

    Decl *VisitFriendTemplateDecl(clang::FriendTemplateDecl *decl) {
      // Friends are not imported; Swift has a different access control
      // mechanism.
      return nullptr;
    }

    Decl *VisitStaticAssertDecl(clang::StaticAssertDecl *decl) {
      // Static assertions are an implementation detail.
      return nullptr;
    }

    Decl *VisitBlockDecl(clang::BlockDecl *decl) {
      // Blocks are not imported (although block types can be imported).
      return nullptr;
    }

    Decl *VisitClassScopeFunctionSpecializationDecl(
                 clang::ClassScopeFunctionSpecializationDecl *decl) {
      // Note: templates are not imported.
      return nullptr;
    }

    Decl *VisitImportDecl(clang::ImportDecl *decl) {
      // Transitive module imports are not handled at the declaration level.
      // Rather, they are understood from the module itself.
      return nullptr;
    }
  };
}

Decl *ClangImporter::Implementation::importDecl(clang::NamedDecl *decl) {
  if (!decl)
    return nullptr;
  
  auto known = ImportedDecls.find(decl->getCanonicalDecl());
  if (known != ImportedDecls.end())
    return known->second;

  SwiftDeclConverter converter(*this);
  auto result = converter.Visit(decl);
  auto canon = decl->getCanonicalDecl();
  if (result) {
    assert(!result->getClangDecl() || result->getClangDecl() == canon);
    result->setClangDecl(canon);
  }
  return ImportedDecls[canon] = result;
}

DeclContext *
ClangImporter::Implementation::importDeclContext(clang::DeclContext *dc) {
  // FIXME: Should map to the module we want to import into (?).
  if (dc->isTranslationUnit())
    return firstClangModule;
  
  auto decl = dyn_cast<clang::NamedDecl>(dc);
  if (!decl)
    return nullptr;

  auto swiftDecl = importDecl(decl);
  if (!swiftDecl)
    return nullptr;

  if (auto nominal = dyn_cast<NominalTypeDecl>(swiftDecl))
    return nominal;
  if (auto extension = dyn_cast<ExtensionDecl>(swiftDecl))
    return extension;
  if (auto constructor = dyn_cast<ConstructorDecl>(swiftDecl))
    return constructor;
  if (auto destructor = dyn_cast<DestructorDecl>(swiftDecl))
    return destructor;
  return nullptr;
}
