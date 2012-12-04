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
#include "swift/AST/Types.h"
#include "clang/AST/ASTContext.h"
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

      auto type = Impl.importType(decl->getUnderlyingType());
      if (!type)
        return nullptr;

      return new (Impl.SwiftContext) TypeAliasDecl(
                                      Impl.importSourceLoc(decl->getLocStart()),
                                      name,
                                      Impl.importSourceLoc(decl->getLocation()),
                                      TypeLoc::withoutLoc(type),
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
    static void setVarDeclContexts(SmallVectorImpl<Pattern *> &patterns,
                                   DeclContext *dc) {
      for (auto pattern : patterns) {
        auto pat = pattern->getSemanticsProvidingPattern();
        if (auto named = dyn_cast<NamedPattern>(pat))
          named->getDecl()->setDeclContext(dc);
      }
    }

    Decl *VisitFunctionDecl(clang::FunctionDecl *decl) {
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

      auto name = Impl.importName(decl->getDeclName());
      if (name.empty())
        return nullptr;

      // FIXME: Poor location info.
      auto loc = Impl.importSourceLoc(decl->getLocStart());
      auto funcExpr = FuncExpr::create(Impl.SwiftContext, loc,
                                       argPatterns, bodyPatterns,
                                       TypeLoc::withoutLoc(resultTy),
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

      return new (Impl.SwiftContext)
               VarDecl(Impl.importSourceLoc(decl->getLocation()),
                       name, type, dc);
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
      thisPat
        = new (Impl.SwiftContext) TypedPattern(thisPat,
                                               TypeLoc::withoutLoc(thisTy));
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
      auto loc = Impl.importSourceLoc(decl->getLocStart());
      auto nameLoc = Impl.importSourceLoc(decl->getLocation());
      auto funcExpr = FuncExpr::create(Impl.SwiftContext, loc,
                                       argPatterns, bodyPatterns,
                                       TypeLoc::withoutLoc(resultTy),
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

      return result;
    }

  private:
    /// \brief Given an imported method, try to import it as a constructor.
    ///
    /// Objective-C methods in the 'init' and 'new' family are imported as
    /// constructors in Swift, enabling the 'new' syntax, e.g.,
    ///
    /// \code
    /// new NSArray(1024) // same as NSArray.alloc.initWithCapacity:1024
    /// \endcode
    ConstructorDecl *importAsConstructor(Decl *decl) {
      // Only consider Objective-C methods...
      auto objcMethod
        = dyn_cast_or_null<clang::ObjCMethodDecl>(decl->getClangDecl());
      if (!objcMethod)
        return nullptr;

      // ...in the 'init' or 'new' family...
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
        return nullptr;

      case clang::OMF_init:
      case clang::OMF_new:
        break;
      }

      // FIXME: Hack.
      auto name = Impl.SwiftContext.getIdentifier("constructor");
      auto dc = decl->getDeclContext();

      // Figure out the type of the container.
      auto containerTy = dc->getDeclaredTypeOfContext();
      assert(containerTy && "Method in non-type context?");

      // Add the implicit 'this' parameter patterns.
      SmallVector<Pattern *, 4> argPatterns;
      SmallVector<Pattern *, 4> bodyPatterns;
      auto thisTy = containerTy;
      auto thisMetaTy = MetaTypeType::get(containerTy, Impl.SwiftContext);
      auto thisName = Impl.SwiftContext.getIdentifier("this");
      auto thisVar = new (Impl.SwiftContext) VarDecl(SourceLoc(), thisName,
                                                     thisMetaTy,
                                                     Impl.firstClangModule);
      Pattern *thisPat = new (Impl.SwiftContext) NamedPattern(thisVar);
      thisPat
        = new (Impl.SwiftContext) TypedPattern(thisPat,
                                               TypeLoc::withoutLoc(thisMetaTy));
      
      argPatterns.push_back(thisPat);
      bodyPatterns.push_back(thisPat);

      // Import the type that this method will have.
      auto type = Impl.importFunctionType(objcMethod->getResultType(),
                                          { objcMethod->param_begin(),
                                            objcMethod->param_size() },
                                          objcMethod->isVariadic(),
                                          argPatterns,
                                          bodyPatterns,
                                          objcMethod->getSelector());
      assert(type && "Type has already been successfully converted?");

      // A constructor returns an object of the type, not 'id'.
      // This is effectively implementing related-result-type semantics.
      // FIXME: Perhaps actually check whether the routine has a related result
      // type?
      type = FunctionType::get(type->castTo<FunctionType>()->getInput(),
                               thisTy, Impl.SwiftContext);

      // Add the 'this' parameter to the function type.
      type = FunctionType::get(thisMetaTy, type, Impl.SwiftContext);

      // FIXME: Poor location info.
      auto loc = Impl.importSourceLoc(objcMethod->getLocStart());

      // FIXME: Losing body patterns here.
      VarDecl *thisDecl
        = new (Impl.SwiftContext) VarDecl(
                                    SourceLoc(),
                                    Impl.SwiftContext.getIdentifier("this"),
                                    thisTy, dc);

      auto result = new (Impl.SwiftContext) ConstructorDecl(name, loc,
                                                            argPatterns.front(),
                                                            thisDecl,
                                                            /*GenericParams=*/0,
                                                            dc);
      result->setType(type);
      thisDecl->setDeclContext(result);
      setVarDeclContexts(argPatterns, result);
      setVarDeclContexts(bodyPatterns, result);
      return result;
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
      auto result
        = new (Impl.SwiftContext)
            ExtensionDecl(Impl.importSourceLoc(decl->getLocStart()),
                          TypeLoc::withoutLoc(objcClass->getDeclaredType()),
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

          // If there is a constructor associated with this member, add it now.
          if (auto ctor = importAsConstructor(member)) {
            members.push_back(ctor);
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

          // If there is a constructor associated with this member, add it now.
          if (auto ctor = importAsConstructor(member)) {
            members.push_back(ctor);
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

      // Turn this into a property.
      // FIXME: Fake locations for '{' and '}'?
      result->setProperty(Impl.SwiftContext, SourceLoc(), getter, setter,
                          SourceLoc());

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
