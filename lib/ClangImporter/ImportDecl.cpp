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
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Types.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclVisitor.h"
#include "llvm/ADT/StringExtras.h"

using namespace swift;

namespace {
  /// \brief Convert Clang declarations into the corresponding Swift
  /// declarations.
  class SwiftDeclConverter
    : public clang::DeclVisitor<SwiftDeclConverter, ValueDecl *>
  {
    ClangImporter::Implementation &Impl;

  public:
    explicit SwiftDeclConverter(ClangImporter::Implementation &impl)
      : Impl(impl) { }

    ValueDecl *VisitDecl(clang::Decl *decl) {
      return nullptr;
    }

    ValueDecl *VisitTranslationUnitDecl(clang::TranslationUnitDecl *decl) {
      // Note: translation units are handled specially by importDeclContext.
      return nullptr;
    }

    ValueDecl *VisitNamespaceDecl(clang::NamespaceDecl *decl) {
      // FIXME: Implement once Swift has namespaces.
      return nullptr;
    }

    ValueDecl *VisitUsingDirectiveDecl(clang::UsingDirectiveDecl *decl) {
      // Never imported.
      return nullptr;
    }

    ValueDecl *VisitNamespaceAliasDecl(clang::NamespaceAliasDecl *decl) {
      // FIXME: Implement once Swift has namespaces.
      return nullptr;
    }

    ValueDecl *VisitLabelDecl(clang::LabelDecl *decl) {
      // Labels are function-local, and therefore never imported.
      return nullptr;
    }

    ValueDecl *VisitTypedefNameDecl(clang::TypedefNameDecl *decl){
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

    ValueDecl *
    VisitUnresolvedUsingTypenameDecl(clang::UnresolvedUsingTypenameDecl *decl) {
      // Note: only occurs in templates.
      return nullptr;
    }

    ValueDecl *VisitEnumDecl(clang::EnumDecl *decl) {
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

    ValueDecl *VisitRecordDecl(clang::RecordDecl *decl) {
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

    ValueDecl *VisitClassTemplateSpecializationDecl(
                 clang::ClassTemplateSpecializationDecl *decl) {
      // FIXME: We could import specializations, but perhaps only as unnamed
      // structural types.
      return nullptr;
    }

    ValueDecl *VisitClassTemplatePartialSpecializationDecl(
                 clang::ClassTemplatePartialSpecializationDecl *decl) {
      // Note: templates are not imported.
      return nullptr;
    }

    ValueDecl *VisitTemplateTypeParmDecl(clang::TemplateTypeParmDecl *decl) {
      // Note: templates are not imported.
      return nullptr;
    }

    ValueDecl *VisitEnumConstantDecl(clang::EnumConstantDecl *decl) {
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

    ValueDecl *
    VisitUnresolvedUsingValueDecl(clang::UnresolvedUsingValueDecl *decl) {
      // Note: templates are not imported.
      return nullptr;
    }

    ValueDecl *VisitIndirectFieldDecl(clang::IndirectFieldDecl *decl) {
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

    ValueDecl *VisitFunctionDecl(clang::FunctionDecl *decl) {
      auto dc = Impl.importDeclContext(decl->getDeclContext());
      if (!dc)
        return nullptr;

      // Import the function type. If we have parameters, make sure their names
      // get into the resulting function type.
      Type type;
      if (decl->param_size())
        type = Impl.importFunctionType(
                 decl->getType()->getAs<clang::FunctionType>()->getResultType(),
                 { decl->param_begin(), decl->param_size() },
                 decl->isVariadic());
      else
        type = Impl.importType(decl->getType());

      if (!type)
        return nullptr;

      auto name = Impl.importName(decl->getDeclName());
      if (name.empty())
        return nullptr;

      return new (Impl.SwiftContext)
               FuncDecl(SourceLoc(),
                        Impl.importSourceLoc(decl->getLocStart()),
                        name,
                        Impl.importSourceLoc(decl->getLocation()),
                        /*GenericParams=*/0,
                        type,
                        /*Body=*/nullptr,
                        dc);
    }

    ValueDecl *VisitCXXMethodDecl(clang::CXXMethodDecl *decl) {
      // FIXME: Import C++ member functions as methods.
      return nullptr;
    }

    ValueDecl *VisitFieldDecl(clang::FieldDecl *decl) {
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

    ValueDecl *VisitObjCIvarDecl(clang::ObjCIvarDecl *decl) {
      // FIXME: Deal with fact that a property and an ivar can have the same
      // name.
      return VisitFieldDecl(decl);
    }

    ValueDecl *VisitObjCAtDefsFieldDecl(clang::ObjCAtDefsFieldDecl *decl) {
      // @defs is an anachronism; ignore it.
      return nullptr;
    }

    ValueDecl *VisitVarDecl(clang::VarDecl *decl) {
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

    ValueDecl *VisitImplicitParamDecl(clang::ImplicitParamDecl *decl) {
      // Parameters are never directly imported.
      return nullptr;
    }

    ValueDecl *VisitParmVarDecl(clang::ParmVarDecl *decl) {
      // Parameters are never directly imported.
      return nullptr;
    }

    ValueDecl *
    VisitNonTypeTemplateParmDecl(clang::NonTypeTemplateParmDecl *decl) {
      // Note: templates are not imported.
      return nullptr;
    }

    ValueDecl *VisitTemplateDecl(clang::TemplateDecl *decl) {
      // Note: templates are not imported.
      return nullptr;
    }

    ValueDecl *VisitUsingDecl(clang::UsingDecl *decl) {
      // Using declarations are not imported.
      return nullptr;
    }

    ValueDecl *VisitUsingShadowDecl(clang::UsingShadowDecl *decl) {
      // Using shadow declarations are not imported; rather, name lookup just
      // looks through them.
      return nullptr;
    }

    ValueDecl *VisitObjCMethodDecl(clang::ObjCMethodDecl *decl) {
      auto dc = Impl.importDeclContext(decl->getDeclContext());
      if (!dc)
        return nullptr;

      // The name of the method is the first part of the selector.
      auto name
        = Impl.importName(decl->getSelector().getIdentifierInfoForSlot(0));
      if (name.empty())
        return nullptr;

      // Import the type that this method will have.
      auto type = Impl.importFunctionType(decl->getResultType(),
                                          { decl->param_begin(),
                                            decl->param_size() },
                                          decl->isVariadic(),
                                          decl->getSelector());
      if (!type)
        return nullptr;

      // Figure out the type of the container.
      auto containerTy= dc->getDeclaredTypeOfContext();
      assert(containerTy && "Method in non-type context?");

      // Add the implicit 'this' parameter.
      auto thisTy = containerTy;
      if (decl->isClassMethod())
        thisTy = MetaTypeType::get(thisTy, Impl.SwiftContext);
      TupleTypeElt thisParam(thisTy, Impl.SwiftContext.getIdentifier("this"));
      auto thisTupleTy = TupleType::get({ &thisParam, 1 }, Impl.SwiftContext);
      type = FunctionType::get(thisTupleTy, type, Impl.SwiftContext);

      // FIXME: Related result type? Not so important when we get constructors
      // working.

      // FIXME: Add proper parameter patterns so this looks more like a method
      // declaration when Swift prints it back out.
      auto result = new (Impl.SwiftContext)
                      FuncDecl(SourceLoc(),
                               Impl.importSourceLoc(decl->getLocStart()),
                               name,
                               Impl.importSourceLoc(decl->getLocation()),
                               /*GenericParams=*/0,
                               type,
                               /*Body=*/nullptr,
                               dc);

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

    // FIXME: ObjCCategoryDecl
    // FIXME: ObjCProtocolDecl

    ValueDecl *VisitObjCInterfaceDecl(clang::ObjCInterfaceDecl *decl) {
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
        if (auto objcMethod = dyn_cast<clang::ObjCMethodDecl>(nd))
          if (auto property = objcMethod->findPropertyDecl())
            if (Impl.importDecl(
                  const_cast<clang::ObjCPropertyDecl *>(property)))
              continue;

        members.push_back(member);
      }

      // FIXME: Source range isn't accurate.
      result->setMembers(Impl.SwiftContext.AllocateCopy(members),
                         Impl.importSourceRange(clang::SourceRange(
                                                  decl->getLocation(),
                                                  decl->getLocEnd())));

      return result;
    }

    ValueDecl *VisitObjCImplDecl(clang::ObjCImplDecl *decl) {
      // Implementations of Objective-C classes and categories are not
      // reflected into Swift.
      return nullptr;
    }

    ValueDecl *VisitObjCPropertyDecl(clang::ObjCPropertyDecl *decl) {
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

    ValueDecl *
    VisitObjCCompatibleAliasDecl(clang::ObjCCompatibleAliasDecl *decl) {
      // Like C++ using declarations, name lookup simply looks through
      // Objective-C compatibility aliases. They are not imported directly.
      return nullptr;
    }

    ValueDecl *VisitLinkageSpecDecl(clang::LinkageSpecDecl *decl) {
      // Linkage specifications are not imported.
      return nullptr;
    }

    ValueDecl *VisitObjCPropertyImplDecl(clang::ObjCPropertyImplDecl *decl) {
      // @synthesize and @dynamic are not imported, since they are not part
      // of the interface to a class.
      return nullptr;
    }

    ValueDecl *VisitFileScopeAsmDecl(clang::FileScopeAsmDecl *decl) {
      return nullptr;
    }

    ValueDecl *VisitAccessSpecDecl(clang::AccessSpecDecl *decl) {
      return nullptr;
    }

    ValueDecl *VisitFriendDecl(clang::FriendDecl *decl) {
      // Friends are not imported; Swift has a different access control
      // mechanism.
      return nullptr;
    }

    ValueDecl *VisitFriendTemplateDecl(clang::FriendTemplateDecl *decl) {
      // Friends are not imported; Swift has a different access control
      // mechanism.
      return nullptr;
    }

    ValueDecl *VisitStaticAssertDecl(clang::StaticAssertDecl *decl) {
      // Static assertions are an implementation detail.
      return nullptr;
    }

    ValueDecl *VisitBlockDecl(clang::BlockDecl *decl) {
      // Blocks are not imported (although block types can be imported).
      return nullptr;
    }

    ValueDecl *VisitClassScopeFunctionSpecializationDecl(
                 clang::ClassScopeFunctionSpecializationDecl *decl) {
      // Note: templates are not imported.
      return nullptr;
    }

    ValueDecl *VisitImportDecl(clang::ImportDecl *decl) {
      // Transitive module imports are not handled at the declaration level.
      // Rather, they are understood from the module itself.
      return nullptr;
    }
  };
}

ValueDecl *ClangImporter::Implementation::importDecl(clang::NamedDecl *decl) {
  if (!decl)
    return nullptr;
  
  auto known = ImportedDecls.find(decl->getCanonicalDecl());
  if (known != ImportedDecls.end())
    return known->second;

  SwiftDeclConverter converter(*this);
  auto result = converter.Visit(decl);
  return ImportedDecls[decl->getCanonicalDecl()] = result;
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
