//===--- IAMInference.cpp - Import as member inference system -------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements support for inferring when globals can be imported as
// members
//
//===----------------------------------------------------------------------===//
#include "IAMInference.h"

#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/Lookup.h"

#include "swift/AST/ASTContext.h"
#include "swift/Basic/StringExtras.h"

#include <array>
#include <tuple>

using namespace swift;

using NameBuffer = SmallString<32>;

static const std::array<StringRef, 2> InitSpecifiers{{
    StringRef("Create"), StringRef("Make"),
}};
static const std::array<StringRef, 2> PropertySpecifiers{{
    StringRef("Get"), StringRef("Set"),
}};

namespace {
enum class CFNameKind : unsigned {
  None,
  CFType,
  NonCFType,
};
}
static CFNameKind getCFNameKind(StringRef name) {
#define CF_TYPE(TYPE)                                                          \
  if (name == #TYPE)                                                           \
    return CFNameKind::CFType;
#define NON_CF_TYPE(TYPE)                                                      \
  if (name == #TYPE)                                                           \
    return CFNameKind::NonCFType;
#include "CFDatabase.def"
  return CFNameKind::None;
}

static bool isCFTypeName(StringRef name) {
  return getCFNameKind(name) == CFNameKind::CFType;
}

// static bool isExplicitNonCFTypeName(StringRef name) {
//   return getCFNameKind(name) == CFNameKind::NonCFType;
// }

IAMOptions IAMOptions::getDefault() { return {}; }

namespace {
class IAMInference {
  ASTContext &context;
  clang::Sema &clangSema;
  IAMOptions options;

public:
  IAMInference(ASTContext &ctx, clang::Sema &sema, IAMOptions opts)
      : context(ctx), clangSema(sema), options(opts) {
    (void)options;
  }

  IAMResult infer(const clang::NamedDecl *);

private:
  // typeID
  IAMResult importAsTypeID(const clang::QualType typeIDTy,
                           EffectiveClangContext effectiveDC) {
    return {formDeclName("typeID"), IAMAccessorKind::Getter, effectiveDC};
  }

  // Init
  IAMResult importAsConstructor(StringRef name, StringRef initSpecifier,
                                ArrayRef<const clang::ParmVarDecl *> params,
                                EffectiveClangContext effectiveDC) {
    if (name != "init") {
      // TODO: move words onto argument labels, as dictated by options
    }
    return {formDeclName("init", params, ""), effectiveDC};
  }

  // Instance computed property
  IAMResult
  importAsInstanceProperty(StringRef name, StringRef propSpec, unsigned selfIdx,
                           ArrayRef<const clang::ParmVarDecl *> nonSelfParams,
                           const clang::FunctionDecl *pairedAccessor,
                           EffectiveClangContext effectiveDC) {
    IAMAccessorKind kind =
        propSpec == "Get" ? IAMAccessorKind::Getter : IAMAccessorKind::Setter;
    assert(kind == IAMAccessorKind::Getter || pairedAccessor && "no set-only");

    return {formDeclName(name), kind, selfIdx, effectiveDC};
  }

  // Instance method
  IAMResult
  importAsInstanceMethod(StringRef name, unsigned selfIdx,
                         ArrayRef<const clang::ParmVarDecl *> nonSelfParams,
                         EffectiveClangContext effectiveDC) {
    return {formDeclName(name, nonSelfParams), selfIdx, effectiveDC};
  }

  // Static stored property
  IAMResult importAsStaticProperty(StringRef name,
                                   EffectiveClangContext effectiveDC) {
    return {formDeclName(name), effectiveDC};
  }

  // Static computed property
  IAMResult
  importAsStaticProperty(StringRef name, StringRef propSpec,
                         ArrayRef<const clang::ParmVarDecl *> nonSelfParams,
                         const clang::FunctionDecl *pairedAccessor,
                         EffectiveClangContext effectiveDC) {
    IAMAccessorKind kind =
        propSpec == "Get" ? IAMAccessorKind::Getter : IAMAccessorKind::Setter;
    assert(kind == IAMAccessorKind::Getter || pairedAccessor && "no set-only");

    return {formDeclName(name), kind, effectiveDC};
  }

  // Static method
  IAMResult
  importAsStaticMethod(StringRef name,
                       ArrayRef<const clang::ParmVarDecl *> nonSelfParams,
                       EffectiveClangContext effectiveDC) {
    return {formDeclName(name, nonSelfParams), effectiveDC};
  }

  template <typename DeclType>
  inline DeclType *clangLookup(StringRef name,
                               clang::Sema::LookupNameKind kind);

  clang::TypeDecl *clangLookupTypeDecl(StringRef name) {
    if (auto ty = clangLookup<clang::TypedefNameDecl>(
            name, clang::Sema::LookupNameKind::LookupOrdinaryName))
      return ty;

    return clangLookup<clang::TagDecl>(
        name, clang::Sema::LookupNameKind::LookupTagName);
  }

  clang::FunctionDecl *clangLookupFunction(StringRef name) {
    return clangLookup<clang::FunctionDecl>(
        name, clang::Sema::LookupNameKind::LookupOrdinaryName);
  }

  const clang::TypeDecl *findTypeAndMatch(StringRef workingName,
                                          NameBuffer &outStr) {
    // FIXME: drop mutable...

    // Longest-prefix matching, alternate with checking for a trailing "Ref"
    // suffix and the prefix itself. We iterate from the back to the beginning.
    auto words = camel_case::getWords(workingName);
    for (auto rWordsIter = words.rbegin(), rWordsEnd = words.rend();
         rWordsIter != rWordsEnd; ++rWordsIter) {
      NameBuffer nameAttempt;
      nameAttempt.append(rWordsIter.base().getPriorStr());
      StringRef prefix = nameAttempt;
      nameAttempt.append("Ref");
      StringRef prefixWithRef = nameAttempt;

      if (auto tyDecl = clangLookupTypeDecl(prefixWithRef)) {
        outStr.append(workingName.drop_front(prefix.size()));
        return tyDecl;
      }
      if (auto tyDecl = clangLookupTypeDecl(prefix)) {
        outStr.append(workingName.drop_front(prefix.size()));
        return tyDecl;
      }
    }

    return nullptr;
  }

  const clang::FunctionDecl *findPairedAccessor(StringRef name,
                                                StringRef propSpec) {
    NameBuffer pairName;
    auto words = camel_case::getWords(name);
    for (auto word : words) {
      if (word == propSpec) {
        if (propSpec == "Get") {
          pairName.append("Set");
        } else {
          assert(propSpec == "Set");
          pairName.append("Get");
        }
      } else {
        pairName.append(word);
      }
    }

    return clangLookupFunction(pairName);
  }

  Identifier getHumbleIdentifier(StringRef name) {
    // Lower-camel-case the incoming name
    NameBuffer buf;
    return {context.getIdentifier(camel_case::toLowercaseWord(name, buf))};
  }

  DeclName formDeclName(StringRef baseName) {
    return {getHumbleIdentifier(baseName)};
  }

  DeclName formDeclName(StringRef baseName,
                        ArrayRef<const clang::ParmVarDecl *> params,
                        StringRef firstPrefix = "") {
    SmallVector<Identifier, 8> argLabels;
    for (unsigned i = 0; i < params.size(); ++i) {
      if (firstPrefix != "") {
        // TODO: move words onto argument labels
      }
      argLabels.push_back(context.getIdentifier(params[i]->getName()));
    }

    return {context, getHumbleIdentifier(baseName), argLabels};
  }

  bool match(StringRef str, StringRef toMatch, NameBuffer &outStr) {
    // TODO: let options dictate fuzzy matching...

    // FIXME: drop mutable...

    auto strWords = camel_case::getWords(str);
    auto matchWords = camel_case::getWords(toMatch);

    auto strIter = strWords.begin();
    auto matchIter = matchWords.begin();

    // Match in order, but allowing interjected words
    while (strIter != strWords.end()) {
      if (matchIter == matchWords.end()) {
        // We matched them all!
        outStr.append(strIter.getRestOfStr());
        return true;
      }
      if (*strIter == *matchIter) {
        // It's a match!
        ++strIter;
        ++matchIter;
        continue;
      }
      // Move on to the next one
      outStr.append(*strIter);
      ++strIter;
    }

    return false;
  }
};
}

static bool isStructType(clang::QualType qt) {
  if (qt->isPointerType())
    qt = qt->getPointeeType();
  return qt->getUnqualifiedDesugaredType()->isStructureType();
}

static StringRef getTypeName(clang::QualType qt) {
  if (auto typedefTy = qt->getAs<clang::TypedefType>()) {
    // Use the sugar-ed, but un-ref-ed name
    auto name = typedefTy->getDecl()->getName();
    if (name.endswith("Ref") &&
        (isCFTypeName(name) ||
         typedefTy->getDecl()->hasAttr<clang::ObjCBridgeAttr>()))
      return name.drop_back(3);
  }

  auto identInfo = qt.getBaseTypeIdentifier();
  if (identInfo)
    return identInfo->getName();

  // Otherwise, no name
  return {};
}

// Get the canonical, but still sugar-ed, effective decl context
static EffectiveClangContext getEffectiveDC(clang::QualType qt) {
  auto ty = qt.getTypePtr();
  if (auto typedefType = ty->getAs<clang::TypedefType>())
    return {typedefType->getDecl()->getCanonicalDecl()};

  auto pointeeQT = ty->getPointeeType();
  if (pointeeQT != clang::QualType())
    ty = pointeeQT.getTypePtr();

  if (auto typedefType = ty->getAs<clang::TypedefType>()) {
    return {typedefType->getDecl()->getCanonicalDecl()};
  }

  if (const clang::RecordType *recTy = ty->getAsStructureType())
    return {recTy->getDecl()->getCanonicalDecl()};

  assert(0 && "not a valid Clang context");
  return {};
}

static EffectiveClangContext getEffectiveDC(const clang::TypeDecl *tyDecl) {
  // FIXME: ewwe
  auto typeDecl = const_cast<clang::TypeDecl *>(tyDecl);
  if (auto typedefName = dyn_cast<clang::TypedefNameDecl>(typeDecl))
    return {typedefName->getCanonicalDecl()};
  if (auto tagDecl = dyn_cast<clang::TagDecl>(typeDecl))
    return {tagDecl->getCanonicalDecl()};

  assert("expected valid Clang context");
  return {};
}

static bool hasWord(StringRef s, StringRef matchWord) {
  for (auto word : camel_case::getWords(s))
    if (word == matchWord)
      return true;
  return false;
}

IAMResult IAMInference::infer(const clang::NamedDecl *clangDecl) {
  if (auto varDecl = dyn_cast<clang::VarDecl>(clangDecl)) {
    // Try to find a type to add this as a static property to
    StringRef workingName = varDecl->getName();
    NameBuffer remainingName;
    const clang::TypeDecl *tyDecl =
        findTypeAndMatch(workingName, remainingName);
    if (tyDecl)
      return importAsStaticProperty(remainingName, getEffectiveDC(tyDecl));
    return {};
  }

  // Try to infer a member function
  auto funcDecl = dyn_cast<clang::FunctionDecl>(clangDecl);
  if (!funcDecl)
    return {};

  // Can't really import variadics well
  if (funcDecl->isVariadic())
    return {};

  // FIXME: drop "Mutable"...

  StringRef workingName = funcDecl->getName();
  auto retTy = funcDecl->getReturnType();
  unsigned numParams = funcDecl->getNumParams();

  // 0) Special cases are specially handled: *GetTypeID()
  //
  StringRef getTypeID = "GetTypeID";
  if (numParams == 0 && workingName.endswith(getTypeID)) {
    NameBuffer remainingName;
    if (auto tyDecl = findTypeAndMatch(workingName.drop_back(getTypeID.size()),
                                       remainingName)) {
      // We shouldn't have anything else left in our name for typeID
      if (remainingName.empty())
        return importAsTypeID(retTy, getEffectiveDC(tyDecl));
    }
  }

  // 1) If we find an init specifier and our name matches the return type, we
  //    import as some kind of constructor
  //
  if (!retTy->isVoidType()) {
    NameBuffer remainingName;
    StringRef typeName = getTypeName(retTy);
    if (typeName != StringRef())
      if (match(workingName, typeName, remainingName))
        for (auto initSpec : InitSpecifiers)
          if (hasWord(remainingName, initSpec))
            return importAsConstructor(
                remainingName, initSpec,
                {funcDecl->param_begin(), funcDecl->param_end()},
                getEffectiveDC(retTy));
  }

  // 2) If we find a likely self reference in the parameters, make an instance
  //    member (method or property)
  //
  SmallVector<const clang::ParmVarDecl *, 8> nonSelfParams;
  unsigned selfIdx = 0;
  for (auto paramI = funcDecl->param_begin(), paramE = funcDecl->param_end();
       paramI != paramE; ++paramI, ++selfIdx) {
    auto param = *paramI;
    NameBuffer remainingName;
    StringRef typeName = getTypeName(param->getType());
    if (typeName != StringRef()) {
      if (match(workingName, typeName, remainingName)) {
        auto effectiveDC = getEffectiveDC(param->getType());
        nonSelfParams.append(funcDecl->param_begin(), paramI);
        nonSelfParams.append(++paramI, paramE);
        // See if it's a property
        for (auto propSpec : PropertySpecifiers) {
          NameBuffer propName;
          if (match(remainingName, propSpec, propName)) {
            if (auto pair = findPairedAccessor(workingName, propSpec)) {
              return importAsInstanceProperty(propName, propSpec, selfIdx,
                                              nonSelfParams, pair, effectiveDC);
            } else if (propSpec == "Get") {
              return importAsInstanceProperty(propName, propSpec, selfIdx,
                                              nonSelfParams, nullptr,
                                              effectiveDC);
            }
          }
        }

        return importAsInstanceMethod(remainingName, selfIdx, nonSelfParams,
                                      effectiveDC);
      }
    }
  }

  // No self, must be static
  nonSelfParams = {funcDecl->param_begin(), funcDecl->param_end()};

  // 3) Finally, try to find a class to put this on as a static function
  NameBuffer remainingName;
  if (const clang::TypeDecl *tyDecl =
          findTypeAndMatch(workingName, remainingName)) {
    auto effectiveDC = getEffectiveDC(tyDecl);

    ArrayRef<const clang::ParmVarDecl *> params = {funcDecl->param_begin(),
                                                   funcDecl->param_end()};
    // See if it's a property
    for (auto propSpec : PropertySpecifiers) {
      NameBuffer propName;
      if (match(remainingName, propSpec, propName)) {
        if (auto pair = findPairedAccessor(workingName, propSpec)) {
          return importAsStaticProperty(propName, propSpec, nonSelfParams, pair,
                                        effectiveDC);
        } else if (propSpec == "Get") {

          return importAsStaticProperty(propName, propSpec, nonSelfParams,
                                        nullptr, effectiveDC);
        }
      }
    }

    return importAsStaticMethod(remainingName, nonSelfParams, effectiveDC);
  }

  return {};
}

template <typename DeclType>
DeclType *IAMInference::clangLookup(StringRef name,
                                    clang::Sema::LookupNameKind kind) {
  clang::IdentifierInfo *nameII = &clangSema.getASTContext().Idents.get(name);
  clang::LookupResult lookupResult(clangSema, clang::DeclarationName(nameII),
                                   clang::SourceLocation(), kind);
  if (!clangSema.LookupName(lookupResult, clangSema.TUScope))
    return nullptr;
  auto res = lookupResult.getAsSingle<DeclType>();
  if (!res)
    return nullptr;
  return res->getCanonicalDecl();
}

IAMResult IAMResult::infer(ASTContext &ctx, clang::Sema &clangSema,
                           const clang::NamedDecl *decl, IAMOptions opts) {
  IAMInference inference(ctx, clangSema, opts);
  return inference.infer(decl);
}
