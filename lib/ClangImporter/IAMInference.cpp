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

#include "swift/AST/ASTContext.h"
#include "swift/Basic/StringExtras.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/Lookup.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

#include <array>
#include <tuple>

#define DEBUG_TYPE "Infer import as member"

// Statistics for failure to infer
STATISTIC(FailInferVar, "# of variables unable to infer");
STATISTIC(FailInferFunction, "# of functions unable to infer");

// Success statistics
STATISTIC(SuccessImportAsTypeID, "# imported as 'typeID'");
STATISTIC(SuccessImportAsConstructor, "# imported as 'init'");
STATISTIC(SuccessImportAsInstanceComputedProperty,
          "# imported as instance computed property");
STATISTIC(SuccessImportAsStaticProperty,
          "# imported as static (stored) property");
STATISTIC(SuccessImportAsStaticComputedProperty,
          "# imported as static computed property");
STATISTIC(SuccessImportAsStaticMethod, "# imported as static method");
STATISTIC(SuccessImportAsInstanceMethod, "# imported as instance method");

// Statistics for why we couldn't infer in more specific ways, and fell back to
// methods
STATISTIC(InvalidPropertyStaticNumParams,
          "couldn't infer as static property: invalid number of parameters");
STATISTIC(InvalidPropertyInstanceNumParams,
          "couldn't infer as instance property: invalid number of parameters");
STATISTIC(InvalidPropertyStaticGetterSetterType,
          "couldn't infer as static property: getter/setter type mismatch");
STATISTIC(InvalidPropertyInstanceGetterSetterType,
          "couldn't infer as instance property: getter/setter type mismatch");
STATISTIC(InvalidPropertyInstanceNoSelf,
          "couldn't infer as instance property: couldn't find self");

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

// As append, but skip a repeated word at the boundary. Case sensitive.
// Example: appendUniq("FooBar", "BarBaz") ==> "FooBarBaz"
void appendUniq(NameBuffer &src, StringRef toAppend) {
  if (src.empty()) {
    src = toAppend;
    return;
  }

  auto appendWords = camel_case::getWords(toAppend);
  StringRef lastWord = *camel_case::getWords(src).rbegin();
  auto wI = appendWords.begin();
  while (wI != appendWords.end() && *wI == lastWord)
    ++wI;
  src.append(wI.getRestOfStr());
}

StringRef skipLeadingUnderscores(StringRef str) {
  unsigned numToDrop = 0;
  for (/*empty*/; numToDrop < str.size(); ++numToDrop)
    if (str[numToDrop] != '_')
      break;
  return str.drop_front(numToDrop);
}

// Form a humble camel name from a string. Skips leading underscores.
static void formHumbleCamelName(StringRef str, NameBuffer &out) {
  str = skipLeadingUnderscores(str);
  auto newStr = camel_case::toLowercaseInitialisms(str, out);
  if (newStr == str)
    out = newStr;
}

// Form a humble camel by appending the two strings and adjusting case as
// needed. Skips leading underscores in either name, and skips a repeated word
// at the boundary. Example: formHumbleCamelName("__FooBar", "barBaz") ==>
// "fooBarBaz".
static void formHumbleCamelName(StringRef left, StringRef right,
                                NameBuffer &out) {
  left = skipLeadingUnderscores(left);
  if (left == "") {
    formHumbleCamelName(right, out);
    return;
  }
  right = skipLeadingUnderscores(right);
  if (right == "") {
    formHumbleCamelName(left, out);
    return;
  }

  StringRef lastWord = *camel_case::getWords(left).rbegin();
  auto rightWords = camel_case::getWords(right);
  auto wI = rightWords.begin();
  while (wI != rightWords.end() &&
         camel_case::sameWordIgnoreFirstCase(*wI, lastWord))
    ++wI;

  formHumbleCamelName(left, out);
  camel_case::appendSentenceCase(out, wI.getRestOfStr());
}

static unsigned dropWord(StringRef str, StringRef word, NameBuffer &out) {
  unsigned numDropped = 0;
  auto words = camel_case::getWords(str);
  for (auto wI = words.begin(), wE = words.end(); wI != wE; ++wI)
    if (*wI == word)
      ++numDropped;
    else
      out.append(*wI);

  return numDropped;
}

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
    ++SuccessImportAsTypeID;
    return {formDeclName("typeID"), IAMAccessorKind::Getter, effectiveDC};
  }

  // Init
  IAMResult importAsConstructor(StringRef name, StringRef initSpecifier,
                                ArrayRef<const clang::ParmVarDecl *> params,
                                EffectiveClangContext effectiveDC) {
    ++SuccessImportAsConstructor;
    NameBuffer buf;
    if (name != initSpecifier) {
      assert(name.size() > initSpecifier.size() &&
             "should have more words in it");
      auto didDrop = dropWord(name, initSpecifier, buf);
      (void)didDrop;
      assert(didDrop != 0 && "specifier not present?");
    }
    return {formDeclName("init", params, buf), effectiveDC};
  }

  // Instance computed property
  IAMResult
  importAsInstanceProperty(StringRef name, StringRef propSpec, unsigned selfIdx,
                           ArrayRef<const clang::ParmVarDecl *> nonSelfParams,
                           const clang::FunctionDecl *pairedAccessor,
                           EffectiveClangContext effectiveDC) {
    ++SuccessImportAsInstanceComputedProperty;
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
    ++SuccessImportAsInstanceMethod;
    return {formDeclName(name, nonSelfParams), selfIdx, effectiveDC};
  }

  // Static stored property
  IAMResult importAsStaticProperty(StringRef name,
                                   EffectiveClangContext effectiveDC) {
    ++SuccessImportAsStaticProperty;
    return {formDeclName(name), effectiveDC};
  }

  // Static computed property
  IAMResult
  importAsStaticProperty(StringRef name, StringRef propSpec,
                         ArrayRef<const clang::ParmVarDecl *> nonSelfParams,
                         const clang::FunctionDecl *pairedAccessor,
                         EffectiveClangContext effectiveDC) {
    ++SuccessImportAsStaticComputedProperty;
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
    ++SuccessImportAsStaticMethod;
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

    // TODO: should we try some form of fuzzy or fuzzier matching?

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

  bool validToImportAsProperty(const clang::FunctionDecl *originalDecl,
                               StringRef propSpec, Optional<unsigned> selfIndex,
                               const clang::FunctionDecl *&pairedAccessor);

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
    formHumbleCamelName(name, buf);
    return {context.getIdentifier(buf)};
  }

  DeclName formDeclName(StringRef baseName) {
    return {getHumbleIdentifier(baseName)};
  }

  DeclName formDeclName(StringRef baseName,
                        ArrayRef<const clang::ParmVarDecl *> params,
                        StringRef firstPrefix = "") {
    SmallVector<Identifier, 8> argLabels;

    if (params.empty() && firstPrefix != "") {
      // We need to form an argument label, despite there being no argument
      NameBuffer paramName;
      formHumbleCamelName(firstPrefix, paramName);

      // FIXME: enable this when we have ImportDecl support.
      // argLabels.push_back(context.getIdentifier(paramName));
    }

    for (unsigned i = 0; i < params.size(); ++i) {
      NameBuffer paramName;
      if (i == 0 && firstPrefix != "") {
        formHumbleCamelName(firstPrefix, params[i]->getName(), paramName);
      } else {
        // TODO: strip leading underscores
        formHumbleCamelName(params[i]->getName(), paramName);
      }

      argLabels.push_back(context.getIdentifier(paramName));
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
        appendUniq(outStr, strIter.getRestOfStr());
        return true;
      }
      if (*strIter == *matchIter) {
        // It's a match!
        ++strIter;
        ++matchIter;
        continue;
      }
      // Move on to the next one
      appendUniq(outStr, *strIter);
      ++strIter;
    }

    return false;
  }
};
}

// A loose type equality check that disregards all sugar, qualification, looks
// through pointers, etc.
static bool rouglyEqual(clang::QualType left, clang::QualType right) {
  auto leftPointee = left->getPointeeType();
  if (leftPointee != clang::QualType())
    left = leftPointee;
  auto rightPointee = right->getPointeeType();
  if (rightPointee != clang::QualType())
    right = rightPointee;
  return left->getUnqualifiedDesugaredType() ==
         right->getUnqualifiedDesugaredType();
}

bool IAMInference::validToImportAsProperty(
    const clang::FunctionDecl *originalDecl, StringRef propSpec,
    Optional<unsigned> selfIndex, const clang::FunctionDecl *&pairedAccessor) {
  bool isGet = propSpec == "Get";
  pairedAccessor = findPairedAccessor(originalDecl->getName(), propSpec);
  if (!pairedAccessor)
    return isGet;

  auto getterDecl = isGet ? originalDecl : pairedAccessor;
  auto setterDecl = isGet ? pairedAccessor : originalDecl;
  auto getterTy = getterDecl->getReturnType();

  // See if this is a static property
  if (!selfIndex) {
    // Getter has none, setter has one arg
    if (getterDecl->getNumParams() != 0 || setterDecl->getNumParams() != 1) {
      ++InvalidPropertyStaticNumParams;
      return false;
    }

    // Setter's arg type should be same as getter's return type
    if (!rouglyEqual(getterTy, setterDecl->getParamDecl(0)->getType())) {
      ++InvalidPropertyStaticGetterSetterType;
      return false;
    }

    return true;
  }

  // Instance property, look beyond self
  if (getterDecl->getNumParams() != 1 || setterDecl->getNumParams() != 2) {
    ++InvalidPropertyInstanceNumParams;
    return false;
  }
  auto selfTy = getterDecl->getParamDecl(0)->getType();

  clang::QualType setterTy = {};
  auto setterParam0Ty = setterDecl->getParamDecl(0)->getType();
  auto setterParam1Ty = setterDecl->getParamDecl(1)->getType();

  if (rouglyEqual(setterParam0Ty, selfTy)) {
    setterTy = setterParam1Ty;
  } else if (rouglyEqual(setterParam1Ty, selfTy)) {
    setterTy = setterParam0Ty;
  } else {
    ++InvalidPropertyInstanceNoSelf;
    return false;
  }

  if (!rouglyEqual(setterTy, getterTy)) {
    ++InvalidPropertyInstanceGetterSetterType;
    return false;
  }

  return true;
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
    auto fail = [varDecl]() -> IAMResult {
      DEBUG(llvm::dbgs() << "failed to infer variable: ");
      DEBUG(varDecl->print(llvm::dbgs()));
      DEBUG(llvm::dbgs() << "\n");
      ++FailInferVar;
      return {};
    };

    // Try to find a type to add this as a static property to
    StringRef workingName = varDecl->getName();
    if (workingName.empty())
      return fail();

    // Special pattern: constants of the form "kFooBarBaz", extend "FooBar" with
    // property "Baz"
    if (*camel_case::getWords(workingName).begin() == "k") {
      workingName = workingName.drop_front(1);
    }

    NameBuffer remainingName;
    const clang::TypeDecl *tyDecl =
        findTypeAndMatch(workingName, remainingName);
    if (tyDecl)
      return importAsStaticProperty(remainingName, getEffectiveDC(tyDecl));
    return fail();
  }

  // Try to infer a member function
  auto funcDecl = dyn_cast<clang::FunctionDecl>(clangDecl);
  if (!funcDecl) {
    // TODO: Do we want to collects stats here? Should it be assert?
    return {};
  }

  auto fail = [funcDecl]() -> IAMResult {
    DEBUG(llvm::dbgs() << "failed to infer function: ");
    DEBUG(funcDecl->print(llvm::dbgs()));
    DEBUG(llvm::dbgs() << "\n");
    ++FailInferFunction;
    return {};
  };

  // Can't really import variadics well
  if (funcDecl->isVariadic())
    return fail();

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
            const clang::FunctionDecl *pairedAccessor;
            if (validToImportAsProperty(funcDecl, propSpec, selfIdx,
                                        pairedAccessor))
              return importAsInstanceProperty(propName, propSpec, selfIdx,
                                              nonSelfParams, pairedAccessor,
                                              effectiveDC);
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
        const clang::FunctionDecl *pairedAccessor;
        if (validToImportAsProperty(funcDecl, propSpec, None, pairedAccessor))
          return importAsStaticProperty(propName, propSpec, nonSelfParams,
                                        pairedAccessor, effectiveDC);
      }
    }

    return importAsStaticMethod(remainingName, nonSelfParams, effectiveDC);
  }

  return fail();
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
