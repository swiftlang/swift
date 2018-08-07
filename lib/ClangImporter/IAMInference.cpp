//===--- IAMInference.cpp - Import as member inference system -------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements support for inferring when globals can be imported as
// members
//
//===----------------------------------------------------------------------===//
#include "CFTypeInfo.h"
#include "IAMInference.h"
#include "ImporterImpl.h"

#include "swift/AST/ASTContext.h"
#include "swift/Basic/StringExtras.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/Lex/Preprocessor.h"
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

// Specifically skipped/avoided
STATISTIC(SkipLeadingUnderscore,
          "# of globals skipped due to leading underscore");
STATISTIC(SkipCFMemoryManagement,
          "# of CF memory management globals skipped");

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

// Omit needless words stats
STATISTIC(OmitNumTimes,
          "# of times omitNeedlessWords was able to fire on an API");

using namespace swift;
using namespace importer;

using NameBuffer = SmallString<32>;

static const std::array<StringRef, 2> InitSpecifiers{{
    StringRef("Create"), StringRef("Make"),
}};
static const std::array<StringRef, 2> PropertySpecifiers{{
    StringRef("Get"), StringRef("Set"),
}};

IAMOptions IAMOptions::getDefault() { return {}; }

// As append, but skip a repeated word at the boundary. First-letter-case
// insensitive.
// Example: appendUniq("FooBar", "barBaz") ==> "FooBarBaz"
static void appendUniq(NameBuffer &src, StringRef toAppend) {
  if (src.empty()) {
    src = toAppend;
    return;
  }

  auto appendWords = camel_case::getWords(toAppend);
  StringRef lastWord = *camel_case::getWords(src).rbegin();
  auto wI = appendWords.begin();
  while (wI != appendWords.end() && wI->equals_lower(lastWord))
    ++wI;
  src.append(wI.getRestOfStr());
}

static StringRef skipLeadingUnderscores(StringRef str) {
  while (!str.empty() && str.startswith("_"))
    str = str.drop_front(1);
  return str;
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
  while (wI != rightWords.end() && wI->equals_lower(lastWord))
    ++wI;

  formHumbleCamelName(left, out);
  camel_case::appendSentenceCase(out, wI.getRestOfStr());
}

static bool hasWord(StringRef s, StringRef matchWord) {
  for (auto word : camel_case::getWords(s))
    if (word == matchWord)
      return true;
  return false;
}

// Drops the specified word, and returns the number of times it was dropped.
// When forming the resultant string, will call appendUniq to skip repeated
// words at the boundary.
static unsigned dropWordUniq(StringRef str, StringRef word, NameBuffer &out) {
  unsigned numDropped = 0;
  auto words = camel_case::getWords(str);
  for (auto wI = words.begin(), wE = words.end(); wI != wE; ++wI)
    if (*wI == word)
      ++numDropped;
    else
      appendUniq(out, *wI);

  return numDropped;
}

static clang::Module *getSubmodule(const clang::NamedDecl *decl, clang::Sema &clangSema) {
  if (auto m = decl->getImportedOwningModule())
    return m;
  if (auto m = decl->getLocalOwningModule())
    return m;
  if (auto m = clangSema.getPreprocessor().getCurrentModule())
    return m;
  if (auto m = clangSema.getPreprocessor().getCurrentLexerSubmodule())
    return m;

  return nullptr;
}

static clang::Module *getTopModule(clang::Module *m) {
  while (m->Parent)
    m = m->Parent;
  return m;
}
static clang::Module *getTopModule(const clang::NamedDecl *decl, clang::Sema &clangSema) {
  auto m = getSubmodule(decl, clangSema);
  if (!m)
    return nullptr;
  return getTopModule(m);
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
  IAMResult inferVar(const clang::VarDecl *);

private:
  // typeID
  IAMResult importAsTypeID(const clang::QualType typeIDTy,
                           EffectiveClangContext effectiveDC) {
    ++SuccessImportAsTypeID;
    return {formDeclName("typeID", /*isInitializer=*/false),
            IAMAccessorKind::Getter, effectiveDC};
  }

  // Init
  IAMResult importAsConstructor(StringRef name, StringRef initSpecifier,
                                ArrayRef<const clang::ParmVarDecl *> params,
                                EffectiveClangContext effectiveDC) {
    ++SuccessImportAsConstructor;
    NameBuffer buf;
    StringRef prefix = buf;
    if (name != initSpecifier) {
      assert(name.size() > initSpecifier.size() &&
             "should have more words in it");
      bool didDrop = dropWordUniq(name, initSpecifier, buf);
      (void)didDrop;
      prefix = buf;

      // Skip "with"
      auto prefixWords = camel_case::getWords(prefix);
      if (prefixWords.begin() != prefixWords.end() &&
          (*prefixWords.begin() == "With" || *prefixWords.begin() == "with")) {
        prefix = prefix.drop_front(4);
      }

      // Skip "CF" or "NS"
      prefixWords = camel_case::getWords(prefix);
      if (prefixWords.begin() != prefixWords.end() &&
          (*prefixWords.begin() == "CF" || *prefixWords.begin() == "NS")) {
        prefix = prefix.drop_front(2);
      }

      assert(didDrop != 0 && "specifier not present?");
    }
    return {formDeclName("init", true, params, prefix), effectiveDC};
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

    return {formDeclName(name, /*isInitializer=*/false),
            kind, selfIdx, effectiveDC};
  }

  // Instance method
  IAMResult
  importAsInstanceMethod(StringRef name, unsigned selfIdx,
                         ArrayRef<const clang::ParmVarDecl *> nonSelfParams,
                         EffectiveClangContext effectiveDC) {
    ++SuccessImportAsInstanceMethod;
    return {formDeclName(name, /*isInitializer=*/false, nonSelfParams),
            selfIdx, effectiveDC};
  }

  // Static stored property
  IAMResult importAsStaticProperty(StringRef name,
                                   EffectiveClangContext effectiveDC) {
    ++SuccessImportAsStaticProperty;
    return {formDeclName(name, /*isInitializer=*/false), effectiveDC};
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

    return {formDeclName(name, /*isInitializer=*/false),
            kind, effectiveDC};
  }

  // Static method
  IAMResult
  importAsStaticMethod(StringRef name,
                       ArrayRef<const clang::ParmVarDecl *> nonSelfParams,
                       EffectiveClangContext effectiveDC) {
    ++SuccessImportAsStaticMethod;
    return {formDeclName(name, /*isInitializer=*/false, nonSelfParams),
            effectiveDC};
  }

  Identifier getIdentifier(StringRef str) {
    if (str == "")
      return Identifier();
    return context.getIdentifier(str);
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

  EffectiveClangContext findTypeAndMatch(StringRef workingName,
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
        return getEffectiveDC(clang::QualType(tyDecl->getTypeForDecl(), 0));
      }
      if (auto tyDecl = clangLookupTypeDecl(prefix)) {
        outStr.append(workingName.drop_front(prefix.size()));
        return getEffectiveDC(clang::QualType(tyDecl->getTypeForDecl(), 0));
      }
    }

    return {};
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

  DeclBaseName getHumbleBaseName(StringRef name, bool isInitializer) {
    // Lower-camel-case the incoming name
    NameBuffer buf;
    formHumbleCamelName(name, buf);
    if (isInitializer && buf == "init")
      return DeclBaseName::createConstructor();
    return getIdentifier(buf);
  }

  DeclName formDeclName(StringRef baseName, bool isInitializer) {
    return {getHumbleBaseName(baseName, isInitializer)};
  }

  DeclName formDeclName(StringRef baseName, bool isInitializer,
                        ArrayRef<const clang::ParmVarDecl *> params,
                        StringRef firstPrefix = "") {

    // TODO: redesign from a SmallString to a StringScratchBuffer design for all
    // of this name mangling, since we have to use one for omit needless words
    // anyways

    if (params.empty() && firstPrefix != "") {
      // We need to form an argument label, despite there being no argument
      NameBuffer paramName;
      formHumbleCamelName(firstPrefix, paramName);
      return {context, getHumbleBaseName(baseName, isInitializer),
              getIdentifier(paramName)};
    }

    StringScratchSpace scratch;
    SmallVector<StringRef, 8> argStrs;
    for (unsigned i = 0; i < params.size(); ++i) {
      NameBuffer paramName;
      if (i == 0 && firstPrefix != "") {
        formHumbleCamelName(firstPrefix, params[i]->getName(), paramName);
      } else {
        // TODO: strip leading underscores
        formHumbleCamelName(params[i]->getName(), paramName);
      }

      argStrs.push_back(scratch.copyString(paramName));
    }

    DeclName beforeOmit;
    (void)beforeOmit;
    {
      SmallVector<Identifier, 8> argLabels;
      for (auto str : argStrs)
        argLabels.push_back(getIdentifier(str));
      LLVM_DEBUG((beforeOmit = {context,
                                getHumbleBaseName(baseName, isInitializer),
                                argLabels}));
    }

    SmallVector<OmissionTypeName, 8> paramTypeNames;
    for (auto param : params) {
      paramTypeNames.push_back(getClangTypeNameForOmission(
          clangSema.getASTContext(), param->getType()));
    }

    auto humbleBaseName = getHumbleBaseName(baseName, isInitializer);
    baseName = humbleBaseName.userFacingName();
    bool didOmit =
        omitNeedlessWords(baseName, argStrs, "", "", "", paramTypeNames, false,
                          false, nullptr, scratch);
    SmallVector<Identifier, 8> argLabels;
    for (auto str : argStrs)
      argLabels.push_back(getIdentifier(str));

    DeclName ret(context,
                 getHumbleBaseName(baseName, isInitializer),
                 argLabels);

    if (didOmit) {
      ++OmitNumTimes;
      LLVM_DEBUG(llvm::dbgs() << "omission detected: " << beforeOmit << " ==> "
                              << ret << "\n");
    }

    return ret;
  }

  bool matchTypeName(StringRef str, clang::QualType qt, NameBuffer &outStr);
  bool match(StringRef str, StringRef toMatch, NameBuffer &outStr);

  EffectiveClangContext getEffectiveDC(clang::QualType qt) {
    // Read through some attributes
    while (qt.getTypePtrOrNull() && isa<clang::AttributedType>(qt.getTypePtr()))
      qt = qt.getSingleStepDesugaredType(clangSema.getASTContext());

    // Read through typedefs until we get to a CF typedef or a non-typedef-ed
    // type
    while (qt.getTypePtrOrNull() && isa<clang::TypedefType>(qt.getTypePtr())) {
      auto typedefType = cast<clang::TypedefType>(qt.getTypePtr());
      if (auto pointeeInfo = CFPointeeInfo::classifyTypedef(
              typedefType->getDecl()->getCanonicalDecl())) {
        if (pointeeInfo.isRecord() || pointeeInfo.isTypedef())
          return {typedefType->getDecl()->getCanonicalDecl()};
        assert(pointeeInfo.isVoid() && "no other type");
        return {};
      }
      qt = qt.getSingleStepDesugaredType(clangSema.getASTContext());
    }

    auto pointeeQT = qt.getTypePtr()->getPointeeType();
    if (pointeeQT != clang::QualType())
      // Retry on the pointee
      return getEffectiveDC(pointeeQT);

    if (auto tagDecl = qt.getTypePtr()->getAsTagDecl()) {
      auto canon = tagDecl->getCanonicalDecl();
      if (canon->getDefinition())
        return {canon};

      // TODO: Once the importer learns how to import un-defined structs, then
      // we will be able to infer them. Until that point, we have to bail
      // because ImportDecl won't be able to re-map this.
      return {};
    }

    // Failed to find a type we can extend
    return {};
  }
};
} // end anonymous namespace

static StringRef getTypeName(clang::QualType qt) {
  if (auto typedefTy = qt->getAs<clang::TypedefType>()) {
    // Check for a CF type name (drop the "Ref")
    auto cfName = getCFTypeName(typedefTy->getDecl()->getCanonicalDecl());
    if (cfName != StringRef())
      return cfName;
  }

  auto identInfo = qt.getBaseTypeIdentifier();
  if (identInfo)
    return identInfo->getName();

  // Otherwise, no name
  return {};
}

bool IAMInference::matchTypeName(StringRef str, clang::QualType qt,
                                 NameBuffer &outStr) {
  StringRef typeName = getTypeName(qt);
  if (typeName == "")
    return false;

  // Special case: Mutable can appear in both and may screw up word order. Or,
  // Mutable can occur in the type name only. Either way, we want to have the
  // potential of successfully matching the type.
  NameBuffer nonMutableStr;
  NameBuffer nonMutableTypeName;
  if (hasWord(typeName, "Mutable")) {
    dropWordUniq(typeName, "Mutable", nonMutableTypeName);
    typeName = nonMutableTypeName;
    if (hasWord(str, "Mutable")) {
      dropWordUniq(str, "Mutable", nonMutableStr);
      str = nonMutableStr;
    }
  }

  return match(str, typeName, outStr);
}

bool IAMInference::match(StringRef str, StringRef toMatch, NameBuffer &outStr) {
  // TODO: let options dictate fuzzy matching...

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

// A loose type equality check that disregards all sugar, qualification, looks
// through pointers, etc.
static bool roughlyEqual(clang::QualType left, clang::QualType right) {
  auto leftPointee = left->getPointeeType();
  if (leftPointee != clang::QualType())
    left = leftPointee;
  auto rightPointee = right->getPointeeType();
  if (rightPointee != clang::QualType())
    right = rightPointee;
  return left->getUnqualifiedDesugaredType() ==
         right->getUnqualifiedDesugaredType();
}

static bool
isValidAsStaticProperty(const clang::FunctionDecl *getterDecl,
                        const clang::FunctionDecl *setterDecl = nullptr) {
  // Getter has none, setter has one arg
  if (getterDecl->getNumParams() != 0 ||
      (setterDecl && setterDecl->getNumParams() != 1)) {
    ++InvalidPropertyStaticNumParams;
    return false;
  }

  // Setter's arg type should be same as getter's return type
  auto getterTy = getterDecl->getReturnType();
  if (setterDecl &&
      !roughlyEqual(getterTy, setterDecl->getParamDecl(0)->getType())) {
    ++InvalidPropertyStaticGetterSetterType;
    return false;
  }

  return true;
}

static bool
isValidAsInstanceProperty(const clang::FunctionDecl *getterDecl,
                          const clang::FunctionDecl *setterDecl = nullptr) {
  // Instance property, look beyond self
  if (getterDecl->getNumParams() != 1 ||
      (setterDecl && setterDecl->getNumParams() != 2)) {
    ++InvalidPropertyInstanceNumParams;
    return false;
  }

  if (!setterDecl)
    return true;

  // Make sure they pair up
  auto getterTy = getterDecl->getReturnType();
  auto selfTy = getterDecl->getParamDecl(0)->getType();

  clang::QualType setterTy = {};
  auto setterParam0Ty = setterDecl->getParamDecl(0)->getType();
  auto setterParam1Ty = setterDecl->getParamDecl(1)->getType();

  if (roughlyEqual(setterParam0Ty, selfTy)) {
    setterTy = setterParam1Ty;
  } else if (roughlyEqual(setterParam1Ty, selfTy)) {
    setterTy = setterParam0Ty;
  } else {
    ++InvalidPropertyInstanceNoSelf;
    return false;
  }

  if (!roughlyEqual(setterTy, getterTy)) {
    ++InvalidPropertyInstanceGetterSetterType;
    return false;
  }

  return true;
}

bool IAMInference::validToImportAsProperty(
    const clang::FunctionDecl *originalDecl, StringRef propSpec,
    Optional<unsigned> selfIndex, const clang::FunctionDecl *&pairedAccessor) {
  bool isGet = propSpec == "Get";
  pairedAccessor = findPairedAccessor(originalDecl->getName(), propSpec);
  if (!pairedAccessor) {
    if (!isGet)
      return false;
    if (!selfIndex)
      return isValidAsStaticProperty(originalDecl);
    return isValidAsInstanceProperty(originalDecl);
  }

  auto getterDecl = isGet ? originalDecl : pairedAccessor;
  auto setterDecl = isGet ? pairedAccessor : originalDecl;

  if (getTopModule(getterDecl, clangSema) !=
      getTopModule(setterDecl, clangSema)) {
    // We paired up decls from two different modules, so either we still infer
    // as a getter with no setter, or we cannot be a property
    if (isGet) {
      pairedAccessor = nullptr;
      setterDecl = nullptr;
    } else  {
      // This is set-only as far as we're concerned
      return false;
    }
  }

  if (!selfIndex)
    return isValidAsStaticProperty(getterDecl, setterDecl);

  return isValidAsInstanceProperty(getterDecl, setterDecl);
}

IAMResult IAMInference::inferVar(const clang::VarDecl *varDecl) {
  auto fail = [varDecl]() -> IAMResult {
    LLVM_DEBUG(llvm::dbgs() << "failed to infer variable: ");
    LLVM_DEBUG(varDecl->print(llvm::dbgs()));
    (void)varDecl;
    LLVM_DEBUG(llvm::dbgs() << "\n");
    ++FailInferVar;
    return {};
  };

  // Try to find a type to add this as a static property to
  StringRef workingName = varDecl->getName();
  if (workingName.empty())
    return fail();

  // Special pattern: constants of the form "kFooBarBaz", extend "FooBar" with
  // property "Baz"
  if (*camel_case::getWords(workingName).begin() == "k")
    workingName = workingName.drop_front(1);

  NameBuffer remainingName;
  if (auto effectiveDC = findTypeAndMatch(workingName, remainingName))

    return importAsStaticProperty(remainingName, effectiveDC);

  return fail();
}

IAMResult IAMInference::infer(const clang::NamedDecl *clangDecl) {
  if (clangDecl->getName().startswith("_")) {
    ++SkipLeadingUnderscore;
    return {};
  }

  // Try to infer a member variable
  if (auto varDecl = dyn_cast<clang::VarDecl>(clangDecl))
    return inferVar(varDecl);

  // Try to infer a member function
  auto funcDecl = dyn_cast<clang::FunctionDecl>(clangDecl);
  if (!funcDecl) {
    // TODO: Do we want to collects stats here? Should it be assert?
    return {};
  }

  auto fail = [funcDecl]() -> IAMResult {
    LLVM_DEBUG(llvm::dbgs() << "failed to infer function: ");
    LLVM_DEBUG(funcDecl->print(llvm::dbgs()));
    (void)funcDecl;
    LLVM_DEBUG(llvm::dbgs() << "\n");
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

  // 0) Special cases are specially handled
  //
  StringRef getTypeID = "GetTypeID";
  StringRef cfSpecials[] = {"Release", "Retain", "Autorelease"};
  // *GetTypeID
  if (numParams == 0 && workingName.endswith(getTypeID)) {
    NameBuffer remainingName;
    if (auto effectiveDC = findTypeAndMatch(
            workingName.drop_back(getTypeID.size()), remainingName)) {
      // We shouldn't have anything else left in our name for typeID
      if (remainingName.empty()) {

        return importAsTypeID(retTy, effectiveDC);
      }
    }
    // *Release/*Retain/*Autorelease
  } else if (numParams == 1 &&
             std::any_of(std::begin(cfSpecials), std::end(cfSpecials),
                         [workingName](StringRef suffix) {
                           return workingName.endswith(suffix);
                         })) {
    if (auto type =
            funcDecl->getParamDecl(0)->getType()->getAs<clang::TypedefType>()) {
      if (CFPointeeInfo::classifyTypedef(type->getDecl())) {
        ++SkipCFMemoryManagement;
        return {};
      }
    }
  }

  // 1) If we find an init specifier and our name matches the return type, we
  //    import as some kind of constructor
  //
  if (!retTy->isVoidType()) {
    NameBuffer remainingName;
    if (matchTypeName(workingName, retTy, remainingName))
      for (auto initSpec : InitSpecifiers)
        if (hasWord(remainingName, initSpec))
          if (auto effectiveDC = getEffectiveDC(retTy))
            return importAsConstructor(
                remainingName, initSpec,
                {funcDecl->param_begin(), funcDecl->param_end()}, effectiveDC);
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
    if (matchTypeName(workingName, param->getType(), remainingName)) {
      auto effectiveDC = getEffectiveDC(param->getType());
      if (!effectiveDC)
        continue;
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

  // No self, must be static
  nonSelfParams = {funcDecl->param_begin(), funcDecl->param_end()};

  // 3) Finally, try to find a class to put this on as a static function
  NameBuffer remainingName;
  if (auto effectiveDC = findTypeAndMatch(workingName, remainingName)) {
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
    StringRef methodName =
        remainingName == "" ? workingName : StringRef(remainingName);
    return importAsStaticMethod(methodName, nonSelfParams, effectiveDC);
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
