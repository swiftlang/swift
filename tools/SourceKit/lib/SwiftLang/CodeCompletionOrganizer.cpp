//===--- CodeCompletionOrganizer.cpp --------------------------------------===//
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

#include "CodeCompletionOrganizer.h"
#include "SourceKit/Support/FuzzyStringMatcher.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Module.h"
#include "swift/Frontend/Frontend.h"
#include "clang/Basic/CharInfo.h"
#include "clang/Basic/Module.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/ilist.h"
#include "llvm/ADT/ilist_node.h"
#include <deque>

using namespace SourceKit;
using namespace CodeCompletion;
using namespace swift;
using namespace ide;

namespace {
enum class ItemKind : uint8_t {
  None,
  Group,
  Result,
};
struct Item {
  std::string name;
  std::string description;
  uint8_t kind : 2;
  double matchScore = 0.0; ///< The quality of the filter matching.
  double finalScore = -1.0; ///< The final score including match and context.
  ItemKind getKind() const { return static_cast<ItemKind>(kind); }
  Item(ItemKind k = ItemKind::None) : kind(static_cast<decltype(kind)>(k)) {}
  virtual ~Item() {}
};
struct Result : public Item {
  Completion *value;
  Result(Completion *result = nullptr)
      : Item(ItemKind::Result), value(result) {}
  static bool classof(const Item *item) {
    return item->getKind() == ItemKind::Result;
  }
};
class ImportDepth {
  llvm::StringMap<uint8_t> depths;

public:
  ImportDepth() = default;
  ImportDepth(ASTContext &context, CompilerInvocation &invocation);

  Optional<uint8_t> lookup(StringRef module) {
    auto I = depths.find(module);
    if (I == depths.end())
      return None;
    return I->getValue();
  }
};
} // end anonymous namespace

struct CodeCompletion::Group : public Item {
  std::vector<std::unique_ptr<Item>> contents;
  Group() : Item(ItemKind::Group) {}
  static bool classof(const Item *item) {
    return item->getKind() == ItemKind::Group;
  }
};

//===----------------------------------------------------------------------===//
// extendCompletions
//===----------------------------------------------------------------------===//

std::vector<Completion *> SourceKit::CodeCompletion::extendCompletions(
    ArrayRef<SwiftResult *> swiftResults, CompletionSink &sink,
    SwiftCompletionInfo &info, const NameToPopularityMap *nameToPopularity,
    const Options &options, Completion *prefix,
    Optional<SemanticContextKind> overrideContext,
    Optional<SemanticContextKind> overrideOperatorContext) {

  ImportDepth depth;
  if (info.swiftASTContext) {
    // Build import depth map.
    depth = ImportDepth(*info.swiftASTContext, *info.invocation);
  }

  if (info.completionContext)
    sink.adoptSwiftSink(info.completionContext->getResultSink());

  std::vector<Completion *> results;
  for (auto *result : swiftResults) {
    CompletionBuilder builder(sink, *result);
    if (result->getSemanticContext() == SemanticContextKind::OtherModule)
      builder.setModuleImportDepth(depth.lookup(result->getModuleName()));

    if (prefix) {
      builder.setPrefix(prefix->getCompletionString());
      builder.setSemanticContext(prefix->getSemanticContext());
    }

    if (overrideOperatorContext && result->isOperator()) {
      builder.setSemanticContext(*overrideOperatorContext);
    } else if (overrideContext) {
      builder.setSemanticContext(*overrideContext);
    }

    // If this result is not from the current module, try to get a popularity
    // score for it.
    if (nameToPopularity) {
      builder.setPopularityFactor(
          nameToPopularity->lookup(builder.getOriginalName()));
    }

    results.push_back(builder.finish());
  }

  return results;
}

static StringRef copyString(llvm::BumpPtrAllocator &allocator, StringRef str);

bool SourceKit::CodeCompletion::addCustomCompletions(
    CompletionSink &sink, std::vector<Completion *> &completions,
    ArrayRef<CustomCompletionInfo> customCompletions,
    CompletionKind completionKind) {

  auto addCompletion = [&](CustomCompletionInfo customCompletion) {
    using Chunk = CodeCompletionString::Chunk;
    auto nameCopy = copyString(sink.allocator, customCompletion.Name);
    auto chunk = Chunk::createWithText(Chunk::ChunkKind::Text, 0, nameCopy);
    auto *completionString =
        CodeCompletionString::create(sink.allocator, chunk);
    CodeCompletion::SwiftResult swiftResult(
        CodeCompletion::SwiftResult::ResultKind::Pattern,
        SemanticContextKind::ExpressionSpecific,
        /*numBytesToErase=*/0, completionString);

    CompletionBuilder builder(sink, swiftResult);
    builder.setCustomKind(customCompletion.Kind);
    completions.push_back(builder.finish());
  };

  bool changed = false;

  for (auto &custom : customCompletions) {
    switch (completionKind) {
    case CompletionKind::StmtOrExpr:
      if (custom.Contexts.contains(CustomCompletionInfo::Stmt)) {
        changed = true;
        addCompletion(custom);
      }
      break;
    case CompletionKind::PostfixExprBeginning:
    case CompletionKind::AssignmentRHS:
    case CompletionKind::CallArg:
    case CompletionKind::ReturnStmtExpr:
      if (custom.Contexts.contains(CustomCompletionInfo::Expr)) {
        changed = true;
        addCompletion(custom);
      }
      break;
    case CompletionKind::TypeSimpleBeginning:
      if (custom.Contexts.contains(CustomCompletionInfo::Type)) {
        changed = true;
        addCompletion(custom);
      }
      break;
    default:
      break;
    }
  }

  return changed;
}

//===----------------------------------------------------------------------===//
// CodeCompletionOrganizer::Impl declaration
//===----------------------------------------------------------------------===//

class CodeCompletionOrganizer::Impl {
  std::unique_ptr<Group> rootGroup;
  CompletionKind completionKind;

  void groupStemsRecursive(Group *group, bool recurseIntoNewGroups,
                           StringRef(getStem)(StringRef));

public:
  Impl(CompletionKind kind);

  void addCompletionsWithFilter(ArrayRef<Completion *> completions,
                                StringRef filterText, Options options,
                                Completion *&exactMatch);

  void sort(Options options);

  void groupOverloads() {
    groupStemsRecursive(
        rootGroup.get(), /*recurseIntoNewGroups*/ false, [](StringRef name) {
          auto endIdx = name.find_first_of("([");
          if (endIdx == 0 && !name.empty())
            return name.slice(0, 1); // [ => subscript, ( => initializer
          return name.slice(0, endIdx);
        });
  }

  void groupStems() {
    groupStemsRecursive(rootGroup.get(), /*recurseIntoNewGroups*/ true,
        [](StringRef name) {
          unsigned i = 0;
          while (i < name.size()) {
            char c = name[i];
            // FIXME: unicode
            if (i > 0 && clang::isUppercase(c) &&
                !clang::isUppercase(name[i - 1]))
              break;
            if (!clang::isAlphanumeric(c))
              break;
            ++i;
          }
          if (i == 0 && !name.empty())
            return name.slice(0, 1);
          return name.slice(0, i);
        });
  }

  CodeCompletionViewRef takeView() {
    assert(rootGroup);
    auto view = std::make_shared<CodeCompletionView>();
    view->rootGroup = rootGroup.release(); // View takes ownership.

    // Reset fields manually in case the move constructors don't.
    rootGroup = nullptr;

    return view;
  }
};

//===----------------------------------------------------------------------===//
// CodeCompletionOrganizer implementation
//===----------------------------------------------------------------------===//

CodeCompletionOrganizer::CodeCompletionOrganizer(const Options &options,
                                                 CompletionKind kind)
    : impl(*new Impl(kind)), options(options) {}
CodeCompletionOrganizer::~CodeCompletionOrganizer() { delete &impl; }

void CodeCompletionOrganizer::preSortCompletions(
    llvm::MutableArrayRef<Completion *> completions) {
  // We do a case-sensitive sort here, then do a case-insensitive sort after any
  // name-based grouping.

  llvm::array_pod_sort(completions.begin(), completions.end(),
                       [](Completion *const *a, Completion *const *b) {

     // Sort first by filter name (case-sensitive).
     if (int primary = (*a)->getName().compare((*b)->getName()))
       return primary;

     // Next, sort by full description text.
     return (*a)->getDescription().compare((*b)->getDescription());
   });
}

void CodeCompletionOrganizer::addCompletionsWithFilter(
    ArrayRef<Completion *> completions, StringRef filterText,
    Completion *&exactMatch) {
  impl.addCompletionsWithFilter(completions, filterText, options, exactMatch);
}

void CodeCompletionOrganizer::groupAndSort(const Options &options) {
  if (options.groupStems)
    impl.groupStems();
  else if (options.groupOverloads)
    impl.groupOverloads();

  impl.sort(options);
}

CodeCompletionViewRef CodeCompletionOrganizer::takeResultsView() {
  return impl.takeView();
}

//===----------------------------------------------------------------------===//
// ImportDepth
//===----------------------------------------------------------------------===//

ImportDepth::ImportDepth(ASTContext &context, CompilerInvocation &invocation) {
  llvm::DenseSet<Module *> seen;
  std::deque<std::pair<Module *, uint8_t>> worklist;

  StringRef mainModule = invocation.getModuleName();
  auto *main = context.getLoadedModule(context.getIdentifier(mainModule));
  assert(main && "missing main module");
  worklist.emplace_back(main, uint8_t(0));

  // Imports from -import-name such as Playground auxiliary sources are treated
  // specially by applying import depth 0.
  llvm::StringSet<> auxImports;
  for (StringRef moduleName :
       invocation.getFrontendOptions().ImplicitImportModuleNames)
    auxImports.insert(moduleName);

  // Private imports from this module.
  // FIXME: only the private imports from the current source file.
  SmallVector<Module::ImportedModule, 16> mainImports;
  main->getImportedModules(mainImports, Module::ImportFilter::Private);
  for (auto &import : mainImports) {
    uint8_t depth = 1;
    if (auxImports.count(import.second->getName().str()))
      depth = 0;
    worklist.emplace_back(import.second, depth);
  }

  // Fill depths with BFS over module imports.
  while (!worklist.empty()) {
    Module *module;
    uint8_t depth;
    std::tie(module, depth) = worklist.front();
    worklist.pop_front();

    if (!seen.insert(module).second)
      continue;

    // Insert new module:depth mapping.
    const clang::Module *CM = module->findUnderlyingClangModule();
    if (CM) {
      depths[CM->getFullModuleName()] = depth;
    } else {
      depths[module->getName().str()] = depth;
    }

    // Add imports to the worklist.
    SmallVector<Module::ImportedModule, 16> imports;
    module->getImportedModules(imports);
    for (auto &import : imports) {
      uint8_t next = std::max(depth, uint8_t(depth + 1)); // unsigned wrap

      // Implicitly imported sub-modules get the same depth as their parent.
      if (const clang::Module *CMI = import.second->findUnderlyingClangModule())
        if (CM && CMI->isSubModuleOf(CM))
          next = depth;
      worklist.emplace_back(import.second, next);
    }
  }
}

//===----------------------------------------------------------------------===//
// CodeCompletionOrganizer::Impl utilities
//===----------------------------------------------------------------------===//

static StringRef copyString(llvm::BumpPtrAllocator &allocator, StringRef str) {
  char *newStr = allocator.Allocate<char>(str.size());
  std::copy(str.begin(), str.end(), newStr);
  return StringRef(newStr, str.size());
}

static std::unique_ptr<Group> make_group(StringRef name) {
  auto g = llvm::make_unique<Group>();
  g->name = name;
  g->description = name;
  return g;
}

static std::unique_ptr<Result> make_result(Completion *result) {
  auto r = llvm::make_unique<Result>(result);
  r->name = result->getName();
  r->description = result->getDescription();
  return r;
}


//===----------------------------------------------------------------------===//
// CodeCompletionOrganizer::Impl implementation
//===----------------------------------------------------------------------===//

CodeCompletionOrganizer::Impl::Impl(CompletionKind kind)
    : completionKind(kind) {
  assert(!rootGroup && "initialized twice");
  rootGroup = make_group("");
}

static bool matchesExpectedStyle(Completion *completion, NameStyle style) {
  switch (completion->getAssociatedDeclKind()) {
  case CodeCompletionDeclKind::Class:
  case CodeCompletionDeclKind::Struct:
  case CodeCompletionDeclKind::Enum:
  case CodeCompletionDeclKind::Protocol:
  case CodeCompletionDeclKind::TypeAlias:
    return style.possiblyUpperCamelCase();

  case CodeCompletionDeclKind::StaticMethod:
  case CodeCompletionDeclKind::InstanceMethod:
  case CodeCompletionDeclKind::FreeFunction:
  case CodeCompletionDeclKind::StaticVar:
  case CodeCompletionDeclKind::InstanceVar:
  case CodeCompletionDeclKind::LocalVar:
  case CodeCompletionDeclKind::GlobalVar:
    return style.possiblyLowerCamelCase();

  default:
    return true; // Conservatively say yes.
  }
}

static bool isHighPriorityKeyword(CodeCompletionKeywordKind kind) {
  switch (kind) {
  case CodeCompletionKeywordKind::kw_let:
  case CodeCompletionKeywordKind::kw_var:
  case CodeCompletionKeywordKind::kw_if:
  case CodeCompletionKeywordKind::kw_for:
  case CodeCompletionKeywordKind::kw_while:
  case CodeCompletionKeywordKind::kw_func:
    return true;
  default:
    return false;
  }
}

void CodeCompletionOrganizer::Impl::addCompletionsWithFilter(
    ArrayRef<Completion *> completions, StringRef filterText, Options options,
    Completion *&exactMatch) {
  assert(rootGroup);

  auto &contents = rootGroup->contents;

  // If we have no filter text, add all non-hidden results.
  if (filterText.empty()) {
    bool hideLowPriority =
        options.hideLowPriority &&
        completionKind != CompletionKind::TypeSimpleBeginning &&
        completionKind != CompletionKind::PostfixExpr;
    for (Completion *completion : completions) {
      NameStyle style(completion->getName());
      bool hideUnderscore = options.hideUnderscores && style.leadingUnderscores;
      if (hideUnderscore && options.reallyHideAllUnderscores)
        continue;

      bool hideByNameStyle = options.hideByNameStyle &&
                             completion->getKind() == Completion::Declaration &&
                             !matchesExpectedStyle(completion, style);

      hideByNameStyle |= hideUnderscore;

      switch (completion->getSemanticContext()) {
      case SemanticContextKind::Super:
      case SemanticContextKind::OutsideNominal:
        if (hideUnderscore)
          continue;
        break;
      case SemanticContextKind::OtherModule:
      case SemanticContextKind::None:
        if (auto depth = completion->getModuleImportDepth()) {
          if (*depth == 0) // Treat as if it's "thismodule"
            break;
        }
        if (completion->getExpectedTypeRelation() >= Completion::Convertible ||
            (completion->getKind() == Completion::Literal &&
             completionKind != CompletionKind::StmtOrExpr))
          break;

        if (completion->getKind() == Completion::Keyword &&
            completionKind == CompletionKind::StmtOrExpr &&
            isHighPriorityKeyword(completion->getKeywordKind()))
          break;

        if (hideByNameStyle || hideLowPriority)
          continue;
        break;
      default:
        break;
      }

      // Build wrapper and add to results.
      contents.push_back(make_result(completion));
    }
    return;
  }

  FuzzyStringMatcher pattern(filterText);
  pattern.normalize = true;
  for (Completion *completion : completions) {
    bool match = false;
    if (options.fuzzyMatching && filterText.size() >= options.minFuzzyLength) {
      match = pattern.matchesCandidate(completion->getName());
    } else {
      match = completion->getName().startswith_lower(filterText);
    }

    if (match && completion->getName().equals_lower(filterText)) {
      if (!exactMatch)
        exactMatch = completion;
      match = (options.addInnerResults || options.addInnerOperators)
                  ? options.includeExactMatch
                  : true;
    }

    // Build wrapper and add to results.
    if (match) {
      auto wrapper = make_result(completion);
      if (options.fuzzyMatching)
        wrapper->matchScore = pattern.scoreCandidate(completion->getName());
      contents.push_back(std::move(wrapper));
    }
  }
}

static double getSemanticContextScore(bool useImportDepth,
                                      Completion *completion) {
  double order = -1.0;
  switch (completion->getSemanticContext()) {
  case SemanticContextKind::ExpressionSpecific: order = 0; break;
  case SemanticContextKind::Local: order = 1; break;
  case SemanticContextKind::CurrentNominal: order = 2; break;
  case SemanticContextKind::Super: order = 3; break;
  case SemanticContextKind::OutsideNominal: order = 4; break;
  case SemanticContextKind::CurrentModule: order = 5; break;
  case SemanticContextKind::OtherModule: {
    unsigned depth = Completion::maxModuleImportDepth + 1; // unknown > known
    if (useImportDepth && completion->getModuleImportDepth())
      depth = *completion->getModuleImportDepth();
    // We treat depth == 0 the same as CurrentModule.
    order = (depth == 0) ? 5.0 : 6.0; // Base value.
    order += double(depth) / (Completion::maxModuleImportDepth + 1);
    assert((depth == 0 && order == 5.0) ||
           (depth && 6.0 <= order && order <= 7.0));
    break;
  }
  case SemanticContextKind::None: order = 8.0; break;
  }
  assert(0.0 <= order && order <= 8.0);
  return (8.0 - order) / 8.0;
}

static double combinedScore(const Options &options, double matchScore,
                            Completion *completion) {

  double score = matchScore * options.fuzzyMatchWeight;
  score += getSemanticContextScore(options.useImportDepth, completion) *
           options.semanticContextWeight;

  PopularityFactor popularity = completion->getPopularityFactor();
  if (popularity.isPopular())
    score += popularity.rawValue + options.popularityBonus;
  else if (popularity.isUnpopular())
    score += popularity.rawValue - options.popularityBonus;

  // Add a tiny score boost to prioritize certain operators.
  // FIXME: we need a better way to prioritize known operators.
  if (completion->getKind() == Completion::Pattern) {
    if (completion->getName().endswith(".")) {
      score += 0.02;
    } else if (completion->getName().endswith("(")) {
      score += 0.01;
    }
  }

  return score;
}

static int compareResultName(Item &a, Item &b) {
  // Sort first by filter name (case-insensitive).
  if (int primary = StringRef(a.name).compare_lower(b.name))
    return primary;

  // Next, sort by full description text.
  return a.description.compare(b.description);
};

namespace {
enum class ResultBucket {
  Normal,
  Literal,
  NormalTypeMatch,
  LiteralTypeMatch,
  HighPriorityKeyword,
  ExpressionSpecific,
};
} // end anonymous namespace

static ResultBucket getResultBucket(Item &item) {
  if (isa<Group>(item))
    return ResultBucket::Normal; // FIXME: take best contained result.
  auto *completion = cast<Result>(item).value;

  if (completion->getSemanticContext() ==
      SemanticContextKind::ExpressionSpecific)
    return ResultBucket::ExpressionSpecific;

  bool matchesType =
      completion->getExpectedTypeRelation() >= Completion::Convertible;

  switch (completion->getKind()) {
  case Completion::Literal:
    return matchesType ? ResultBucket::LiteralTypeMatch : ResultBucket::Literal;
  case Completion::Keyword:
    return isHighPriorityKeyword(completion->getKeywordKind())
               ? ResultBucket::HighPriorityKeyword
               : ResultBucket::Normal;
  case Completion::Pattern:
  case Completion::Declaration:
    return matchesType ? ResultBucket::NormalTypeMatch : ResultBucket::Normal;
  }
}

static int compareHighPriorityKeywords(Item &a_, Item &b_) {
  static CodeCompletionKeywordKind order[] = {
    CodeCompletionKeywordKind::kw_let,
    CodeCompletionKeywordKind::kw_var,
    CodeCompletionKeywordKind::kw_if,
    CodeCompletionKeywordKind::kw_for,
    CodeCompletionKeywordKind::kw_while,
    CodeCompletionKeywordKind::kw_func,
  };
  auto size = sizeof(order) / sizeof(order[0]);

  auto getIndex = [=](Item &item) {
     auto I = std::find(order, &order[size], cast<Result>(item).value->getKeywordKind());
    assert(I != &order[size]);
    return std::distance(order, I);
  };

  auto a = getIndex(a_);
  auto b = getIndex(b_);
  return a < b ? -1 : (b < a ? 1 : 0);
}

static int compareLiterals(Item &a_, Item &b_) {
  static CodeCompletionLiteralKind order[] = {
    CodeCompletionLiteralKind::IntegerLiteral,
    CodeCompletionLiteralKind::FloatLiteral,
    CodeCompletionLiteralKind::BooleanLiteral,
    CodeCompletionLiteralKind::StringLiteral,
    CodeCompletionLiteralKind::ArrayLiteral,
    CodeCompletionLiteralKind::DictionaryLiteral,
    CodeCompletionLiteralKind::Tuple,
    CodeCompletionLiteralKind::NilLiteral,
    CodeCompletionLiteralKind::ColorLiteral,
  };
  auto size = sizeof(order) / sizeof(order[0]);

  auto getIndex = [=](Item &item) {
    auto I = std::find(order, &order[size], cast<Result>(item).value->getLiteralKind());
    assert(I != &order[size]);
    return std::distance(order, I);
  };

  auto a = getIndex(a_);
  auto b = getIndex(b_);
  return a < b ? -1 : (b < a ? 1 : 0);
}

static void sortRecursive(const Options &options, Group *group) {
  // Sort all of the subgroups first, and fill in the bucket for each result.
  auto &contents = group->contents;
  double best = -1.0;
  for (auto &item : contents) {
    if (Group *g = dyn_cast<Group>(item.get())) {
      sortRecursive(options, g);
    } else {
      Result *r = cast<Result>(item.get());
      item->finalScore = combinedScore(options, item->matchScore, r->value);
    }

    if (item->finalScore > best)
      best = item->finalScore;
  }

  group->finalScore = best;

  // Now sort the group itself.

  if (options.sortByName) {
    llvm::array_pod_sort(contents.begin(), contents.end(),
        [](const std::unique_ptr<Item> *a, const std::unique_ptr<Item> *b) {
      return compareResultName(**a, **b);
    });
    return;
  }

  llvm::array_pod_sort(contents.begin(), contents.end(), [](const std::unique_ptr<Item> *a_, const std::unique_ptr<Item> *b_) {
    Item &a = **a_;
    Item &b = **b_;

    auto bucketA = getResultBucket(a);
    auto bucketB = getResultBucket(b);
    if (bucketA < bucketB)
      return 1;
    else if (bucketB < bucketA)
      return -1;

    // Special internal orderings.
    switch (bucketA) {
    case ResultBucket::HighPriorityKeyword:
      return compareHighPriorityKeywords(a, b);
    case ResultBucket::Literal:
    case ResultBucket::LiteralTypeMatch:
      return compareLiterals(a, b);
    default:
      break;
    }

    // "Normal" order.
    if (a.finalScore < b.finalScore)
      return 1;
    else if (b.finalScore < a.finalScore)
      return -1;

    return compareResultName(a, b);
  });
}

void CodeCompletionOrganizer::Impl::sort(Options options) {
  sortRecursive(options, rootGroup.get());
}

void CodeCompletionOrganizer::Impl::groupStemsRecursive(
    Group *group, bool recurseIntoNewGroups, StringRef(getStem)(StringRef)) {
  std::vector<std::unique_ptr<Item>> newContents;
  std::vector<std::unique_ptr<Item>> &worklist = group->contents;

  auto getSubStem = [getStem](StringRef name, StringRef groupName) {
    StringRef subName = name.slice(groupName.size(), StringRef::npos);
    return getStem(subName);
  };

  if (worklist.empty())
    return;

  auto start = worklist.begin();
  while (start != worklist.end()) {
    if (Group *g = dyn_cast<Group>(start->get())) {
      groupStemsRecursive(g, recurseIntoNewGroups, getStem);
      newContents.push_back(std::move(*start));
      ++start;
      continue;
    }

    StringRef stem = getSubStem((*start)->name, group->name);
    auto end = start;
    auto next = ++end;
    while (end != worklist.end() && !stem.empty() &&
           stem == getSubStem((*end)->name, group->name))
      ++end;
    if (end == next) {
      // Only one element; don't group.
      newContents.push_back(std::move(*start));
      start = end; // == next == ++start
    } else if (end == worklist.end() && newContents.empty()) {
      // Only one group; inline it.
      // FIXME: this is wrong, we should try to sub-group.
      assert(start == worklist.begin());
      std::swap(newContents, worklist);
      break;
    } else {
      std::string name = (Twine(group->name) + stem).str();
      if (recurseIntoNewGroups) {
        while (true) {
          StringRef next = getSubStem((*start)->name, name);
          if (next.empty())
            break;
          auto I = start; ++I;
          for ( ; I != end; ++I)
            if (next != getSubStem((*I)->name, name))
              goto done;
          name += next;
        }
      done:
        ; // exit label
      }

      auto newGroup = make_group(name);
      for (; start != end; ++start)
        newGroup->contents.push_back(std::move(*start));

      if (recurseIntoNewGroups)
        groupStemsRecursive(newGroup.get(), recurseIntoNewGroups, getStem);
      newContents.push_back(std::move(newGroup));
    }
  }

  group->contents = std::move(newContents);
}

//===----------------------------------------------------------------------===//
// CodeCompletionView
//===----------------------------------------------------------------------===//

static bool walkRecursive(CodeCompletionView::Walker &walker, const Item *item) {
  if (auto *result = dyn_cast<Result>(item))
    return walker.handleResult(result->value);

  walker.startGroup(item->name);
  for (auto &child : cast<Group>(item)->contents) {
    if (!walkRecursive(walker, child.get()))
      return false;
  }
  walker.endGroup();
  return true;
}

bool CodeCompletionView::walk(CodeCompletionView::Walker &walker) const {
  assert(rootGroup);
  return walkRecursive(walker, rootGroup);
}

CodeCompletionView::~CodeCompletionView() { delete rootGroup; }

unsigned LimitedResultView::getNextOffset() const { return start; }

bool LimitedResultView::walk(CodeCompletionView::Walker &walker) const {
  const Group *root = baseView.rootGroup;
  walker.startGroup(root->name);
  auto begin = root->contents.begin();
  auto end = root->contents.end();
  unsigned count = 0;
  while (count < start && begin != end) {
    ++count;
    ++begin;
  }
  while (begin != end && (maxResults == 0 || count < start + maxResults)) {
    if (!walkRecursive(walker, begin->get()))
      return false;
    ++count;
    ++begin;
  }

  if (begin == end) {
    assert(maxResults == 0 || count <= start + maxResults);
    start = 0;
  } else {
    start = count;
  }

  walker.endGroup();
  return true;
}

//===----------------------------------------------------------------------===//
// CompletionBuilder
//===----------------------------------------------------------------------===//

void CompletionBuilder::getFilterName(CodeCompletionString *str,
                                      raw_ostream &OS) {
  using ChunkKind = CodeCompletionString::Chunk::ChunkKind;

  // FIXME: we need a more uniform way to handle operator completions.
  if (str->getChunks().size() == 1 && str->getChunks()[0].is(ChunkKind::Dot)) {
    OS << ".";
    return;
  } else if (str->getChunks().size() == 2 &&
             str->getChunks()[0].is(ChunkKind::QuestionMark) &&
             str->getChunks()[1].is(ChunkKind::Dot)) {
    OS << "?.";
    return;
  }

  auto FirstTextChunk = str->getFirstTextChunkIndex();
  if (FirstTextChunk.hasValue()) {
    for (auto C : str->getChunks().slice(*FirstTextChunk)) {

      if (C.getKind() == ChunkKind::BraceStmtWithCursor)
        break;

      bool shouldPrint = !C.isAnnotation();
      switch (C.getKind()) {
      case ChunkKind::TypeAnnotation:
      case ChunkKind::CallParameterClosureType:
      case ChunkKind::CallParameterType:
      case ChunkKind::DeclAttrParamEqual:
      case ChunkKind::Comma:
      case ChunkKind::Whitespace:
        continue;
      case ChunkKind::CallParameterColon:
        // Since we don't add the type, also don't add the space after ':'.
        if (shouldPrint)
          OS << ":";
        continue;
      default:
        break;
      }

      if (C.hasText() && shouldPrint)
        OS << C.getText();
    }
  }
}

void CompletionBuilder::getDescription(SwiftResult *result, raw_ostream &OS,
                                       bool leadingPunctuation) {
  auto str = result->getCompletionString();
  bool isOperator = result->isOperator();

  auto FirstTextChunk = str->getFirstTextChunkIndex(leadingPunctuation);
  int TextSize = 0;
  if (FirstTextChunk.hasValue()) {
    for (auto C : str->getChunks().slice(*FirstTextChunk)) {
      using ChunkKind = CodeCompletionString::Chunk::ChunkKind;
      if (C.getKind() == ChunkKind::BraceStmtWithCursor)
        break;
      if (C.getKind() == ChunkKind::TypeAnnotation ||
          C.getKind() == ChunkKind::CallParameterClosureType ||
          C.getKind() == ChunkKind::Whitespace)
        continue;
      if (isOperator && C.getKind() == ChunkKind::CallParameterType)
        continue;
      if (C.hasText()) {
        TextSize += C.getText().size();
        OS << C.getText();
      }
    }
  }
  assert((TextSize > 0) &&
         "code completion result should have non-empty description!");
}

CompletionBuilder::CompletionBuilder(CompletionSink &sink, SwiftResult &base)
    : sink(sink), current(base) {
  semanticContext = current.getSemanticContext();
  completionString =
      const_cast<CodeCompletionString *>(current.getCompletionString());

  // FIXME: this works around the fact we're producing invalid completion
  // strings for our inner "." result.
  if (current.getCompletionString()->getFirstTextChunkIndex().hasValue()) {
    llvm::raw_svector_ostream OSS(originalName);
    getFilterName(current.getCompletionString(), OSS);
  }
}

void CompletionBuilder::setPrefix(CodeCompletionString *prefix) {
  if (!prefix)
    return;

  modified = true;

  // The underlying text is kept alive by an CompletionSink.  The chunks
  // themselves get copied into the CodeCompletionString.
  std::vector<CodeCompletionString::Chunk> chunks;
  chunks.reserve(prefix->getChunks().size());
  for (auto chunk : prefix->getChunks()) {
    if (chunk.is(CodeCompletionString::Chunk::ChunkKind::TypeAnnotation))
      continue; // The type is the type of the actual result, not the prefix.
    chunks.push_back(chunk);
  }

  auto existing = current.getCompletionString()->getChunks();
  chunks.insert(chunks.end(), existing.begin(), existing.end());
  completionString = CodeCompletionString::create(sink.allocator, chunks);
}

Completion *CompletionBuilder::finish() {
  SwiftResult base = current;
  llvm::SmallString<64> nameStorage;
  StringRef name = getOriginalName();
  if (modified) {
    // We've modified the original result, so build a new one.
    if (current.getKind() == SwiftResult::Declaration) {
      base = SwiftResult(semanticContext, current.getNumBytesToErase(),
                         completionString, current.getAssociatedDeclKind(),
                         current.getModuleName(), current.isNotRecommended(),
                         current.getBriefDocComment(),
                         current.getAssociatedUSRs(),
                         current.getDeclKeywords());
    } else {
      base = SwiftResult(current.getKind(), semanticContext,
                         current.getNumBytesToErase(), completionString);
    }

    llvm::raw_svector_ostream OSS(nameStorage);
    getFilterName(base.getCompletionString(), OSS);
    name = OSS.str();
  }

  llvm::SmallString<64> description;
  {
    llvm::raw_svector_ostream OSS(description);
    getDescription(&base, OSS, /*leadingPunctuation*/ true);
  }

  auto *result = new (sink.allocator)
      Completion(std::move(base), copyString(sink.allocator, name),
                 copyString(sink.allocator, description));
  result->moduleImportDepth = moduleImportDepth;
  result->popularityFactor = popularityFactor;
  result->opaqueCustomKind = customKind;
  return result;
}

//===----------------------------------------------------------------------===//
// NameStyle
//===----------------------------------------------------------------------===//

NameStyle::NameStyle(StringRef name)
    : leadingUnderscores(0), trailingUnderscores(0) {
  // Trim leading and trailing underscores.
  StringRef center = name.ltrim("_");
  if (center == "")
    return;
  leadingUnderscores = name.size() - center.size();
  center = center.rtrim("_");
  assert(!center.empty());
  trailingUnderscores = name.size() - center.size() - leadingUnderscores;

  unsigned pos = 0;

  enum Case {
    None = 0,
    Lower,
    Upper,
  };

  auto caseOf = [](char c) {
    if (clang::isLowercase(c))
      return Lower;
    if (clang::isUppercase(c))
      return Upper;
    return None;
  };

  unsigned underscores = 0;
  unsigned caseCount[3] = {0, 0, 0};
  Case leadingCase = None;
  Case prevCase = None;
  for (; pos < center.size(); ++pos) {
    char c = center[pos];
    Case curCase = caseOf(c);
    if (!leadingCase)
      leadingCase = curCase;

    underscores += (c == '_');
    caseCount[curCase] += 1;
    prevCase = curCase;
  }

  assert(caseCount[leadingCase] > 0);

  if (caseCount[Lower] && !caseCount[Upper]) {
    wordDelimiter = underscores ? LowercaseWithUnderscores : Lowercase;
    return;
  }
  if (caseCount[Upper] && !caseCount[Lower]) {
    wordDelimiter = underscores ? UppercaseWithUnderscores : Uppercase;
    return;
  }
  if (leadingCase && !underscores) {
    wordDelimiter = leadingCase == Lower ? LowerCamelCase : UpperCamelCase;
    return;
  }

  // FIXME: should we try to choose a delimiter if there is more than one?
  wordDelimiter = Unknown;
}
