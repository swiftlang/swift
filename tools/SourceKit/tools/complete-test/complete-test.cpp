//===--- complete-test.cpp ------------------------------------------------===//
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

#include "sourcekitd/sourcekitd.h"

#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/raw_ostream.h"
#include <fstream>
#include <optional>
#include <regex>
#if defined(__unix__) || (defined(__APPLE__) && defined(__MACH__))
#include <unistd.h>
#include <sys/param.h>
#elif defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <Windows.h>
#endif

// FIXME: Platform compatibility.
#include <dispatch/dispatch.h>

using namespace llvm;

#if defined(_WIN32)
namespace {
int STDOUT_FILENO = _fileno(stdout);
}
#endif

namespace {
struct TestOptions {
  StringRef sourceFile;
  StringRef completionToken;
  StringRef popularAPI;
  StringRef unpopularAPI;
  std::optional<bool> sortByName;
  std::optional<bool> useImportDepth;
  std::optional<bool> groupOverloads;
  std::optional<bool> groupStems;
  std::optional<bool> includeExactMatch;
  std::optional<bool> addInnerResults;
  std::optional<bool> addInnerOperators;
  std::optional<bool> addInitsToTopLevel;
  std::optional<unsigned> requestStart;
  std::optional<unsigned> requestLimit;
  std::optional<unsigned> hideUnderscores;
  std::optional<bool> hideByName;
  std::optional<bool> hideLowPriority;
  std::optional<unsigned> showTopNonLiteral;
  std::optional<bool> fuzzyMatching;
  std::optional<unsigned> fuzzyWeight;
  std::optional<unsigned> popularityBonus;
  std::optional<bool> verifyUSRToDecl;
  StringRef filterRulesJSON;
  std::string moduleCachePath;
  bool rawOutput = false;
  bool structureOutput = false;
  bool disableImplicitConcurrencyModuleImport = false;
  bool disableImplicitStringProcessingModuleImport = false;
  ArrayRef<const char *> compilerArgs;
};
} // end anonymous namespace
static int handleTestInvocation(TestOptions &options);

static sourcekitd_uid_t KeyRequest;
static sourcekitd_uid_t KeyCompilerArgs;
static sourcekitd_uid_t KeyOffset;
static sourcekitd_uid_t KeyLength;
static sourcekitd_uid_t KeySourceFile;
static sourcekitd_uid_t KeySourceText;
static sourcekitd_uid_t KeyName;
static sourcekitd_uid_t KeyNameOffset;
static sourcekitd_uid_t KeyNameLength;
static sourcekitd_uid_t KeyBodyOffset;
static sourcekitd_uid_t KeyBodyLength;
static sourcekitd_uid_t KeyThrowOffset;
static sourcekitd_uid_t KeyThrowLength;
static sourcekitd_uid_t KeyIsLocal;
static sourcekitd_uid_t KeyDescription;
static sourcekitd_uid_t KeyCodeCompleteOptions;
static sourcekitd_uid_t KeySortByName;
static sourcekitd_uid_t KeyUseImportDepth;
static sourcekitd_uid_t KeyGroupOverloads;
static sourcekitd_uid_t KeyGroupStems;
static sourcekitd_uid_t KeyFilterText;
static sourcekitd_uid_t KeyFilterRules;
static sourcekitd_uid_t KeyRequestStart;
static sourcekitd_uid_t KeyRequestLimit;
static sourcekitd_uid_t KeyHideUnderscores;
static sourcekitd_uid_t KeyHideLowPriority;
static sourcekitd_uid_t KeyHideByName;
static sourcekitd_uid_t KeyIncludeExactMatch;
static sourcekitd_uid_t KeyAddInnerResults;
static sourcekitd_uid_t KeyAddInnerOperators;
static sourcekitd_uid_t KeyAddInitsToTopLevel;
static sourcekitd_uid_t KeyFuzzyMatching;
static sourcekitd_uid_t KeyFuzzyWeight;
static sourcekitd_uid_t KeyPopularityBonus;
static sourcekitd_uid_t KeyTopNonLiteral;
static sourcekitd_uid_t KeyKind;
static sourcekitd_uid_t KeyResults;
static sourcekitd_uid_t KeyPopular;
static sourcekitd_uid_t KeyUnpopular;
static sourcekitd_uid_t KeySubStructure;
static sourcekitd_uid_t KeyVerifyUSRToDecl;

// Returns false and sets 'error' on failure.
static bool parseOptions(ArrayRef<const char *> args, TestOptions &options,
                         std::string &error) {
  for (unsigned i = 0; i < args.size(); ++i) {
    StringRef opt, value;
    std::tie(opt, value) = StringRef(args[i]).split('=');
    if (opt == "--") {
      options.compilerArgs = args.slice(i + 1);
      break;
    }
    if (opt == "-" || !opt.starts_with("-")) {
      options.sourceFile = args[i];
      continue;
    }

    if (opt.starts_with("--")) {
      error = std::string("unrecognized option '") + args[i] + "'";
      return false;
    }
    opt = opt.ltrim("-");

    if (opt == "tok") {
      options.completionToken = value;
    } else if (opt == "sort") {
      if (value == "context") {
        options.sortByName = false;
      } else if (value == "name") {
        options.sortByName = true;
      } else {
        error = "unrecognized argument for -sort=";
        return false;
      }
    } else if (opt == "add-inits-to-top-level") {
      options.addInitsToTopLevel = true;
    } else if (opt == "include-exact-match") {
      options.includeExactMatch = true;
    } else if (opt == "no-include-exact-match") {
      options.includeExactMatch = false;
    } else if (opt == "add-inner-results") {
      options.addInnerResults = true;
    } else if (opt == "no-inner-results") {
      options.addInnerResults = false;
    } else if (opt == "inner-operators") {
      options.addInnerOperators = true;
    } else if (opt == "no-inner-operators") {
      options.addInnerOperators = false;
    } else if (opt == "depth") {
      options.useImportDepth = true;
    } else if (opt == "no-depth") {
      options.useImportDepth = false;
    } else if (opt == "fuzz") {
      options.fuzzyMatching = true;
    } else if (opt == "no-fuzz") {
      options.fuzzyMatching = false;
    } else if (opt == "fuzzy-weight") {
      unsigned uval;
      if (value.getAsInteger(10, uval)) {
        error = "unrecognized integer value for -fuzzy-weight=";
        return false;
      }
      options.fuzzyWeight = uval;
    } else if (opt == "popularity-bonus") {
      unsigned uval;
      if (value.getAsInteger(10, uval)) {
        error = "unrecognized integer value for -popularity-bonus=";
        return false;
      }
      options.popularityBonus = uval;
    } else if (opt == "group") {
      if (value == "overloads") {
        options.groupOverloads = true;
        if (!options.groupStems)
          options.groupStems = false;
      } else if (value == "stems") {
        options.groupStems = true;
      } else if (value == "none") {
        options.groupStems = false;
        options.groupOverloads = false;
      } else {
        error = "unrecognized argument for -group=";
        return false;
      }
    } else if (opt == "start") {
      unsigned uval;
      if (value.getAsInteger(10, uval)) {
        error = "unrecognized integer value for -start=";
        return false;
      }
      options.requestStart = uval;
    } else if (opt == "limit") {
      unsigned uval;
      if (value.getAsInteger(10, uval)) {
        error = "unrecognized integer value for -limit=";
        return false;
      }
      options.requestLimit = uval;
    } else if (opt == "raw") {
      options.rawOutput = true;
    } else if (opt == "structure") {
      options.structureOutput = true;
    } else if (opt == "hide-underscores") {
      unsigned uval;
      if (value.getAsInteger(10, uval)) {
        error = "unrecognized integer value for -hide-underscores=";
        return false;
      }
      options.hideUnderscores = uval;
    } else if (opt == "hide-low-priority") {
      unsigned uval;
      if (value.getAsInteger(10, uval)) {
        error = "unrecognized integer value for -hide-low-priority=";
        return false;
      }
      options.hideLowPriority = uval;
    } else if (opt == "hide-by-name") {
      unsigned uval;
      if (value.getAsInteger(10, uval)) {
        error = "unrecognized integer value for -hide-by-name=";
        return false;
      }
      options.hideByName = uval;
    } else if (opt == "hide-none") {
      options.hideUnderscores = 0;
      options.hideLowPriority = false;
      options.hideByName = false;
    } else if (opt == "popular") {
      options.popularAPI = value;
    } else if (opt == "unpopular") {
      options.unpopularAPI = value;
    } else if (opt == "filter-rules") {
      options.filterRulesJSON = value;
    } else if (opt == "top") {
      unsigned uval;
      if (value.getAsInteger(10, uval)) {
        error = "unrecognized integer value for -tope=";
        return false;
      }
      options.showTopNonLiteral = uval;
    } else if (opt == "module-cache-path") {
      options.moduleCachePath = value.str();
    } else if (opt == "disable-implicit-concurrency-module-import") {
      options.disableImplicitConcurrencyModuleImport = true;
    } else if (opt == "disable-implicit-string-processing-module-import") {
      options.disableImplicitStringProcessingModuleImport = true;
    } else if (opt == "verify-usr-to-decl") {
      options.verifyUSRToDecl = true;
    } else if (opt == "no-verify-usr-to-decl") {
      options.verifyUSRToDecl = false;
    }
  }

  if (options.sourceFile.empty()) {
    error = "missing <source-file>";
    return false;
  }
  if (options.completionToken.empty()) {
    error = "missing -tok=<completion-token>";
    return false;
  }

  return true;
}

static int skt_main(int argc, const char **argv);

int main(int argc, const char **argv) {
  dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_HIGH, 0), ^{
    int ret = skt_main(argc, argv);
    exit(ret);
  });

  dispatch_main();
}

static void notification_receiver(sourcekitd_response_t resp) {
  if (sourcekitd_response_is_error(resp)) {
    sourcekitd_response_description_dump(resp);
    exit(1);
  }
}

static int skt_main(int argc, const char **argv) {
  llvm::sys::PrintStackTraceOnErrorSignal(argv[0]);

  sourcekitd_initialize();

  sourcekitd_set_notification_handler(^(sourcekitd_response_t resp) {
    notification_receiver(resp);
  });

  KeyRequest = sourcekitd_uid_get_from_cstr("key.request");
  KeyCompilerArgs = sourcekitd_uid_get_from_cstr("key.compilerargs");
  KeyOffset = sourcekitd_uid_get_from_cstr("key.offset");
  KeyLength = sourcekitd_uid_get_from_cstr("key.length");
  KeyKind = sourcekitd_uid_get_from_cstr("key.kind");
  KeyCodeCompleteOptions =
      sourcekitd_uid_get_from_cstr("key.codecomplete.options");
  KeySortByName = sourcekitd_uid_get_from_cstr("key.codecomplete.sort.byname");
  KeyUseImportDepth =
      sourcekitd_uid_get_from_cstr("key.codecomplete.sort.useimportdepth");
  KeyGroupOverloads =
      sourcekitd_uid_get_from_cstr("key.codecomplete.group.overloads");
  KeyGroupStems = sourcekitd_uid_get_from_cstr("key.codecomplete.group.stems");
  KeyFilterText = sourcekitd_uid_get_from_cstr("key.codecomplete.filtertext");
  KeyFilterRules = sourcekitd_uid_get_from_cstr("key.codecomplete.filterrules");
  KeyRequestLimit =
      sourcekitd_uid_get_from_cstr("key.codecomplete.requestlimit");
  KeyRequestStart =
      sourcekitd_uid_get_from_cstr("key.codecomplete.requeststart");
  KeyHideUnderscores =
      sourcekitd_uid_get_from_cstr("key.codecomplete.hideunderscores");
  KeyHideLowPriority =
      sourcekitd_uid_get_from_cstr("key.codecomplete.hidelowpriority");
  KeyHideByName = sourcekitd_uid_get_from_cstr("key.codecomplete.hidebyname");
  KeyIncludeExactMatch =
      sourcekitd_uid_get_from_cstr("key.codecomplete.includeexactmatch");
  KeyAddInnerResults =
      sourcekitd_uid_get_from_cstr("key.codecomplete.addinnerresults");
  KeyAddInnerOperators =
      sourcekitd_uid_get_from_cstr("key.codecomplete.addinneroperators");
  KeyAddInitsToTopLevel =
      sourcekitd_uid_get_from_cstr("key.codecomplete.addinitstotoplevel");
  KeyFuzzyMatching =
      sourcekitd_uid_get_from_cstr("key.codecomplete.fuzzymatching");
  KeyFuzzyWeight =
      sourcekitd_uid_get_from_cstr("key.codecomplete.sort.fuzzyweight");
  KeyPopularityBonus =
      sourcekitd_uid_get_from_cstr("key.codecomplete.sort.popularitybonus");
  KeyTopNonLiteral =
      sourcekitd_uid_get_from_cstr("key.codecomplete.showtopnonliteralresults");
  KeyVerifyUSRToDecl =
      sourcekitd_uid_get_from_cstr("key.codecomplete.verifyusrtodecl");
  KeySourceFile = sourcekitd_uid_get_from_cstr("key.sourcefile");
  KeySourceText = sourcekitd_uid_get_from_cstr("key.sourcetext");
  KeyName = sourcekitd_uid_get_from_cstr("key.name");
  KeyNameOffset = sourcekitd_uid_get_from_cstr("key.nameoffset");
  KeyNameLength = sourcekitd_uid_get_from_cstr("key.namelength");
  KeyBodyOffset = sourcekitd_uid_get_from_cstr("key.bodyoffset");
  KeyBodyLength = sourcekitd_uid_get_from_cstr("key.bodylength");
  KeyThrowOffset = sourcekitd_uid_get_from_cstr("key.throwoffset");
  KeyThrowLength = sourcekitd_uid_get_from_cstr("key.throwlength");
  KeyIsLocal = sourcekitd_uid_get_from_cstr("key.is_local");
  KeyDescription = sourcekitd_uid_get_from_cstr("key.description");
  KeyResults = sourcekitd_uid_get_from_cstr("key.results");
  KeyPopular = sourcekitd_uid_get_from_cstr("key.popular");
  KeyUnpopular = sourcekitd_uid_get_from_cstr("key.unpopular");
  KeySubStructure = sourcekitd_uid_get_from_cstr("key.substructure");

  auto Args = llvm::ArrayRef(argv + 1, argc - 1);
  TestOptions options;
  std::string error;
  if (!parseOptions(Args, options, error)) {
    llvm::errs() << "usage: complete-test -tok=A file\n" << error << "\n";
    return 1;
  }

  int ret = handleTestInvocation(options);
  sourcekitd_shutdown();
  return ret;
}

static std::string
removeCodeCompletionTokens(StringRef Input, StringRef TokenName,
                           SmallVectorImpl<std::string> &prefixes,
                           unsigned *CompletionOffset) {
  assert(TokenName.size() >= 1);
  *CompletionOffset = ~0U;

  std::string CleanFile;
  CleanFile.reserve(Input.size());

  std::regex tokenRegex(R"(#\^([^^,]+)(,[^^]*)?\^#)");
  auto pos = Input.begin();
  while (pos != Input.end()) {
    std::match_results<StringRef::iterator> match;
    std::regex_search(pos, Input.end(), match, tokenRegex);
    if (match.empty()) {
      CleanFile.append(pos, Input.end()); // remaining text
      pos = Input.end();
      break;
    }

    // Update the buffer.
    CleanFile.append(pos, match.prefix().second);
    pos = match.suffix().first;

    // Check the token.
    assert(match.size() == 2 || match.size() == 3);
    if (match[1].str() != TokenName)
      continue;
    *CompletionOffset = CleanFile.size();
    if (match.size() == 2 || !match[2].matched)
      continue;

    std::string fullMatch = match[2].str();
    assert(fullMatch[0] == ',');
    StringRef next = StringRef(fullMatch).split(',').second;
    while (next != "") {
      auto split = next.split(',');
      prefixes.push_back(split.first.str());
      next = split.second;
    }
  }

  return CleanFile;
}

namespace {
class ResponsePrinter {
  llvm::raw_ostream &OS;
  unsigned indentWidth;
  unsigned currentIndentation;
  bool structuredOutput = false;

public:
  ResponsePrinter(llvm::raw_ostream &OS, unsigned indentWidth,
                  unsigned startingIndent, bool structure)
      : OS(OS), indentWidth(indentWidth), currentIndentation(startingIndent),
        structuredOutput(structure) {}
  void printResponse(sourcekitd_response_t resp) {
    auto dict = sourcekitd_response_get_value(resp);
    printGroup(dict);
  }
  void printArray(sourcekitd_variant_t array) {
    sourcekitd_variant_array_apply(
        array, ^bool(size_t index, sourcekitd_variant_t value) {
          printGroupOrCompletion(value);
          return true;
        });
  }
  void printGroupOrCompletion(sourcekitd_variant_t value) {
    static sourcekitd_uid_t GroupUID =
        sourcekitd_uid_get_from_cstr("source.lang.swift.codecomplete.group");
    if (GroupUID == sourcekitd_variant_dictionary_get_uid(value, KeyKind)) {
      printGroup(value);
    } else {
      printCompletion(value);
    }
  }
  void printCompletion(sourcekitd_variant_t completion) {
    assert(sourcekitd_variant_get_type(completion) ==
           SOURCEKITD_VARIANT_TYPE_DICTIONARY);
    // FIXME: kind, semantic context?
    StringRef desc = dictGetString(completion, KeyDescription);
    if (!structuredOutput) {
      indent() << desc << "\n";
      return;
    }

    auto structure =
        sourcekitd_variant_dictionary_get_value(completion, KeySubStructure);
    if (sourcekitd_variant_get_type(structure) ==
        SOURCEKITD_VARIANT_TYPE_NULL) {
      indent() << "(no structure)\n";
      return;
    }

    unsigned index = 0;
    auto printUntil =
        [desc, &index, this](unsigned end) -> llvm::raw_ostream & {
          for (; index != end; ++index)
            OS.write(desc[index]);
          return OS;
        };

    auto getUInt = [](sourcekitd_variant_t dict, sourcekitd_uid_t key) {
      auto value = sourcekitd_variant_dictionary_get_int64(dict, key);
      assert(0 <= value && value <= UINT_MAX);
      return value;
    };

    auto baseStart = getUInt(structure, KeyNameOffset);
    auto baseLen = getUInt(structure, KeyNameLength);
    // {name:basename}([params:{p:{t:Int}}, {p:label:{t: String}}])
    indent();
    if (baseLen) {
      printUntil(baseStart) << "{name:";
      printUntil(baseStart + baseLen) << "}";
    }

    auto paramStart = getUInt(structure, KeyBodyOffset);
    auto paramLen = getUInt(structure, KeyBodyLength);
    auto params =
        sourcekitd_variant_dictionary_get_value(structure, KeySubStructure);
    if (sourcekitd_variant_get_type(params) != SOURCEKITD_VARIANT_TYPE_NULL) {
      assert(paramStart >= baseStart + baseLen);
      printUntil(paramStart) << "{params:";

      sourcekitd_variant_array_apply(
          params, ^bool(size_t index, sourcekitd_variant_t param) {
        auto start = getUInt(param, KeyNameOffset);
        auto len = getUInt(param, KeyNameLength);
        auto tStart = getUInt(param, KeyBodyOffset);
        auto tLen = getUInt(param, KeyBodyLength);
        bool isLocalName = getUInt(param, KeyIsLocal);

        if (len) {
          assert(start >= paramStart);
          assert(start + len <= paramStart + paramLen);
          printUntil(start) << "{" << (isLocalName ? "l" : "n") << ":";
          printUntil(start + len) << "}";
        }
        if (tLen != 0) {
          assert(tStart >= start);
          assert(tStart + tLen <= paramStart + paramLen);
          printUntil(tStart) << "{t:";
          printUntil(tStart + tLen) << "}";
        }
        return true;
      });

      printUntil(paramStart + paramLen) << "}";
    } else if (paramLen != 0) {
      printUntil(paramStart) << "{params:";
      printUntil(paramStart + paramLen) << "}";
    }

    auto throwStart = getUInt(structure, KeyThrowOffset);
    auto throwLength = getUInt(structure, KeyThrowLength);
    if (throwLength != 0) {
      printUntil(throwStart) << "{throws:";
      printUntil(throwStart + throwLength) << "}";
    }

    printUntil(desc.size()) << "\n";
  }
  void printGroup(sourcekitd_variant_t dict) {
    struct RestoreInt {
      unsigned old;
      unsigned &value;
      RestoreInt(unsigned &value) : old(value), value(value) {}
      ~RestoreInt() { value = old; }
    } restoreIndent(currentIndentation);

    StringRef name = dictGetString(dict, KeyName);
    if (!name.empty()) {
      indent() << name << ":\n";
      currentIndentation += indentWidth;
    }

    auto results = sourcekitd_variant_dictionary_get_value(dict, KeyResults);
    printArray(results);
  }
  StringRef dictGetString(sourcekitd_variant_t dict, sourcekitd_uid_t key) {
    auto val = sourcekitd_variant_dictionary_get_value(dict, key);
    assert(sourcekitd_variant_get_type(val) == SOURCEKITD_VARIANT_TYPE_STRING);
    StringRef str(sourcekitd_variant_string_get_ptr(val),
                  sourcekitd_variant_string_get_length(val));
    return str;
  }
  llvm::raw_ostream &indent() {
    for (unsigned i = 0; i < currentIndentation; ++i)
      OS.write(' ');
    return OS;
  }
};
} // end anonymous namespace

static void printResponse(sourcekitd_response_t resp, bool raw, bool structure,
                          unsigned indentation) {
  if (raw) {
    llvm::outs().flush();
    sourcekitd_response_description_dump_filedesc(resp, STDOUT_FILENO);
    return;
  }
  ResponsePrinter p(llvm::outs(), 4, indentation, structure);
  p.printResponse(resp);
  llvm::outs().flush();
}

static std::unique_ptr<llvm::MemoryBuffer>
getBufferForFilename(StringRef name) {

  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> buffer =
      llvm::MemoryBuffer::getFile(name);

  if (!buffer) {
    llvm::errs() << "error reading '" << name
                 << "': " << buffer.getError().message() << "\n";
    return nullptr;
  }
  return std::move(buffer.get());
}

static sourcekitd_object_t createBaseRequest(sourcekitd_uid_t requestUID,
                                             const char *name,
                                             unsigned offset) {
  sourcekitd_object_t request =
      sourcekitd_request_dictionary_create(nullptr, nullptr, 0);
  sourcekitd_request_dictionary_set_uid(request, KeyRequest, requestUID);
  sourcekitd_request_dictionary_set_int64(request, KeyOffset, offset);
  sourcekitd_request_dictionary_set_string(request, KeyName, name);
  return request;
}

using HandlerFunc = std::function<bool(sourcekitd_response_t)>;

static bool sendRequestSync(sourcekitd_object_t request, HandlerFunc func) {
  auto response = sourcekitd_send_request_sync(request);
  bool result = func(response);
  sourcekitd_response_dispose(response);
  return result;
}

static bool codeCompleteRequest(sourcekitd_uid_t requestUID, const char *name,
                                unsigned offset, const char *sourceText,
                                const char *filterText, TestOptions &options,
                                HandlerFunc func) {

  auto request = createBaseRequest(requestUID, name, offset);
  sourcekitd_request_dictionary_set_string(request, KeySourceFile, name);
  sourcekitd_request_dictionary_set_string(request, KeySourceText, sourceText);

  auto opts = sourcekitd_request_dictionary_create(nullptr, nullptr, 0);
  {
    auto addBoolOption = [&](sourcekitd_uid_t key, std::optional<bool> option) {
      if (option)
        sourcekitd_request_dictionary_set_int64(opts, key, *option);
    };
    addBoolOption(KeySortByName, options.sortByName);
    addBoolOption(KeyUseImportDepth, options.useImportDepth);
    addBoolOption(KeyGroupOverloads, options.groupOverloads);
    addBoolOption(KeyGroupStems, options.groupStems);
    addBoolOption(KeyIncludeExactMatch, options.includeExactMatch);
    addBoolOption(KeyAddInnerResults, options.addInnerResults);
    addBoolOption(KeyAddInnerOperators, options.addInnerOperators);
    addBoolOption(KeyAddInitsToTopLevel, options.addInitsToTopLevel);
    addBoolOption(KeyFuzzyMatching, options.fuzzyMatching);
    addBoolOption(KeyHideLowPriority, options.hideLowPriority);
    addBoolOption(KeyHideByName, options.hideByName);
    addBoolOption(KeyVerifyUSRToDecl, options.verifyUSRToDecl);

    auto addIntOption = [&](sourcekitd_uid_t key,
                            std::optional<unsigned> option) {
      if (option)
        sourcekitd_request_dictionary_set_int64(opts, key, *option);
    };
    addIntOption(KeyRequestStart, options.requestStart);
    addIntOption(KeyRequestLimit, options.requestLimit);
    addIntOption(KeyHideUnderscores, options.hideUnderscores);
    addIntOption(KeyFuzzyWeight, options.fuzzyWeight);
    addIntOption(KeyPopularityBonus, options.popularityBonus);
    addIntOption(KeyTopNonLiteral, options.showTopNonLiteral);

    if (filterText)
      sourcekitd_request_dictionary_set_string(opts, KeyFilterText, filterText);

    if (!options.filterRulesJSON.empty()) {
      auto buffer = getBufferForFilename(options.filterRulesJSON);
      if (!buffer)
        return 1;

      char *err = nullptr;
      auto dict =
          sourcekitd_request_create_from_yaml(buffer->getBuffer().data(), &err);
      if (!dict) {
        assert(err);
        llvm::errs() << err;
        free(err);
        return 1;
      }

      sourcekitd_request_dictionary_set_value(opts, KeyFilterRules, dict);
    }
  }
  sourcekitd_request_dictionary_set_value(request, KeyCodeCompleteOptions,opts);
  sourcekitd_request_release(opts);

  auto args = sourcekitd_request_array_create(nullptr, 0);
  {
    sourcekitd_request_array_set_string(args, SOURCEKITD_ARRAY_APPEND, name);
    if (const char *sdk = getenv("SDKROOT")) {
      sourcekitd_request_array_set_string(args, SOURCEKITD_ARRAY_APPEND,"-sdk");
      sourcekitd_request_array_set_string(args, SOURCEKITD_ARRAY_APPEND, sdk);
    }
    if (!options.moduleCachePath.empty()) {
      sourcekitd_request_array_set_string(args, SOURCEKITD_ARRAY_APPEND, "-module-cache-path");
      sourcekitd_request_array_set_string(args, SOURCEKITD_ARRAY_APPEND, options.moduleCachePath.c_str());
    }
    // Add -- options.
    for (const char *arg : options.compilerArgs)
      sourcekitd_request_array_set_string(args, SOURCEKITD_ARRAY_APPEND, arg);
    if (options.disableImplicitConcurrencyModuleImport) {
      sourcekitd_request_array_set_string(args, SOURCEKITD_ARRAY_APPEND,
          "-Xfrontend");
      sourcekitd_request_array_set_string(args, SOURCEKITD_ARRAY_APPEND,
          "-disable-implicit-concurrency-module-import");
    }
    if (options.disableImplicitStringProcessingModuleImport) {
      sourcekitd_request_array_set_string(args, SOURCEKITD_ARRAY_APPEND,
          "-Xfrontend");
      sourcekitd_request_array_set_string(args, SOURCEKITD_ARRAY_APPEND,
          "-disable-implicit-string-processing-module-import");
    }
  }
  sourcekitd_request_dictionary_set_value(request, KeyCompilerArgs, args);
  sourcekitd_request_release(args);

  // Send the request!
  bool result = sendRequestSync(request, func);
  sourcekitd_request_release(request);
  return result;
}

static bool readPopularAPIList(StringRef filename,
                               std::vector<std::string> &result) {
  std::ifstream in(filename.str());
  if (!in.is_open()) {
    llvm::errs() << "error opening '" << filename << "'\n";
    return true;
  }

  std::string line;
  while (std::getline(in, line)) {
    result.emplace_back();
    std::swap(result.back(), line);
  }
  return false;
}

static bool setupPopularAPI(const TestOptions &options) {
  if (options.popularAPI.empty() && options.unpopularAPI.empty())
    return false;

  sourcekitd_uid_t RequestCodeCompleteSetPopularAPI =
      sourcekitd_uid_get_from_cstr("source.request.codecomplete.setpopularapi");

  auto req = sourcekitd_request_dictionary_create(nullptr, nullptr, 0);
  sourcekitd_request_dictionary_set_uid(req, KeyRequest,
                                        RequestCodeCompleteSetPopularAPI);

  auto addPopularList = [&req](StringRef filename, sourcekitd_uid_t key) {
    std::vector<std::string> names;
    if (readPopularAPIList(filename, names))
      return true;

    sourcekitd_object_t popular = sourcekitd_request_array_create(nullptr, 0);
    for (auto name : names)
      sourcekitd_request_array_set_string(popular, SOURCEKITD_ARRAY_APPEND,
                                          name.c_str());
    sourcekitd_request_dictionary_set_value(req, key, popular);
    return false;
  };

  if (!options.popularAPI.empty() &&
      addPopularList(options.popularAPI, KeyPopular))
    return true;

  if (!options.unpopularAPI.empty() &&
      addPopularList(options.unpopularAPI, KeyUnpopular))
    return true;

  auto resp = sourcekitd_send_request_sync(req);
  bool fail = false;
  if (sourcekitd_response_is_error(resp)) {
    fail = true;
    sourcekitd_response_description_dump(resp);
  }
  sourcekitd_response_dispose(resp);
  sourcekitd_request_release(req);
  return fail;
}

static int handleTestInvocation(TestOptions &options) {

  StringRef SourceFilename = options.sourceFile;
  auto SourceBuf = getBufferForFilename(SourceFilename);
  if (!SourceBuf)
    return 1;

  unsigned CodeCompletionOffset;
  SmallVector<std::string, 4> prefixes;
  std::string CleanFile = removeCodeCompletionTokens(
      SourceBuf->getBuffer(), options.completionToken, prefixes,
      &CodeCompletionOffset);

  if (CodeCompletionOffset == ~0U) {
    llvm::errs() << "cannot find code completion token in source file\n";
    return 1;
  }

  sourcekitd_uid_t RequestCodeCompleteOpen =
      sourcekitd_uid_get_from_cstr("source.request.codecomplete.open");
  sourcekitd_uid_t RequestCodeCompleteClose =
      sourcekitd_uid_get_from_cstr("source.request.codecomplete.close");
  sourcekitd_uid_t RequestCodeCompleteUpdate =
      sourcekitd_uid_get_from_cstr("source.request.codecomplete.update");

  if (setupPopularAPI(options))
    return 1;

  // Open the connection and get the first set of results.
  bool isError = codeCompleteRequest(
      RequestCodeCompleteOpen, SourceFilename.data(), CodeCompletionOffset,
      CleanFile.c_str(), /*filterText*/ nullptr, options,
      [&](sourcekitd_object_t response) -> bool {
        if (sourcekitd_response_is_error(response)) {
          sourcekitd_response_description_dump(response);
          return true;
        }

        // If there are no prefixes, just dump all the results.
        if (prefixes.empty())
          printResponse(response, options.rawOutput, options.structureOutput,
                        /*indentation*/ 0);
        return false;
      });

  if (isError)
    return isError;

  for (auto &prefix : prefixes) {
    isError |= codeCompleteRequest(
        RequestCodeCompleteUpdate, SourceFilename.data(), CodeCompletionOffset,
        CleanFile.c_str(), prefix.c_str(), options,
        [&](sourcekitd_object_t response) -> bool {
          if (sourcekitd_response_is_error(response)) {
            sourcekitd_response_description_dump(response);
            return true;
          }
          llvm::outs() << "Results for filterText: " << prefix << " [\n";
          llvm::outs().flush();
          printResponse(response, options.rawOutput, options.structureOutput,
                        /*indentation*/ 4);
          llvm::outs() << "]\n";
          llvm::outs().flush();
          return false;
        });
    if (isError)
      break;
  }

  // Close the code completion connection.
  auto request = createBaseRequest(RequestCodeCompleteClose,
                                   SourceFilename.data(), CodeCompletionOffset);

  isError |= sendRequestSync(request, [&](sourcekitd_object_t response) {
    if (sourcekitd_response_is_error(response)) {
      sourcekitd_response_description_dump(response);
      return true;
    }
    return false;
  });

  sourcekitd_request_release(request);
  return isError;
}
