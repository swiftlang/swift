//===--- sourcekitdAPI-Common.cpp -----------------------------------------===//
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

#include "DictionaryKeys.h"
#include "sourcekitd/Internal.h"
#include "sourcekitd/Logging.h"
#include "sourcekitd/RequestResponsePrinterBase.h"
#include "SourceKit/Support/Logging.h"
#include "SourceKit/Support/UIdent.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Mutex.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/Threading.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/YAMLParser.h"
#include <mutex>

using namespace SourceKit;
using namespace sourcekitd;
using llvm::ArrayRef;
using llvm::StringRef;
using llvm::raw_ostream;


UIdent sourcekitd::KeyVersionMajor("key.version_major");;
UIdent sourcekitd::KeyVersionMinor("key.version_minor");;
UIdent sourcekitd::KeyResults("key.results");
UIdent sourcekitd::KeyRequest("key.request");
UIdent sourcekitd::KeyCompilerArgs("key.compilerargs");
UIdent sourcekitd::KeyOffset("key.offset");
UIdent sourcekitd::KeySourceFile("key.sourcefile");
UIdent sourcekitd::KeySourceText("key.sourcetext");
UIdent sourcekitd::KeyModuleName("key.modulename");
UIdent sourcekitd::KeyGroupName("key.groupname");
UIdent sourcekitd::KeyActionName("key.actionname");
UIdent sourcekitd::KeySynthesizedExtension("key.synthesizedextensions");
UIdent sourcekitd::KeyNotification("key.notification");
UIdent sourcekitd::KeyKeyword("key.keyword");
UIdent sourcekitd::KeyName("key.name");
UIdent sourcekitd::KeyNames("key.names");
UIdent sourcekitd::KeyUIDs("key.uids");
UIdent sourcekitd::KeyEnableSyntaxMap("key.enablesyntaxmap");
UIdent sourcekitd::KeyEnableDiagnostics("key.enablediagnostics");
UIdent sourcekitd::KeySyntacticOnly("key.syntactic_only");
UIdent sourcekitd::KeyLength("key.length");
UIdent sourcekitd::KeyActionable("key.actionable");
UIdent sourcekitd::KeyParentLoc("key.parent_loc");
UIdent sourcekitd::KeyKind("key.kind");
UIdent sourcekitd::KeyAccessibility("key.accessibility");
UIdent sourcekitd::KeySetterAccessibility("key.setter_accessibility");
UIdent sourcekitd::KeyUSR("key.usr");
UIdent sourcekitd::KeyOriginalUSR("key.original_usr");
UIdent sourcekitd::KeyDefaultImplementationOf("key.default_implementation_of");
UIdent sourcekitd::KeyInterestedUSR("key.interested_usr");
UIdent sourcekitd::KeyLine("key.line");
UIdent sourcekitd::KeyColumn("key.column");
UIdent sourcekitd::KeyReceiverUSR("key.receiver_usr");
UIdent sourcekitd::KeyIsDynamic("key.is_dynamic");
UIdent sourcekitd::KeyIsTestCandidate("key.is_test_candidate");
UIdent sourcekitd::KeyDescription("key.description");
UIdent sourcekitd::KeyTypeName("key.typename");
UIdent sourcekitd::KeyRuntimeName("key.runtime_name");
UIdent sourcekitd::KeySelectorName("key.selector_name");
UIdent sourcekitd::KeyOverrides("key.overrides");
UIdent sourcekitd::KeyDocBrief("key.doc.brief");
UIdent sourcekitd::KeyAssociatedUSRs("key.associated_usrs");
UIdent sourcekitd::KeyDocFullAsXML("key.doc.full_as_xml");
UIdent sourcekitd::KeyGenericParams("key.generic_params");
UIdent sourcekitd::KeyGenericRequirements("key.generic_requirements");
UIdent sourcekitd::KeyAnnotatedDecl("key.annotated_decl");
UIdent sourcekitd::KeyFullyAnnotatedDecl("key.fully_annotated_decl");
UIdent sourcekitd::KeyRelatedDecls("key.related_decls");
UIdent sourcekitd::KeyContext("key.context");
UIdent sourcekitd::KeyModuleImportDepth("key.moduleimportdepth");
UIdent sourcekitd::KeyNumBytesToErase("key.num_bytes_to_erase");
UIdent sourcekitd::KeyNotRecommended("key.not_recommended");
UIdent sourcekitd::KeyFilePath("key.filepath");
UIdent sourcekitd::KeyModuleInterfaceName("key.module_interface_name");
UIdent sourcekitd::KeyHash("key.hash");
UIdent sourcekitd::KeyRelated("key.related");
UIdent sourcekitd::KeyInherits("key.inherits");
UIdent sourcekitd::KeyConforms("key.conforms");
UIdent sourcekitd::KeyExtends("key.extends");
UIdent sourcekitd::KeyDependencies("key.dependencies");
UIdent sourcekitd::KeyEntities("key.entities");
UIdent sourcekitd::KeyDiagnostics("key.diagnostics");
UIdent sourcekitd::KeySeverity("key.severity");
UIdent sourcekitd::KeyRanges("key.ranges");
UIdent sourcekitd::KeyFixits("key.fixits");
UIdent sourcekitd::KeyAnnotations("key.annotations");
UIdent sourcekitd::KeyDiagnosticStage("key.diagnostic_stage");
UIdent sourcekitd::KeySyntaxMap("key.syntaxmap");
UIdent sourcekitd::KeyIsSystem("key.is_system");
UIdent sourcekitd::KeyEnableStructure("key.enablesubstructure");
UIdent sourcekitd::KeySubStructure("key.substructure");
UIdent sourcekitd::KeyElements("key.elements");
UIdent sourcekitd::KeyNameOffset("key.nameoffset");
UIdent sourcekitd::KeyNameLength("key.namelength");
UIdent sourcekitd::KeyBodyOffset("key.bodyoffset");
UIdent sourcekitd::KeyBodyLength("key.bodylength");
UIdent sourcekitd::KeyThrowOffset("key.throwoffset");
UIdent sourcekitd::KeyThrowLength("key.throwlength");
UIdent sourcekitd::KeyIsLocal("key.is_local");
UIdent sourcekitd::KeyAttributes("key.attributes");
UIdent sourcekitd::KeyAttribute("key.attribute");
UIdent sourcekitd::KeyInheritedTypes("key.inheritedtypes");
UIdent sourcekitd::KeyFormatOptions("key.editor.format.options");
UIdent sourcekitd::KeyCodeCompleteOptions("key.codecomplete.options");
UIdent sourcekitd::KeyFilterRules("key.codecomplete.filterrules");
UIdent sourcekitd::KeyNextRequestStart("key.nextrequeststart");
UIdent sourcekitd::KeyPopular("key.popular");
UIdent sourcekitd::KeyUnpopular("key.unpopular");
UIdent sourcekitd::KeyHide("key.hide");
UIdent sourcekitd::KeySimplified("key.simplified");
UIdent sourcekitd::KeyRangeContent("key.rangecontent");

UIdent sourcekitd::KeyIsDeprecated("key.is_deprecated");
UIdent sourcekitd::KeyIsUnavailable("key.is_unavailable");
UIdent sourcekitd::KeyIsOptional("key.is_optional");
UIdent sourcekitd::KeyPlatform("key.platform");
UIdent sourcekitd::KeyMessage("key.message");
UIdent sourcekitd::KeyIntroduced("key.introduced");
UIdent sourcekitd::KeyDeprecated("key.deprecated");
UIdent sourcekitd::KeyObsoleted("key.obsoleted");
UIdent sourcekitd::KeyRemoveCache("key.removecache");
UIdent sourcekitd::KeyTypeInterface("key.typeinterface");
UIdent sourcekitd::KeyTypeUsr("key.typeusr");
UIdent sourcekitd::KeyContainerTypeUsr("key.containertypeusr");
UIdent sourcekitd::KeyModuleGroups("key.modulegroups");

UIdent sourcekitd::KeyBaseName("key.basename");
UIdent sourcekitd::KeyArgNames("key.argnames");
UIdent sourcekitd::KeySelectorPieces("key.selectorpieces");
UIdent sourcekitd::KeyNameKind("key.namekind");
UIdent sourcekitd::KeyLocalizationKey("key.localization_key");

/// \brief Order for the keys to use when emitting the debug description of
/// dictionaries.
static UIdent *OrderedKeys[] = {
  &KeyVersionMajor,
  &KeyVersionMinor,
  &KeyResults,
  &KeyRequest,
  &KeyNotification,
  &KeyKind,
  &KeyAccessibility,
  &KeySetterAccessibility,
  &KeyKeyword,
  &KeyName,
  &KeyUSR,
  &KeyOriginalUSR,
  &KeyDefaultImplementationOf,
  &KeyInterestedUSR,
  &KeyGenericParams,
  &KeyGenericRequirements,
  &KeyDocFullAsXML,
  &KeyLine,
  &KeyColumn,
  &KeyReceiverUSR,
  &KeyIsDynamic,
  &KeyFilePath,
  &KeyModuleInterfaceName,
  &KeyHash,
  &KeyCompilerArgs,
  &KeySeverity,
  &KeyOffset,
  &KeyLength,
  &KeySourceFile,
  &KeySourceText,
  &KeyEnableSyntaxMap,
  &KeyEnableStructure,
  &KeyDescription,
  &KeyTypeName,
  &KeyRuntimeName,
  &KeySelectorName,
  &KeyAnnotatedDecl,
  &KeyFullyAnnotatedDecl,
  &KeyDocBrief,
  &KeyContext,
  &KeyModuleImportDepth,
  &KeyNumBytesToErase,
  &KeyNotRecommended,
  &KeyAnnotations,
  &KeyDiagnosticStage,
  &KeySyntaxMap,
  &KeyIsSystem,
  &KeyRelated,
  &KeyInherits,
  &KeyConforms,
  &KeyExtends,
  &KeyDependencies,
  &KeyEntities,
  &KeyNameOffset,
  &KeyNameLength,
  &KeyBodyOffset,
  &KeyBodyLength,
  &KeyThrowOffset,
  &KeyThrowLength,
  &KeyIsLocal,
  &KeyInheritedTypes,
  &KeyAttributes,
  &KeyAttribute,
  &KeyElements,
  &KeySubStructure,
  &KeyRanges,
  &KeyFixits,
  &KeyDiagnostics,
  &KeyFormatOptions,
  &KeyCodeCompleteOptions,
  &KeyFilterRules,
  &KeyNextRequestStart,
  &KeyPopular,
  &KeyUnpopular,
  &KeyHide,

  &KeyPlatform,
  &KeyIsDeprecated,
  &KeyIsUnavailable,
  &KeyIsOptional,
  &KeyMessage,
  &KeyIntroduced,
  &KeyDeprecated,
  &KeyObsoleted,
  &KeyRemoveCache,

  &KeyTypeInterface,
  &KeyTypeUsr,
  &KeyContainerTypeUsr,
  &KeyModuleGroups,

  &KeyBaseName,
  &KeyArgNames,
  &KeySelectorPieces,
  &KeyNameKind,

};

static unsigned findPrintOrderForDictKey(UIdent Key) {
  unsigned Order = 1;
  for (auto UIDPtr : OrderedKeys) {
    if (*UIDPtr == Key)
      return Order;
    ++Order;
  }
  return 10000;
}

bool sourcekitd::compareDictKeys(UIdent LHS, UIdent RHS) {
  if (LHS == RHS)
    return false;
  unsigned LHSOrder = findPrintOrderForDictKey(LHS);
  unsigned RHSOrder = findPrintOrderForDictKey(RHS);
  if (LHSOrder == RHSOrder)
    return LHS.getName() < RHS.getName();
  return LHSOrder < RHSOrder;
}


namespace {
template <typename ImplClass,
          typename RetTy = void>
class VariantVisitor {
public:
  typedef std::vector<std::pair<UIdent, sourcekitd_variant_t>> DictMap;

  static bool compKeys(const std::pair<UIdent, sourcekitd_variant_t> &LHS,
                       const std::pair<UIdent, sourcekitd_variant_t> &RHS) {
    return sourcekitd::compareDictKeys(LHS.first, RHS.first);
  }

  RetTy visit(sourcekitd_variant_t Obj) {
    switch (sourcekitd_variant_get_type(Obj)) {
    case SOURCEKITD_VARIANT_TYPE_NULL:
      return static_cast<ImplClass*>(this)->visitNull();
    case SOURCEKITD_VARIANT_TYPE_DICTIONARY: {
      DictMap Dict;
      DictMap &DictRef = Dict;
      sourcekitd_variant_dictionary_apply_impl(
          Obj,
          [&](sourcekitd_uid_t key, sourcekitd_variant_t value) {
            DictRef.push_back({UIdentFromSKDUID(key), value});
            return true;
          });
      std::sort(Dict.begin(), Dict.end(), compKeys);
      return static_cast<ImplClass*>(this)->visitDictionary(Dict);
    }
    case SOURCEKITD_VARIANT_TYPE_ARRAY: {
      std::vector<sourcekitd_variant_t> Vec;
      for (size_t i = 0, e = sourcekitd_variant_array_get_count(Obj);
           i != e; ++i)
        Vec.push_back(sourcekitd_variant_array_get_value(Obj, i));
      return static_cast<ImplClass*>(this)->visitArray(Vec);
    }
    case SOURCEKITD_VARIANT_TYPE_INT64:
      return static_cast<ImplClass*>(this)->visitInt64(
                                       sourcekitd_variant_int64_get_value(Obj));
    case SOURCEKITD_VARIANT_TYPE_BOOL:
      return static_cast<ImplClass*>(this)->visitBool(
                                       sourcekitd_variant_bool_get_value(Obj));
    case SOURCEKITD_VARIANT_TYPE_STRING: {
      size_t Len = sourcekitd_variant_string_get_length(Obj);
      const char *Ptr = sourcekitd_variant_string_get_ptr(Obj);
      return static_cast<ImplClass*>(this)->visitString(StringRef(Ptr, Len));
    }
    case SOURCEKITD_VARIANT_TYPE_UID: {
      sourcekitd_uid_t UID = sourcekitd_variant_uid_get_value(Obj);
      size_t Len = sourcekitd_uid_get_length(UID);
      const char *Ptr = sourcekitd_uid_get_string_ptr(UID);
      return static_cast<ImplClass*>(this)->visitUID(StringRef(Ptr, Len));
    }
    }
  }
};

class VariantPrinter : public VariantVisitor<VariantPrinter>,
                       public RequestResponsePrinterBase<VariantPrinter,
                                                         sourcekitd_variant_t> {
public:
  VariantPrinter(raw_ostream &OS, unsigned Indent = 0, bool PrintAsJSON = false)
    : RequestResponsePrinterBase(OS, Indent, PrintAsJSON) { }
};
}

void sourcekitd::writeEscaped(llvm::StringRef Str, llvm::raw_ostream &OS) {
  for (unsigned i = 0, e = Str.size(); i != e; ++i) {
    unsigned char c = Str[i];

    switch (c) {
    case '\\':
      OS << '\\' << '\\';
      break;
    case '\t':
      OS << '\\' << 't';
      break;
    case '\n':
      OS << '\\' << 'n';
      break;
    case '"':
      OS << '\\' << '"';
      break;
    default:
      OS << c;
      break;
    }
  }
}

static void printError(sourcekitd_response_t Err, raw_ostream &OS) {
  OS << "error response (";
  switch (sourcekitd_response_error_get_kind(Err)) {
  case SOURCEKITD_ERROR_CONNECTION_INTERRUPTED:
    OS << "Connection Interrupted"; break;
  case SOURCEKITD_ERROR_REQUEST_INVALID:
    OS << "Request Invalid"; break;
  case SOURCEKITD_ERROR_REQUEST_FAILED:
    OS << "Request Failed"; break;
  case SOURCEKITD_ERROR_REQUEST_CANCELLED:
    OS << "Request Cancelled"; break;
  }
  OS << "): ";
  OS << sourcekitd_response_error_get_description(Err);
}

static void printVariant(sourcekitd_variant_t obj, raw_ostream &OS) {
  VariantPrinter(OS).visit(obj);
}

void sourcekitd::printResponse(sourcekitd_response_t Resp, raw_ostream &OS) {
  if (!Resp) {
    OS << "<<NULL>>";
    return;
  }

  if (sourcekitd_response_is_error(Resp))
    printError(Resp, OS);
  else
    printVariant(sourcekitd_response_get_value(Resp), OS);
}

static void fatal_error_handler(void *user_data, const std::string& reason,
                                bool gen_crash_diag) {
  // Write the result out to stderr avoiding errs() because raw_ostreams can
  // call report_fatal_error.
  // FIXME: Put the error message in the crash report.
  fprintf(stderr, "SOURCEKITD FATAL ERROR: %s\n", reason.c_str());
  ::abort();
}

void sourcekitd::enableLogging(StringRef LoggerName) {
  Logger::Level LogLevel = Logger::Level::Warning;
  const char *EnvOpt = ::getenv("SOURCEKIT_LOGGING");
  if (EnvOpt) {
    int Val;
    bool Err = StringRef(EnvOpt).getAsInteger(10, Val);
    if (!Err) {
      if (Val > 2)
        LogLevel = Logger::Level::InfoLowPrio;
      else if (Val == 2)
        LogLevel = Logger::Level::InfoMediumPrio;
      else if (Val == 1)
        LogLevel = Logger::Level::InfoHighPrio;
    }
  }

  Logger::enableLogging(LoggerName, LogLevel);
}

//===----------------------------------------------------------------------===//
// Public API
//===----------------------------------------------------------------------===//

static llvm::sys::Mutex GlobalInitMtx;
static unsigned gInitRefCount = 0;

void sourcekitd_initialize(void) {
  llvm::sys::ScopedLock L(GlobalInitMtx);
  ++gInitRefCount;
  if (gInitRefCount > 1)
    return;

  static std::once_flag flag;
  std::call_once(flag, []() {
    llvm::install_fatal_error_handler(fatal_error_handler, 0);
    sourcekitd::enableLogging("sourcekit");
  });

  LOG_INFO_FUNC(High, "initializing");
  sourcekitd::initialize();
}

void sourcekitd_shutdown(void) {
  llvm::sys::ScopedLock L(GlobalInitMtx);
  --gInitRefCount;
  if (gInitRefCount > 0)
    return;

  LOG_INFO_FUNC(High, "shutting down");
  sourcekitd::shutdown();
}

void
sourcekitd_response_description_dump(sourcekitd_response_t resp) {
  // Avoid colors here, we don't properly detect that the debug window inside
  // Xcode doesn't support colors.
  llvm::SmallString<128> Desc;
  llvm::raw_svector_ostream OS(Desc);
  printResponse(resp, OS);
  llvm::errs() << OS.str() << '\n';
}

void
sourcekitd_response_description_dump_filedesc(sourcekitd_response_t resp,
                                              int fd) {
  llvm::raw_fd_ostream OS(fd, /*shouldClose=*/false);
  printResponse(resp, OS);
  OS << '\n';
}

char *
sourcekitd_response_description_copy(sourcekitd_response_t resp) {
  llvm::SmallString<128> Desc;
  llvm::raw_svector_ostream OS(Desc);
  printResponse(resp, OS);
  return strdup(Desc.c_str());
}


sourcekitd_uid_t
sourcekitd_uid_get_from_cstr(const char *string) {
  return SKDUIDFromUIdent(UIdent(string));
}

sourcekitd_uid_t
sourcekitd_uid_get_from_buf(const char *buf, size_t length) {
  return SKDUIDFromUIdent(UIdent(llvm::StringRef(buf, length)));
}

size_t
sourcekitd_uid_get_length(sourcekitd_uid_t uid) {
  UIdent UID = UIdentFromSKDUID(uid);
  return UID.getName().size();
}

const char *
sourcekitd_uid_get_string_ptr(sourcekitd_uid_t uid) {
  UIdent UID = UIdentFromSKDUID(uid);
  return UID.getName().begin();
}

void
sourcekitd_request_description_dump(sourcekitd_object_t obj) {
  // Avoid colors here, we don't properly detect that the debug window inside
  // Xcode doesn't support colors.
  llvm::SmallString<128> Desc;
  llvm::raw_svector_ostream OS(Desc);
  printRequestObject(obj, OS);
  llvm::errs() << OS.str() << '\n';
}

char *
sourcekitd_request_description_copy(sourcekitd_object_t obj) {
  llvm::SmallString<128> Desc;
  llvm::raw_svector_ostream OS(Desc);
  printRequestObject(obj, OS);
  return strdup(Desc.c_str());
}

//===----------------------------------------------------------------------===//
// Variant API
//===----------------------------------------------------------------------===//

#define VAR_FN(var, name) ((var).data[0] ? \
                            ((VariantFunctions*)(var).data[0])->name : nullptr)

sourcekitd_variant_type_t
sourcekitd_variant_get_type(sourcekitd_variant_t var) {
  if (auto fn = VAR_FN(var, get_type))
    return fn(var);

  // Default implementation:
  // Assume the variant encapsulates a basic type.
  static_assert(SOURCEKITD_VARIANT_TYPE_NULL == 0,
                "NULL type does not match null variant");
  return (sourcekitd_variant_type_t)var.data[2];
}

sourcekitd_variant_t
sourcekitd_variant_dictionary_get_value(sourcekitd_variant_t dict,
                                        sourcekitd_uid_t key) {
  if (auto fn = VAR_FN(dict, dictionary_get_value))
    return fn(dict, key);

  // Default implementation:
  // Linear search for the key/value pair via sourcekitd_variant_dictionary_apply.
  sourcekitd_variant_t result = makeNullVariant();
  sourcekitd_variant_dictionary_apply_impl(
      dict, [&](sourcekitd_uid_t curr_key, sourcekitd_variant_t curr_value) {
        if (curr_key == key) {
          result = curr_value;
          return false;
        }
        return true;
      });

  return result;
}

const char *
sourcekitd_variant_dictionary_get_string(sourcekitd_variant_t dict,
                                         sourcekitd_uid_t key) {
  if (auto fn = VAR_FN(dict, dictionary_get_string))
    return fn(dict, key);

  // Default implementation:
  // Get the value via sourcekitd_variant_dictionary_get_value.
  return sourcekitd_variant_string_get_ptr(
             sourcekitd_variant_dictionary_get_value(dict, key));
}

int64_t
sourcekitd_variant_dictionary_get_int64(sourcekitd_variant_t dict,
                                        sourcekitd_uid_t key) {
  if (auto fn = VAR_FN(dict, dictionary_get_int64))
    return fn(dict, key);

  // Default implementation:
  // Get the value via sourcekitd_variant_dictionary_get_value.
  return sourcekitd_variant_int64_get_value(
             sourcekitd_variant_dictionary_get_value(dict, key));
}

bool
sourcekitd_variant_dictionary_get_bool(sourcekitd_variant_t dict,
                                       sourcekitd_uid_t key) {
  if (auto fn = VAR_FN(dict, dictionary_get_bool))
    return fn(dict, key);

  // Default implementation:
  // Get the value via sourcekitd_variant_dictionary_get_value.
  return sourcekitd_variant_bool_get_value(
             sourcekitd_variant_dictionary_get_value(dict, key));
}

sourcekitd_uid_t
sourcekitd_variant_dictionary_get_uid(sourcekitd_variant_t dict,
                                      sourcekitd_uid_t key) {
  if (auto fn = VAR_FN(dict, dictionary_get_uid))
    return fn(dict, key);

  // Default implementation:
  // Get the value via sourcekitd_variant_dictionary_get_value.
  return sourcekitd_variant_uid_get_value(
             sourcekitd_variant_dictionary_get_value(dict, key));
}

#if SOURCEKITD_HAS_BLOCKS
bool
sourcekitd_variant_dictionary_apply(sourcekitd_variant_t dict,
                              sourcekitd_variant_dictionary_applier_t applier) {
  return sourcekitd_variant_dictionary_apply_impl(dict, applier);
}
#endif

bool
sourcekitd_variant_dictionary_apply_impl(
  sourcekitd_variant_t dict,
  llvm::function_ref<bool(sourcekitd_uid_t, sourcekitd_variant_t)> applier) {
  if (auto fn = VAR_FN(dict, dictionary_apply))
    return fn(dict, applier);

  // Default implementation:
  // Treat as empty container.
  return true;
}

bool
sourcekitd_variant_dictionary_apply_f(sourcekitd_variant_t dict,
                              sourcekitd_variant_dictionary_applier_f_t applier,
                              void *context) {
  return sourcekitd_variant_dictionary_apply_impl(
      dict,
      [&](sourcekitd_uid_t key, sourcekitd_variant_t value) {
          return applier(key, value, context);
  });
}

size_t
sourcekitd_variant_array_get_count(sourcekitd_variant_t array) {
  if (auto fn = VAR_FN(array, array_get_count))
    return fn(array);

  // Default implementation:
  // Treat as empty container.
  return 0;
}

sourcekitd_variant_t
sourcekitd_variant_array_get_value(sourcekitd_variant_t array, size_t index) {
  if (auto fn = VAR_FN(array, array_get_value))
    return fn(array, index);

  llvm::report_fatal_error("Trying to index an empty array.");
}

const char *
sourcekitd_variant_array_get_string(sourcekitd_variant_t array, size_t index) {
  if (auto fn = VAR_FN(array, array_get_string))
    return fn(array, index);

  // Default implementation:
  // Get the value via sourcekitd_variant_array_get_value.
  return sourcekitd_variant_string_get_ptr(
             sourcekitd_variant_array_get_value(array, index));
}

int64_t
sourcekitd_variant_array_get_int64(sourcekitd_variant_t array, size_t index) {
  if (auto fn = VAR_FN(array, array_get_int64))
    return fn(array, index);

  // Default implementation:
  // Get the value via sourcekitd_variant_array_get_value.
  return sourcekitd_variant_int64_get_value(
             sourcekitd_variant_array_get_value(array, index));
}

bool
sourcekitd_variant_array_get_bool(sourcekitd_variant_t array, size_t index) {
  if (auto fn = VAR_FN(array, array_get_bool))
    return fn(array, index);

  // Default implementation:
  // Get the value via sourcekitd_variant_array_get_value.
  return sourcekitd_variant_bool_get_value(
             sourcekitd_variant_array_get_value(array, index));
}

sourcekitd_uid_t
sourcekitd_variant_array_get_uid(sourcekitd_variant_t array, size_t index) {
  if (auto fn = VAR_FN(array, array_get_uid))
    return fn(array, index);

  // Default implementation:
  // Get the value via sourcekitd_variant_array_get_value.
  return sourcekitd_variant_uid_get_value(
             sourcekitd_variant_array_get_value(array, index));
}

#if SOURCEKITD_HAS_BLOCKS
bool
sourcekitd_variant_array_apply(sourcekitd_variant_t array,
                               sourcekitd_variant_array_applier_t applier) {
  return sourcekitd_variant_array_apply_impl(array, applier);
}
#endif

bool sourcekitd_variant_array_apply_impl(
    sourcekitd_variant_t array,
    llvm::function_ref<bool(size_t, sourcekitd_variant_t)> applier) {
  if (auto fn = VAR_FN(array, array_apply))
    return fn(array, applier);

  // Default implementation:
  // Iterate over elements via a for-loop.
  for (size_t i = 0, e = sourcekitd_variant_array_get_count(array); i != e; ++i) {
    bool Continue = applier(i, sourcekitd_variant_array_get_value(array, i));
    if (!Continue)
      return false;
  }
  return true;
}

bool sourcekitd_variant_array_apply_f(
    sourcekitd_variant_t array, sourcekitd_variant_array_applier_f_t applier,
    void *context) {
  return sourcekitd_variant_array_apply_impl(
      array, [&](size_t index, sourcekitd_variant_t value) {
        return applier(index, value, context);
      });
}

int64_t
sourcekitd_variant_int64_get_value(sourcekitd_variant_t obj) {
  if (auto fn = VAR_FN(obj, int64_get_value))
    return fn(obj);

  // Default implementation:
  // Assume this is a variant encapsulating the basic type.
  return obj.data[1];
}

bool
sourcekitd_variant_bool_get_value(sourcekitd_variant_t obj) {
  if (auto fn = VAR_FN(obj, bool_get_value))
    return fn(obj);

  // Default implementation:
  // Assume this is a variant encapsulating the basic type.
  return obj.data[1];
}

size_t
sourcekitd_variant_string_get_length(sourcekitd_variant_t obj) {
  if (auto fn = VAR_FN(obj, string_get_length))
    return fn(obj);

  // Default implementation:
  // Use strlen.
  return strlen(sourcekitd_variant_string_get_ptr(obj));
}

const char *
sourcekitd_variant_string_get_ptr(sourcekitd_variant_t obj) {
  if (auto fn = VAR_FN(obj, string_get_ptr))
    return fn(obj);

  // Default implementation:
  // Assume this is a variant encapsulating the basic type.
  return (const char *)obj.data[1];
}

sourcekitd_uid_t
sourcekitd_variant_uid_get_value(sourcekitd_variant_t obj) {
  if (auto fn = VAR_FN(obj, uid_get_value))
    return fn(obj);

  // Default implementation:
  // Assume this is a variant encapsulating the basic type.
  return (sourcekitd_uid_t)obj.data[1];
}

void
sourcekitd_variant_description_dump(sourcekitd_variant_t obj) {
  // Avoid colors here, we don't properly detect that the debug window inside
  // Xcode doesn't support colors.
  llvm::SmallString<128> Desc;
  llvm::raw_svector_ostream OS(Desc);
  printVariant(obj, OS);
  llvm::errs() << OS.str() << '\n';
}

void
sourcekitd_variant_description_dump_filedesc(sourcekitd_variant_t obj, int fd) {
  llvm::raw_fd_ostream OS(fd, /*shouldClose=*/false);
  printVariant(obj, OS);
  OS << '\n';
}

char *
sourcekitd_variant_description_copy(sourcekitd_variant_t obj) {
  llvm::SmallString<128> Desc;
  llvm::raw_svector_ostream OS(Desc);
  printVariant(obj, OS);
  return strdup(Desc.c_str());
}

char *
sourcekitd_variant_json_description_copy(sourcekitd_variant_t obj) {
  llvm::SmallString<128> Desc;
  {
    llvm::raw_svector_ostream OS(Desc);
    VariantPrinter(OS, /*Indent=*/0, /*PrintAsJSON=*/true).visit(obj);
  }
  return strdup(Desc.c_str());
}

namespace {
class YAMLRequestParser {
public:
  sourcekitd_object_t parse(StringRef YAMLStr, std::string &Error);

private:
  sourcekitd_object_t createObjFromNode(llvm::yaml::Node *Value,
                                        std::string &Error);
  bool parseDict(sourcekitd_object_t Dict, llvm::yaml::MappingNode *Node,
                  std::string &Error);
  bool parseArray(sourcekitd_object_t Array, llvm::yaml::SequenceNode *Node,
                   std::string &Error);
  void initError(StringRef Desc, llvm::yaml::Node *Node, std::string &Error);
  sourcekitd_object_t withError(StringRef Desc, llvm::yaml::Node *Node,
                                std::string &Error) {
    initError(Desc, Node, Error);
    return nullptr;
  }
  bool withBoolError(StringRef Desc, llvm::yaml::Node *Node,
                     std::string &Error) {
    initError(Desc, Node, Error);
    return true;
  }
};
} // anonymous namespace

sourcekitd_object_t
sourcekitd_request_create_from_yaml(const char *yaml, char **error) {
  std::string Error;
  sourcekitd_object_t Result = YAMLRequestParser().parse(yaml, Error);
  if (Result)
    return Result;
  if (error)
    *error = strdup(Error.c_str());
  return nullptr;
}

sourcekitd_object_t YAMLRequestParser::parse(StringRef YAMLStr,
                                             std::string &Error) {
  llvm::SourceMgr SM;
  llvm::yaml::Stream YAMLStream(YAMLStr, SM);
  llvm::yaml::document_iterator I = YAMLStream.begin();
  if (I == YAMLStream.end()) {
    Error = "Error while parsing";
    return nullptr;
  }
  llvm::yaml::Node *Root = I->getRoot();
  if (Root == nullptr) {
    Error = "Error while parsing";
    return nullptr;
  }

  return createObjFromNode(Root, Error);
}

sourcekitd_object_t YAMLRequestParser::createObjFromNode(
                                  llvm::yaml::Node *Value, std::string &Error) {
  if (auto Array = dyn_cast<llvm::yaml::SequenceNode>(Value)) {
    sourcekitd_object_t Val = sourcekitd_request_array_create(nullptr, 0);
    if (parseArray(Val, Array, Error)) {
      sourcekitd_request_release(Val);
      return nullptr;
    }
    return Val;
  }

  if (auto Map = dyn_cast<llvm::yaml::MappingNode>(Value)) {
    sourcekitd_object_t Val =
        sourcekitd_request_dictionary_create(nullptr, nullptr, 0);
    if (parseDict(Val, Map, Error)) {
      sourcekitd_request_release(Val);
      return nullptr;
    }
    return Val;
  }

  if (auto ValueString = dyn_cast<llvm::yaml::ScalarNode>(Value)) {
    StringRef Raw = ValueString->getRawValue().trim();
    SmallString<32> ValueStorage;
    if (Raw[0] == '\"') {
      SmallString<32> Str(ValueString->getValue(ValueStorage));
      return sourcekitd_request_string_create(Str.c_str());
    }

    long long val;
    if (!Raw.getAsInteger(10, val))
      return sourcekitd_request_int64_create(val);

    if (Raw.find(' ') != StringRef::npos)
      return withError("Found space in non-string value", Value, Error);

    return sourcekitd_request_uid_create(
        sourcekitd_uid_get_from_buf(Raw.data(), Raw.size()));
  }

  return withError("Expected value as array, dictionary, or scalar", Value,
                   Error);
}


bool YAMLRequestParser::parseDict(sourcekitd_object_t Dict,
                                  llvm::yaml::MappingNode *Node,
                                  std::string &Error) {

  for (llvm::yaml::MappingNode::iterator KVI = Node->begin(),
                                         KVE = Node->end();
       KVI != KVE; ++KVI) {
    llvm::yaml::Node *Value = (*KVI).getValue();
    if (Value == nullptr || isa<llvm::yaml::NullNode>(Value))
      return withBoolError("Expected value", (*KVI).getKey(), Error);

    sourcekitd_object_t Val = createObjFromNode(Value, Error);
    if (!Val)
      return true;

    auto KeyString = dyn_cast<llvm::yaml::ScalarNode>((*KVI).getKey());
    if (KeyString == nullptr) {
      sourcekitd_request_release(Val);
      return withBoolError("Expected string as key", (*KVI).getKey(), Error);
    }

    SmallString<32> KeyStorage;
    StringRef Key = KeyString->getValue(KeyStorage);
    sourcekitd_uid_t uid_key =
        sourcekitd_uid_get_from_buf(Key.data(), Key.size());

    sourcekitd_request_dictionary_set_value(Dict, uid_key, Val);
  }

  return false;
}

bool YAMLRequestParser::parseArray(sourcekitd_object_t Array,
                                   llvm::yaml::SequenceNode *Node,
                                   std::string &Error) {
  for (llvm::yaml::SequenceNode::iterator AI = Node->begin(),
                                          AE = Node->end();
       AI != AE; ++AI) {
    sourcekitd_object_t Val = createObjFromNode(&*AI, Error);
    if (!Val)
      return true;

    sourcekitd_request_array_set_value(Array, SOURCEKITD_ARRAY_APPEND, Val);
  }

  return false;
}

void YAMLRequestParser::initError(StringRef Desc, llvm::yaml::Node *Node,
                                  std::string &Error) {
  Error = Desc;
  Error += " at: ";
  llvm::SMRange Range = Node->getSourceRange();
  StringRef Text(Range.Start.getPointer(),
                 Range.End.getPointer() - Range.Start.getPointer());
  Error.append(Text.begin(), Text.end());
}
