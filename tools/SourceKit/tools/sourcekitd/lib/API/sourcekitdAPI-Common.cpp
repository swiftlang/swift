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

#include "sourcekitd/DictionaryKeys.h"
#include "sourcekitd/Internal.h"
#include "sourcekitd/Logging.h"
#include "sourcekitd/RequestResponsePrinterBase.h"
#include "SourceKit/Support/Logging.h"
#include "SourceKit/Support/UIdent.h"
#include "swift/Basic/LoadDynamicLibrary.h"
#include "swift/Basic/StringExtras.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Mutex.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/Threading.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/YAMLParser.h"
#include <mutex>

#if defined(_WIN32) && !defined(__CYGWIN__)
#define NOMINMAX
#include <Windows.h>
#else
#include <dlfcn.h>
#endif

using namespace SourceKit;
using namespace sourcekitd;
using llvm::ArrayRef;
using llvm::StringRef;
using llvm::raw_ostream;

#define KEY(NAME, CONTENT) UIdent sourcekitd::Key##NAME(CONTENT);
#include "SourceKit/Core/ProtocolUIDs.def"

/// Order for the keys to use when emitting the debug description of
/// dictionaries.
static UIdent *OrderedKeys[] = {
#define KEY(NAME, CONTENT) &Key##NAME,
#include "SourceKit/Core/ProtocolUIDs.def"
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

/// This a near-copy of llvm::function_ref, but that exposes its members
/// publicly so we can efficiently wrap the applier functions below.
template <typename Fn>
class applier_function_ref;

template <typename Ret, typename... Params>
class applier_function_ref<Ret(Params...)> {
public:
  Ret (*callback)(Params... params, void *context) = nullptr;
  void *context;

  template <typename Callable>
  static Ret callback_fn(Params... params, void *context) {
    return (*reinterpret_cast<Callable *>(context))(
        std::forward<Params>(params)...);
  }

  template <typename Callable>
  applier_function_ref(
      Callable &&callable,
      std::enable_if_t<
          !std::is_same<std::remove_cv_t<std::remove_reference_t<Callable>>,
                        applier_function_ref>::value> * = nullptr)
      : callback(callback_fn<typename std::remove_reference<Callable>::type>),
        context(reinterpret_cast<void *>(&callable)) {}

  Ret operator()(Params... params) const {
    return callback(std::forward<Params>(params)..., context);
  }
};
} // end anonymous namespace

static bool variant_dictionary_apply(
    sourcekitd_variant_t dict,
    applier_function_ref<bool(sourcekitd_uid_t, sourcekitd_variant_t)>
        applier) {
  return sourcekitd_variant_dictionary_apply_impl(dict, applier.callback,
                                                  applier.context);
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
      variant_dictionary_apply(
          Obj, [&](sourcekitd_uid_t key, sourcekitd_variant_t value) {
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
    case SOURCEKITD_VARIANT_TYPE_DOUBLE:
      return static_cast<ImplClass *>(this)->visitDouble(
          sourcekitd_variant_double_get_value(Obj));
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
    case SOURCEKITD_VARIANT_TYPE_DATA: {
      const void *Data = sourcekitd_variant_data_get_ptr(Obj);
      size_t Size = sourcekitd_variant_data_get_size(Obj);
      return static_cast<ImplClass*>(this)->visitData(Data, Size);
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
} // end anonymous namespace

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

static void fatal_error_handler(void *user_data, const char *reason,
                                bool gen_crash_diag) {
  // Write the result out to stderr avoiding errs() because raw_ostreams can
  // call report_fatal_error.
  // FIXME: Put the error message in the crash report.
  fprintf(stderr, "SOURCEKITD FATAL ERROR: %s\n", reason);
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

bool sourcekitd::initializeClient() {
  llvm::sys::ScopedLock L(GlobalInitMtx);
  ++gInitRefCount;
  if (gInitRefCount > 1)
    return false;

  static std::once_flag flag;
  std::call_once(flag, []() {
    llvm::install_fatal_error_handler(fatal_error_handler, 0);
    sourcekitd::enableLogging("sourcekit");
  });

  return true;
}

bool sourcekitd::shutdownClient() {
  llvm::sys::ScopedLock L(GlobalInitMtx);
  --gInitRefCount;
  if (gInitRefCount > 0)
    return false;
  return true;
}

extern "C" const char __dso_handle[];

static void withCurrentLibraryPath(llvm::function_ref<void(const char *)> body) {
#if defined(_WIN32) && !defined(__CYGWIN__)
  char path[MAX_PATH];
  HMODULE currentModule = NULL;

  if (GetModuleHandleExW(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS |
          GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
          (LPCWSTR) &withCurrentLibraryPath, &currentModule) == 0) {
      int error = GetLastError();
      LOG_WARN("plugin-loading", "failed to determine current Windows module. Error: " << error);
      return body(nullptr);
  }
  if (GetModuleFileNameA(currentModule, path, sizeof(path)) == 0) {
      int error = GetLastError();
      LOG_WARN("plugin-loading", "failed to path of current Windows module. Error: " << error);
      return body(nullptr);
  }
  return body(path);
#else
  Dl_info dlinfo;
  dladdr(__dso_handle, &dlinfo);
  return body(dlinfo.dli_fname);
#endif
}

static void loadPlugin(StringRef plugin, PluginInitParams &pluginParams) {
  std::string err;
  auto *handle = swift::loadLibrary(plugin.str().c_str(), &err);
  if (!handle) {
    LOG_WARN("plugin-loading",
             "failed to load plugin '" << plugin << "': " << err);
    return;
  }

  auto *plugin_init_2 = (sourcekitd_plugin_initialize_2_t)swift::getAddressOfSymbol(
      handle, "sourcekitd_plugin_initialize_2");
  if (plugin_init_2) {
    withCurrentLibraryPath([&](const char *currentLibraryPath) {
      plugin_init_2(&pluginParams, currentLibraryPath);
    });
    return;
  }

  // Fall back to the legacy sourcekitd_plugin_initialize function.
  auto *plugin_init = (sourcekitd_plugin_initialize_t)swift::getAddressOfSymbol(
      handle, "sourcekitd_plugin_initialize");
  if (plugin_init) {
    plugin_init(&pluginParams);
    return;
  }

  LOG_WARN("plugin-loading",
           "plugin '"
              << plugin
              << "' missing expected symbol: sourcekitd_plugin_initialize_2 or sourcekitd_plugin_initialize");
}

void sourcekitd::loadPlugins(ArrayRef<std::string> registeredPlugins,
                             PluginInitParams &pluginParams) {
  // Load from environment variable first so that it will override registered
  // plugins.
  StringRef envPlugins = getenv("SOURCEKIT_PLUGINS");
  while (!envPlugins.empty()) {
    StringRef plugin;
    std::tie(plugin, envPlugins) = envPlugins.split(":");
    assert(!plugin.empty());
    loadPlugin(plugin, pluginParams);
  }

  // Load registered plugins.
  for (const auto &path : registeredPlugins)
    loadPlugin(path, pluginParams);
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
  variant_dictionary_apply(
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

static bool variant_dictionary_applier_block_f(sourcekitd_uid_t key,
                                               sourcekitd_variant_t value,
                                               void *context) {
  auto *block = (sourcekitd_variant_dictionary_applier_t *)context;
  return (*block)(key, value);
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

double sourcekitd_variant_dictionary_get_double(sourcekitd_variant_t dict,
                                                sourcekitd_uid_t key) {
  if (auto fn = VAR_FN(dict, dictionary_get_double))
    return fn(dict, key);

  // Default implementation:
  // Get the value via sourcekitd_variant_dictionary_get_value.
  return sourcekitd_variant_double_get_value(
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
  return sourcekitd_variant_dictionary_apply_impl(
      dict, variant_dictionary_applier_block_f, &applier);
}
#endif

bool sourcekitd_variant_dictionary_apply_impl(
    sourcekitd_variant_t dict,
    sourcekitd_variant_dictionary_applier_f_t applier, void *context) {
  if (auto fn = VAR_FN(dict, dictionary_apply))
    return fn(dict, applier, context);

  // Default implementation:
  // Treat as empty container.
  return true;
}

bool
sourcekitd_variant_dictionary_apply_f(sourcekitd_variant_t dict,
                              sourcekitd_variant_dictionary_applier_f_t applier,
                              void *context) {
  return sourcekitd_variant_dictionary_apply_impl(dict, applier, context);
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

double sourcekitd_variant_array_get_double(sourcekitd_variant_t array,
                                           size_t index) {
  if (auto fn = VAR_FN(array, array_get_double))
    return fn(array, index);

  // Default implementation:
  // Get the value via sourcekitd_variant_array_get_value.
  return sourcekitd_variant_double_get_value(
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

static bool variant_array_applier_block_f(size_t index,
                                          sourcekitd_variant_t obj,
                                          void *context) {
  auto *block = (sourcekitd_variant_array_applier_t *)context;
  return (*block)(index, obj);
}

#if SOURCEKITD_HAS_BLOCKS
bool
sourcekitd_variant_array_apply(sourcekitd_variant_t array,
                               sourcekitd_variant_array_applier_t applier) {
  return sourcekitd_variant_array_apply_impl(
      array, variant_array_applier_block_f, &applier);
}
#endif

bool sourcekitd_variant_array_apply_impl(
    sourcekitd_variant_t array, sourcekitd_variant_array_applier_f_t applier,
    void *context) {
  if (auto fn = VAR_FN(array, array_apply))
    return fn(array, applier, context);

  // Default implementation:
  // Iterate over elements via a for-loop.
  for (size_t i = 0, e = sourcekitd_variant_array_get_count(array); i != e; ++i) {
    bool Continue =
        applier(i, sourcekitd_variant_array_get_value(array, i), context);
    if (!Continue)
      return false;
  }
  return true;
}

bool sourcekitd_variant_array_apply_f(
    sourcekitd_variant_t array, sourcekitd_variant_array_applier_f_t applier,
    void *context) {
  return sourcekitd_variant_array_apply_impl(array, applier, context);
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

double sourcekitd_variant_double_get_value(sourcekitd_variant_t obj) {
  if (auto fn = VAR_FN(obj, double_get_value))
    return fn(obj);

  // Default implementation:
  // Assume this is a variant encapsulating the basic type.
  double result;
  std::memcpy(&result, &obj.data[1], sizeof(double));
  return result;
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

size_t
sourcekitd_variant_data_get_size(sourcekitd_variant_t obj) {
  if (auto fn = VAR_FN(obj, data_get_size))
    return fn(obj);

  // Default implementation:
  // We store the byte's length in data[2] and its data in data[1]
  return obj.data[2];
}

const void *
sourcekitd_variant_data_get_ptr(sourcekitd_variant_t obj) {
  if (auto fn = VAR_FN(obj, data_get_ptr))
    return fn(obj);

  // Default implementation:
  // We store the byte's length in data[2] and its data in data[1]
  return reinterpret_cast<void *>(obj.data[1]);
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

    if (Raw.contains(' '))
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
  Error = Desc.str();
  Error += " at: ";
  llvm::SMRange Range = Node->getSourceRange();
  StringRef Text(Range.Start.getPointer(),
                 Range.End.getPointer() - Range.Start.getPointer());
  Error.append(Text.begin(), Text.end());
}

sourcekitd_variant_functions_t sourcekitd_variant_functions_create() {
  auto *vfuncs = new VariantFunctions();
  // Zero-initialize.
  memset(vfuncs, 0, sizeof(VariantFunctions));
  return vfuncs;
}

void sourcekitd_variant_functions_set_get_type(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_get_type_t f) {
  auto *vfuncs = static_cast<VariantFunctions *>(funcs);
  vfuncs->get_type = f;
}
void sourcekitd_variant_functions_set_array_apply(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_array_apply_t f) {
  auto *vfuncs = static_cast<VariantFunctions *>(funcs);
  vfuncs->array_apply = f;
}
void sourcekitd_variant_functions_set_array_get_bool(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_array_get_bool_t f) {
  auto *vfuncs = static_cast<VariantFunctions *>(funcs);
  vfuncs->array_get_bool = f;
}
void sourcekitd_variant_functions_set_array_get_double(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_array_get_double_t f) {
  auto *vfuncs = static_cast<VariantFunctions *>(funcs);
  vfuncs->array_get_double = f;
}
void sourcekitd_variant_functions_set_array_get_count(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_array_get_count_t f) {
  auto *vfuncs = static_cast<VariantFunctions *>(funcs);
  vfuncs->array_get_count = f;
}
void sourcekitd_variant_functions_set_array_get_int64(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_array_get_int64_t f) {
  auto *vfuncs = static_cast<VariantFunctions *>(funcs);
  vfuncs->array_get_int64 = f;
}
void sourcekitd_variant_functions_set_array_get_string(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_array_get_string_t f) {
  auto *vfuncs = static_cast<VariantFunctions *>(funcs);
  vfuncs->array_get_string = f;
}
void sourcekitd_variant_functions_set_array_get_uid(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_array_get_uid_t f) {
  auto *vfuncs = static_cast<VariantFunctions *>(funcs);
  vfuncs->array_get_uid = f;
}
void sourcekitd_variant_functions_set_array_get_value(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_array_get_value_t f) {
  auto *vfuncs = static_cast<VariantFunctions *>(funcs);
  vfuncs->array_get_value = f;
}
void sourcekitd_variant_functions_set_bool_get_value(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_bool_get_value_t f) {
  auto *vfuncs = static_cast<VariantFunctions *>(funcs);
  vfuncs->bool_get_value = f;
}
void sourcekitd_variant_functions_set_double_get_value(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_double_get_value_t f) {
  auto *vfuncs = static_cast<VariantFunctions *>(funcs);
  vfuncs->double_get_value = f;
}
void sourcekitd_variant_functions_set_dictionary_apply(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_dictionary_apply_t f) {
  auto *vfuncs = static_cast<VariantFunctions *>(funcs);
  vfuncs->dictionary_apply = f;
}
void sourcekitd_variant_functions_set_dictionary_get_bool(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_dictionary_get_bool_t f) {
  auto *vfuncs = static_cast<VariantFunctions *>(funcs);
  vfuncs->dictionary_get_bool = f;
}
void sourcekitd_variant_functions_set_dictionary_get_double(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_dictionary_get_double_t f) {
  auto *vfuncs = static_cast<VariantFunctions *>(funcs);
  vfuncs->dictionary_get_double = f;
}
void sourcekitd_variant_functions_set_dictionary_get_int64(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_dictionary_get_int64_t f) {
  auto *vfuncs = static_cast<VariantFunctions *>(funcs);
  vfuncs->dictionary_get_int64 = f;
}
void sourcekitd_variant_functions_set_dictionary_get_string(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_dictionary_get_string_t f) {
  auto *vfuncs = static_cast<VariantFunctions *>(funcs);
  vfuncs->dictionary_get_string = f;
}
void sourcekitd_variant_functions_set_dictionary_get_value(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_dictionary_get_value_t f) {
  auto *vfuncs = static_cast<VariantFunctions *>(funcs);
  vfuncs->dictionary_get_value = f;
}
void sourcekitd_variant_functions_set_dictionary_get_uid(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_dictionary_get_uid_t f) {
  auto *vfuncs = static_cast<VariantFunctions *>(funcs);
  vfuncs->dictionary_get_uid = f;
}
void sourcekitd_variant_functions_set_string_get_length(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_string_get_length_t f) {
  auto *vfuncs = static_cast<VariantFunctions *>(funcs);
  vfuncs->string_get_length = f;
}
void sourcekitd_variant_functions_set_string_get_ptr(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_string_get_ptr_t f) {
  auto *vfuncs = static_cast<VariantFunctions *>(funcs);
  vfuncs->string_get_ptr = f;
}
void sourcekitd_variant_functions_set_int64_get_value(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_int64_get_value_t f) {
  auto *vfuncs = static_cast<VariantFunctions *>(funcs);
  vfuncs->int64_get_value = f;
}
void sourcekitd_variant_functions_set_uid_get_value(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_uid_get_value_t f) {
  auto *vfuncs = static_cast<VariantFunctions *>(funcs);
  vfuncs->uid_get_value = f;
}
void sourcekitd_variant_functions_set_data_get_size(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_data_get_size_t f) {
  auto *vfuncs = static_cast<VariantFunctions *>(funcs);
  vfuncs->data_get_size = f;
}
void sourcekitd_variant_functions_set_data_get_ptr(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_data_get_ptr_t f) {
  auto *vfuncs = static_cast<VariantFunctions *>(funcs);
  vfuncs->data_get_ptr = f;
}

bool sourcekitd_plugin_initialize_is_client_only(
    sourcekitd_plugin_initialize_params_t _params) {
  auto &params = *static_cast<sourcekitd::PluginInitParams *>(_params);
  return params.isClientOnly;
}

uint64_t sourcekitd_plugin_initialize_custom_buffer_start(
    sourcekitd_plugin_initialize_params_t _params) {
  auto &params = *static_cast<sourcekitd::PluginInitParams *>(_params);
  return params.customBufferStart;
}

sourcekitd_uid_get_from_cstr_t sourcekitd_plugin_initialize_uid_get_from_cstr(
    sourcekitd_plugin_initialize_params_t _params) {
  auto &params = *static_cast<sourcekitd::PluginInitParams *>(_params);
  return params.uidGetFromCstr;
}

sourcekitd_uid_get_string_ptr_t sourcekitd_plugin_initialize_uid_get_string_ptr(
    sourcekitd_plugin_initialize_params_t _params) {
  auto &params = *static_cast<sourcekitd::PluginInitParams *>(_params);
  return params.uidGetStringPtr;
}

void sourcekitd_plugin_initialize_register_request_handler(
    sourcekitd_plugin_initialize_params_t _params,
    sourcekitd_request_handler_t handler) {
  auto &params = *static_cast<sourcekitd::PluginInitParams *>(_params);
  auto handler_wrapper =
      ^bool(sourcekitd_object_t req, sourcekitd_request_handle_t,
            void (^resp)(sourcekitd_response_t)) {
        return handler(req, resp);
      };
  params.registerRequestHandler(handler_wrapper);
}

void sourcekitd_plugin_initialize_register_cancellable_request_handler(
    sourcekitd_plugin_initialize_params_t _params,
    sourcekitd_cancellable_request_handler_t handler) {
  auto &params = *static_cast<sourcekitd::PluginInitParams *>(_params);
  params.registerRequestHandler(handler);
}

void sourcekitd_plugin_initialize_register_cancellation_handler(
    sourcekitd_plugin_initialize_params_t _params,
    sourcekitd_cancellation_handler_t handler) {
  auto &params = *static_cast<sourcekitd::PluginInitParams *>(_params);
  params.registerCancellationHandler(handler);
}

void sourcekitd_plugin_initialize_register_custom_buffer(
    sourcekitd_plugin_initialize_params_t _params, uint64_t kind,
    sourcekitd_variant_functions_t funcs) {
  auto &params = *static_cast<sourcekitd::PluginInitParams *>(_params);
  params.registerCustomBuffer(kind, funcs);
}

void *sourcekitd_plugin_initialize_get_swift_ide_inspection_instance(
    sourcekitd_plugin_initialize_params_t _params) {
  auto &params = *static_cast<sourcekitd::PluginInitParams *>(_params);
  return params.opaqueIDEInspectionInstance;
}

PluginInitParams::PluginInitParams(
    bool isClientOnly,
    std::function<void(sourcekitd_cancellable_request_handler_t)>
        registerRequestHandler,
    std::function<void(sourcekitd_cancellation_handler_t)>
        registerCancellationHandler,
    void *opaqueIDEInspectionInstance)
    : isClientOnly(isClientOnly) {
  assert(isClientOnly || (registerRequestHandler != nullptr &&
                          registerCancellationHandler != nullptr));
  // Note: we pass in registerRequestHandler rather than accessing
  // sourcekitd::pluginRegisterRequestHandler so that when constructing
  // parameters from the sourcekitd client library it does not pull in all of
  // request handling when linking.
  if (!isClientOnly) {
    this->registerRequestHandler = registerRequestHandler;
    this->registerCancellationHandler = registerCancellationHandler;
  }
  registerCustomBuffer = [this](uint64_t kind,
                                sourcekitd_variant_functions_t funcs) {
    sourcekitd::pluginRegisterCustomBufferKind(kind, funcs);
    customBufferStart = std::max(customBufferStart, kind + 1);
  };
  this->opaqueIDEInspectionInstance = opaqueIDEInspectionInstance;
}

// MARK: Plugin variant functions

// Note: only modified during plugin loading.
static std::vector<VariantFunctions *> PluginVariantFunctions;

VariantFunctions *sourcekitd::getPluginVariantFunctions(size_t BufKind) {
  size_t index = BufKind - (size_t)CustomBufferKind::CustomBufferKind_End;
  if (index >= PluginVariantFunctions.size() ||
      PluginVariantFunctions[index] == nullptr) {
    llvm::report_fatal_error(
        "unknown custom buffer kind; possible plugin loading failure");
  }
  return PluginVariantFunctions[index];
}

void sourcekitd::pluginRegisterCustomBufferKind(
    uint64_t kind, sourcekitd_variant_functions_t funcs) {
  auto index = kind - (uint64_t)CustomBufferKind::CustomBufferKind_End;
  if (index < PluginVariantFunctions.size()) {
    assert(PluginVariantFunctions[index] == nullptr &&
           "overwriting existing buffer");
  } else {
    PluginVariantFunctions.resize(index + 1);
  }
  PluginVariantFunctions[index] = static_cast<VariantFunctions *>(funcs);
}
