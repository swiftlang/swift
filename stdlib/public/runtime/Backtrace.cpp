//===--- Backtrace.cpp - Swift crash catching and backtracing support ---- ===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Crash catching and backtracing support routines.
//
//===----------------------------------------------------------------------===//

#include <type_traits>

#include "llvm/ADT/StringRef.h"

#include "swift/Runtime/Config.h"
#include "swift/Runtime/Backtrace.h"
#include "swift/Runtime/Debug.h"
#include "swift/Runtime/Paths.h"
#include "swift/Runtime/EnvironmentVariables.h"
#include "swift/Runtime/Win32.h"

#include "swift/Demangling/Demangler.h"

#ifdef __linux__
#include <sys/auxv.h>
#endif

#ifdef _WIN32
#include <windows.h>
#else
#if __has_include(<sys/mman.h>)
#include <sys/mman.h>
#define PROTECT_BACKTRACE_SETTINGS 1
#else
#define PROTECT_BACKTRACE_SETTINGS 0
#warning Backtracer settings will not be protected in this configuration.
#endif

#if TARGET_OS_OSX || TARGET_OS_MACCATALYST
#if __has_include(<sys/codesign.h>)
#include <sys/codesign.h>
#else
// SPI
#define CS_OPS_STATUS 0
#define CS_GET_TASK_ALLOW  0x00000004
#define CS_RUNTIME         0x00010000
#define CS_PLATFORM_BINARY 0x04000000
#define CS_PLATFORM_PATH   0x08000000
extern "C" int csops(int, unsigned int, void *, size_t);
#endif
#include <spawn.h>
#endif
#include <unistd.h>
#endif

#include <cstdlib>
#include <cstring>
#include <cerrno>

#ifdef _WIN32
// We'll probably want dbghelp.h here
#else
#include <cxxabi.h>
#endif

#include "BacktracePrivate.h"

#define DEBUG_BACKTRACING_SETTINGS 0

#ifndef lengthof
#define lengthof(x) (sizeof(x) / sizeof(x[0]))
#endif

using namespace swift::runtime::backtrace;

namespace swift {
namespace runtime {
namespace backtrace {

SWIFT_RUNTIME_STDLIB_INTERNAL BacktraceSettings _swift_backtraceSettings = {
  UnwindAlgorithm::Auto,

  // enabled
  OnOffTty::Default,

  // demangle
  true,

  // interactive
#if TARGET_OS_OSX || defined(__linux__) // || defined(_WIN32)
  OnOffTty::TTY,
#else
  OnOffTty::Off,
#endif

  // color
  OnOffTty::TTY,

  // timeout
  30,

  // threads
  ThreadsToShow::Preset,

  // registers
  RegistersToShow::Preset,

  // images
  ImagesToShow::Preset,

  // limit
  64,

  // top
  16,

  // sanitize,
  SanitizePaths::Preset,

  // preset
  Preset::Auto,

  // cache
  true,

  // outputTo,
  OutputTo::Auto,

  // symbolicate
  Symbolication::Full,

  // swiftBacktracePath
  NULL,
};

}
}
}

namespace {

class BacktraceInitializer {
public:
  BacktraceInitializer();
};

SWIFT_ALLOWED_RUNTIME_GLOBAL_CTOR_BEGIN

BacktraceInitializer backtraceInitializer;

SWIFT_ALLOWED_RUNTIME_GLOBAL_CTOR_END

#if SWIFT_BACKTRACE_ON_CRASH_SUPPORTED

// We need swiftBacktracePath to be aligned on a page boundary, and it also
// needs to be a multiple of the system page size.
#define SWIFT_BACKTRACE_BUFFER_SIZE 16384

static_assert((SWIFT_BACKTRACE_BUFFER_SIZE % SWIFT_PAGE_SIZE) == 0,
              "The backtrace path buffer must be a multiple of the system "
              "page size.  If it isn't, you'll get weird crashes in other "
              "code because we'll protect more than just the buffer.");

// The same goes for swiftBacktraceEnvironment
#define SWIFT_BACKTRACE_ENVIRONMENT_SIZE 32768

static_assert((SWIFT_BACKTRACE_ENVIRONMENT_SIZE % SWIFT_PAGE_SIZE) == 0,
              "The environment buffer must be a multiple of the system "
              "page size.  If it isn't, you'll get weird crashes in other "
              "code because we'll protect more than just the buffer.");

#if _WIN32
#pragma section(SWIFT_BACKTRACE_SECTION, read, write)

#if defined(SWIFT_RUNTIME_FIXED_BACKTRACER_PATH)
const WCHAR swiftBacktracePath[] = L"" SWIFT_RUNTIME_FIXED_BACKTRACER_PATH;
#else
__declspec(allocate(SWIFT_BACKTRACE_SECTION)) WCHAR swiftBacktracePath[SWIFT_BACKTRACE_BUFFER_SIZE];
#endif // !defined(SWIFT_RUNTIME_FIXED_BACKTRACER_PATH)

__declspec(allocate(SWIFT_BACKTRACE_SECTION)) CHAR swiftBacktraceEnv[SWIFT_BACKTRACE_ENVIRONMENT_SIZE];

#elif defined(__linux__) || TARGET_OS_OSX

#if defined(SWIFT_RUNTIME_FIXED_BACKTRACER_PATH)
const char swiftBacktracePath[] = SWIFT_RUNTIME_FIXED_BACKTRACER_PATH;
#else
char swiftBacktracePath[SWIFT_BACKTRACE_BUFFER_SIZE] __attribute__((section(SWIFT_BACKTRACE_SECTION), aligned(SWIFT_PAGE_SIZE)));
#endif // !defined(SWIFT_RUNTIME_FIXED_BACKTRACER_PATH

char swiftBacktraceEnv[SWIFT_BACKTRACE_ENVIRONMENT_SIZE] __attribute__((section(SWIFT_BACKTRACE_SECTION), aligned(SWIFT_PAGE_SIZE)));
#endif // defined(__linux__) || TARGET_OS_OSX

void _swift_backtraceSetupEnvironment();

bool isStdoutATty()
{
#ifndef _WIN32
  return isatty(STDOUT_FILENO);
#else
  DWORD dwMode;
  return GetConsoleMode(GetStdHandle(STD_OUTPUT_HANDLE), &dwMode);
#endif
}

bool isStdinATty()
{
#ifndef _WIN32
  return isatty(STDIN_FILENO);
#else
  DWORD dwMode;
  return GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), &dwMode);
#endif
}

#endif // SWIFT_BACKTRACE_ON_CRASH_SUPPORTED

void _swift_processBacktracingSetting(llvm::StringRef key, llvm::StringRef value);
void _swift_parseBacktracingSettings(const char *);

#if DEBUG_BACKTRACING_SETTINGS
const char *algorithmToString(UnwindAlgorithm algorithm) {
  switch (algorithm) {
  case UnwindAlgorithm::Auto: return "Auto";
  case UnwindAlgorithm::Fast: return "Fast";
  case UnwindAlgorithm::Precise: return "Precise";
  }
}

const char *onOffTtyToString(OnOffTty oot) {
  switch (oot) {
  case OnOffTty::Default: return "Default";
  case OnOffTty::On: return "On";
  case OnOffTty::Off: return "Off";
  case OnOffTty::TTY: return "TTY";
  }
}

const char *boolToString(bool b) {
  return b ? "true" : "false";
}

const char *presetToString(Preset preset) {
  switch (preset) {
  case Preset::Auto: return "Auto";
  case Preset::Friendly: return "Friendly";
  case Preset::Medium: return "Medium";
  case Preset::Full: return Full;
  }
}
#endif

#ifdef __linux__
bool isPrivileged() {
  return getauxval(AT_SECURE);
}
#elif TARGET_OS_OSX || TARGET_OS_MACCATALYST
bool isPrivileged() {
  if (issetugid())
    return true;

  uint32_t flags = 0;
  if (csops(getpid(),
            CS_OPS_STATUS,
            &flags,
            sizeof(flags)) != 0)
    return true;

  if (flags & (CS_PLATFORM_BINARY | CS_PLATFORM_PATH | CS_RUNTIME))
    return true;

  return !(flags & CS_GET_TASK_ALLOW);
}
#elif defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__)
bool isPrivileged() {
  return issetugid();
}
#elif _WIN32
bool isPrivileged() {
  return false;
}
#endif

} // namespace

BacktraceInitializer::BacktraceInitializer() {
  const char *backtracing = swift::runtime::environment::SWIFT_BACKTRACE();

  // Force off for setuid processes.
  if (isPrivileged()) {
    _swift_backtraceSettings.enabled = OnOffTty::Off;
  }

  if (backtracing)
    _swift_parseBacktracingSettings(backtracing);

#if !defined(SWIFT_RUNTIME_FIXED_BACKTRACER_PATH)
  if (!_swift_backtraceSettings.swiftBacktracePath) {
    _swift_backtraceSettings.swiftBacktracePath
      = swift_copyAuxiliaryExecutablePath("swift-backtrace");

    if (!_swift_backtraceSettings.swiftBacktracePath) {
      if (_swift_backtraceSettings.enabled == OnOffTty::On) {
        swift::warning(0,
                       "swift runtime: unable to locate swift-backtrace; "
                       "disabling backtracing.\n");
      }
      _swift_backtraceSettings.enabled = OnOffTty::Off;
    }
  }
#endif

  if (_swift_backtraceSettings.enabled == OnOffTty::Default) {
#if TARGET_OS_OSX
    _swift_backtraceSettings.enabled = OnOffTty::TTY;
#elif defined(__linux__) // || defined(_WIN32)
    _swift_backtraceSettings.enabled = OnOffTty::On;
#else
    _swift_backtraceSettings.enabled = OnOffTty::Off;
#endif
  }

#if !SWIFT_BACKTRACE_ON_CRASH_SUPPORTED
  if (_swift_backtraceSettings.enabled != OnOffTty::Off) {
    swift::warning(0,
                   "swift runtime: backtrace-on-crash is not supported on "
                   "this platform.\n");
    _swift_backtraceSettings.enabled = OnOffTty::Off;
  }
#else

  if (isPrivileged() && _swift_backtraceSettings.enabled != OnOffTty::Off) {
    // You'll only see this warning if you do e.g.
    //
    //    SWIFT_BACKTRACE=enable=on /path/to/some/setuid/binary
    //
    // as opposed to
    //
    //    /path/to/some/setuid/binary
    //
    // i.e. when you're trying to force matters.
    swift::warning(0,
                   "swift runtime: backtrace-on-crash is not supported for "
                   "privileged executables.\n");
    _swift_backtraceSettings.enabled = OnOffTty::Off;
  }

  if (_swift_backtraceSettings.enabled == OnOffTty::TTY)
    _swift_backtraceSettings.enabled =
      isStdoutATty() ? OnOffTty::On : OnOffTty::Off;

  if (_swift_backtraceSettings.interactive == OnOffTty::TTY) {
    _swift_backtraceSettings.interactive =
      (isStdoutATty() && isStdinATty()) ? OnOffTty::On : OnOffTty::Off;
  }

  if (_swift_backtraceSettings.color == OnOffTty::TTY)
    _swift_backtraceSettings.color =
      isStdoutATty() ? OnOffTty::On : OnOffTty::Off;

  if (_swift_backtraceSettings.preset == Preset::Auto) {
    if (_swift_backtraceSettings.interactive == OnOffTty::On)
      _swift_backtraceSettings.preset = Preset::Friendly;
    else
      _swift_backtraceSettings.preset = Preset::Full;
  }

  if (_swift_backtraceSettings.outputTo == OutputTo::Auto) {
    if (_swift_backtraceSettings.interactive == OnOffTty::On)
      _swift_backtraceSettings.outputTo = OutputTo::Stdout;
    else
      _swift_backtraceSettings.outputTo = OutputTo::Stderr;
  }

  if (_swift_backtraceSettings.enabled == OnOffTty::On) {
    // Copy the path to swift-backtrace into swiftBacktracePath, then write
    // protect it so that it can't be overwritten easily at runtime.  We do
    // this to avoid creating a massive security hole that would allow an
    // attacker to overwrite the path and then cause a crash to get us to
    // execute an arbitrary file.

#if _WIN32
    if (_swift_backtraceSettings.algorithm == UnwindAlgorithm::Auto)
      _swift_backtraceSettings.algorithm = UnwindAlgorithm::Precise;

#if !defined(SWIFT_RUNTIME_FIXED_BACKTRACER_PATH)
    int len = ::MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS,
                                    _swift_backtraceSettings.swiftBacktracePath, -1,
                                    swiftBacktracePath,
                                    SWIFT_BACKTRACE_BUFFER_SIZE);
    if (!len) {
      swift::warning(0,
                     "swift runtime: unable to convert path to "
                     "swift-backtrace: %08lx; disabling backtracing.\n",
                     ::GetLastError());
      _swift_backtraceSettings.enabled = OnOffTty::Off;
    } else if (!VirtualProtect(swiftBacktracePath,
                               sizeof(swiftBacktracePath),
                               PAGE_READONLY,
                               NULL)) {
      swift::warning(0,
                     "swift runtime: unable to protect path to "
                     "swift-backtrace: %08lx; disabling backtracing.\n",
                     ::GetLastError());
      _swift_backtraceSettings.enabled = OnOffTty::Off;
    }
#endif // !defined(SWIFT_RUNTIME_FIXED_BACKTRACER_PATH)

    _swift_backtraceSetupEnvironment();

    if (!VirtualProtect(swiftBacktraceEnv,
                        sizeof(swiftBacktraceEnv),
                        PAGE_READONLY,
                        NULL)) {
      swift::warning(0,
                     "swift runtime: unable to protect environment "
                     "for swift-backtrace: %08lx; disabling backtracing.\n",
                     ::GetLastError());
      _swift_backtraceSettings.enabled = OnOffTty::Off;
    }
#else
    if (_swift_backtraceSettings.algorithm == UnwindAlgorithm::Auto)
      _swift_backtraceSettings.algorithm = UnwindAlgorithm::Precise;

#if !defined(SWIFT_RUNTIME_FIXED_BACKTRACER_PATH)
    size_t len = strlen(_swift_backtraceSettings.swiftBacktracePath);
    if (len > SWIFT_BACKTRACE_BUFFER_SIZE - 1) {
      swift::warning(0,
                     "swift runtime: path to swift-backtrace is too long; "
                     "disabling backtracing.\n");
      _swift_backtraceSettings.enabled = OnOffTty::Off;
    } else {
      memcpy(swiftBacktracePath,
             _swift_backtraceSettings.swiftBacktracePath,
             len + 1);

#if PROTECT_BACKTRACE_SETTINGS
      if (mprotect(swiftBacktracePath,
                   sizeof(swiftBacktracePath),
                   PROT_READ) < 0) {
        swift::warning(0,
                       "swift runtime: unable to protect path to "
                       "swift-backtrace at %p: %d; disabling backtracing.\n",
                       swiftBacktracePath,
                       errno);
        _swift_backtraceSettings.enabled = OnOffTty::Off;
      }
#endif // PROTECT_BACKTRACE_SETTINGS
    }
#endif // !defined(SWIFT_RUNTIME_FIXED_BACKTRACER_PATH)

    _swift_backtraceSetupEnvironment();

#if PROTECT_BACKTRACE_SETTINGS
    if (mprotect(swiftBacktraceEnv,
                 sizeof(swiftBacktraceEnv),
                 PROT_READ) < 0) {
        swift::warning(0,
                       "swift runtime: unable to protect environment for "
                       "swift-backtrace at %p: %d; disabling backtracing.\n",
                       swiftBacktraceEnv,
                       errno);
        _swift_backtraceSettings.enabled = OnOffTty::Off;
    }
#endif // PROTECT_BACKTRACE_SETTINGS

#endif // !_WIN32
  }

  if (_swift_backtraceSettings.enabled == OnOffTty::On) {
    ErrorCode err = _swift_installCrashHandler();
    if (err != 0) {
      swift::warning(0,
                     "swift runtime: crash handler installation failed; "
                     "disabling backtracing.\n");
    }
  }
#endif

#if DEBUG_BACKTRACING_SETTINGS
  printf("\nBACKTRACING SETTINGS\n"
         "\n"
         "algorithm: %s\n"
         "enabled: %s\n"
         "demangle: %s\n"
         "interactive: %s\n"
         "color: %s\n"
         "timeout: %u\n"
         "preset: %s\n"
         "swiftBacktracePath: %s\n",
         algorithmToString(_swift_backtraceSettings.algorithm),
         onOffTtyToString(_swift_backtraceSettings.enabled),
         boolToString(_swift_backtraceSettings.demangle),
         onOffTtyToString(_swift_backtraceSettings.interactive),
         onOffTtyToString(_swift_backtraceSettings.color),
         _swift_backtraceSettings.timeout,
         presetToString(_swift_backtraceSettings.preset),
         swiftBacktracePath);

  printf("\nBACKTRACING ENV\n");

  const char *ptr = swiftBacktraceEnv;
  while (*ptr) {
    size_t len = std::strlen(ptr);
    printf("%s\n", ptr);
    ptr += len + 1;
  }
  printf("\n");
#endif
}

namespace {

OnOffTty
parseOnOffTty(llvm::StringRef value)
{
  if (value.equals_insensitive("on")
      || value.equals_insensitive("true")
      || value.equals_insensitive("yes")
      || value.equals_insensitive("y")
      || value.equals_insensitive("t")
      || value.equals_insensitive("1"))
    return OnOffTty::On;
  if (value.equals_insensitive("tty")
      || value.equals_insensitive("auto"))
    return OnOffTty::TTY;
  return OnOffTty::Off;
}

bool
parseBoolean(llvm::StringRef value)
{
  return (value.equals_insensitive("on")
          || value.equals_insensitive("true")
          || value.equals_insensitive("yes")
          || value.equals_insensitive("y")
          || value.equals_insensitive("t")
          || value.equals_insensitive("1"));
}

Symbolication
parseSymbolication(llvm::StringRef value)
{
  if (value.equals_insensitive("on")
      || value.equals_insensitive("true")
      || value.equals_insensitive("yes")
      || value.equals_insensitive("y")
      || value.equals_insensitive("t")
      || value.equals_insensitive("1")
      || value.equals_insensitive("full"))
    return Symbolication::Full;
  if (value.equals_insensitive("fast"))
    return Symbolication::Fast;
  return Symbolication::Off;
}

void
_swift_processBacktracingSetting(llvm::StringRef key,
                                 llvm::StringRef value)

{
  if (key.equals_insensitive("enable")) {
    _swift_backtraceSettings.enabled = parseOnOffTty(value);
  } else if (key.equals_insensitive("demangle")) {
    _swift_backtraceSettings.demangle = parseBoolean(value);
  } else if (key.equals_insensitive("interactive")) {
    _swift_backtraceSettings.interactive = parseOnOffTty(value);
  } else if (key.equals_insensitive("color")) {
    _swift_backtraceSettings.color = parseOnOffTty(value);
  } else if (key.equals_insensitive("timeout")) {
    int count;
    llvm::StringRef valueCopy = value;

    if (value.equals_insensitive("none")) {
      _swift_backtraceSettings.timeout = 0;
    } else if (!valueCopy.consumeInteger(0, count)) {
      // Yes, consumeInteger() really does return *false* for success
      llvm::StringRef unit = valueCopy.trim();

      if (unit.empty()
          || unit.equals_insensitive("s")
          || unit.equals_insensitive("seconds"))
        _swift_backtraceSettings.timeout = count;
      else if (unit.equals_insensitive("m")
               || unit.equals_insensitive("minutes"))
        _swift_backtraceSettings.timeout = count * 60;
      else if (unit.equals_insensitive("h")
               || unit.equals_insensitive("hours"))
        _swift_backtraceSettings.timeout = count * 3600;

      if (_swift_backtraceSettings.timeout < 0) {
        swift::warning(0,
                       "swift runtime: bad backtracing timeout %ds\n",
                       _swift_backtraceSettings.timeout);
        _swift_backtraceSettings.timeout = 0;
      }
    } else {
      swift::warning(0,
                     "swift runtime: bad backtracing timeout '%.*s'\n",
                     static_cast<int>(value.size()), value.data());
    }
  } else if (key.equals_insensitive("unwind")) {
    if (value.equals_insensitive("auto"))
      _swift_backtraceSettings.algorithm = UnwindAlgorithm::Auto;
    else if (value.equals_insensitive("fast"))
      _swift_backtraceSettings.algorithm = UnwindAlgorithm::Fast;
    else if (value.equals_insensitive("precise"))
      _swift_backtraceSettings.algorithm = UnwindAlgorithm::Precise;
    else {
      swift::warning(0,
                     "swift runtime: unknown unwind algorithm '%.*s'\n",
                     static_cast<int>(value.size()), value.data());
    }
  } else if (key.equals_insensitive("sanitize")) {
    _swift_backtraceSettings.sanitize
      = parseBoolean(value) ? SanitizePaths::On : SanitizePaths::Off;
  } else if (key.equals_insensitive("preset")) {
    if (value.equals_insensitive("auto"))
      _swift_backtraceSettings.preset = Preset::Auto;
    else if (value.equals_insensitive("friendly"))
      _swift_backtraceSettings.preset = Preset::Friendly;
    else if (value.equals_insensitive("medium"))
      _swift_backtraceSettings.preset = Preset::Medium;
    else if (value.equals_insensitive("full"))
      _swift_backtraceSettings.preset = Preset::Full;
    else {
      swift::warning(0,
                     "swift runtime: unknown backtracing preset '%.*s'\n",
                     static_cast<int>(value.size()), value.data());
    }
  } else if (key.equals_insensitive("threads")) {
    if (value.equals_insensitive("all"))
      _swift_backtraceSettings.threads = ThreadsToShow::All;
    else if (value.equals_insensitive("crashed"))
      _swift_backtraceSettings.threads = ThreadsToShow::Crashed;
    else {
      swift::warning(0,
                     "swift runtime: unknown threads setting '%.*s'\n",
                     static_cast<int>(value.size()), value.data());
    }
  } else if (key.equals_insensitive("registers")) {
    if (value.equals_insensitive("none"))
      _swift_backtraceSettings.registers = RegistersToShow::None;
    else if (value.equals_insensitive("all"))
      _swift_backtraceSettings.registers = RegistersToShow::All;
    else if (value.equals_insensitive("crashed"))
      _swift_backtraceSettings.registers = RegistersToShow::Crashed;
    else {
      swift::warning(0,
                     "swift runtime: unknown registers setting '%.*s'\n",
                     static_cast<int>(value.size()), value.data());
    }
  } else if (key.equals_insensitive("images")) {
    if (value.equals_insensitive("none"))
      _swift_backtraceSettings.images = ImagesToShow::None;
    else if (value.equals_insensitive("all"))
      _swift_backtraceSettings.images = ImagesToShow::All;
    else if (value.equals_insensitive("mentioned"))
      _swift_backtraceSettings.images = ImagesToShow::Mentioned;
    else {
      swift::warning(0,
                     "swift runtime: unknown registers setting '%.*s'\n",
                     static_cast<int>(value.size()), value.data());
    }
  } else if (key.equals_insensitive("limit")) {
    int limit;
    // Yes, getAsInteger() returns false for success.
    if (value.equals_insensitive("none"))
      _swift_backtraceSettings.limit = -1;
    else if (!value.getAsInteger(0, limit) && limit > 0)
      _swift_backtraceSettings.limit = limit;
    else {
      swift::warning(0,
                     "swift runtime: bad backtrace limit '%.*s'\n",
                     static_cast<int>(value.size()), value.data());
    }
  } else if (key.equals_insensitive("top")) {
    int top;
    // (If you think the next line is wrong, see above.)
    if (!value.getAsInteger(0, top) && top >= 0)
      _swift_backtraceSettings.top = top;
    else {
       swift::warning(0,
                     "swift runtime: bad backtrace top count '%.*s'\n",
                     static_cast<int>(value.size()), value.data());
    }
  } else if (key.equals_insensitive("cache")) {
    _swift_backtraceSettings.cache = parseBoolean(value);
  } else if (key.equals_insensitive("output-to")) {
    if (value.equals_insensitive("auto"))
      _swift_backtraceSettings.outputTo = OutputTo::Auto;
    else if (value.equals_insensitive("stdout"))
      _swift_backtraceSettings.outputTo = OutputTo::Stdout;
    else if (value.equals_insensitive("stderr"))
      _swift_backtraceSettings.outputTo = OutputTo::Stderr;
    else {
      swift::warning(0,
                     "swift runtime: unknown output-to setting '%.*s'\n",
                     static_cast<int>(value.size()), value.data());
    }
  } else if (key.equals_insensitive("symbolicate")) {
    _swift_backtraceSettings.symbolicate = parseSymbolication(value);
#if !defined(SWIFT_RUNTIME_FIXED_BACKTRACER_PATH)
  } else if (key.equals_insensitive("swift-backtrace")) {
    size_t len = value.size();
    char *path = (char *)std::malloc(len + 1);
    std::copy(value.begin(), value.end(), path);
    path[len] = 0;

    std::free(const_cast<char *>(_swift_backtraceSettings.swiftBacktracePath));
    _swift_backtraceSettings.swiftBacktracePath = path;
#endif // !defined(SWIFT_RUNTIME_FIXED_BACKTRACER_PATH)
  } else {
    swift::warning(0,
                   "swift runtime: unknown backtracing setting '%.*s'\n",
                   static_cast<int>(key.size()), key.data());
  }
}

void
_swift_parseBacktracingSettings(const char *settings)
{
  const char *ptr = settings;
  const char *key = ptr;
  const char *keyEnd;
  const char *value;
  const char *valueEnd;
  enum {
    ScanningKey,
    ScanningValue
  } state = ScanningKey;
  int ch;

  while ((ch = *ptr++)) {
    switch (state) {
    case ScanningKey:
      if (ch == '=') {
        keyEnd = ptr - 1;
        value = ptr;
        state = ScanningValue;
        continue;
      }
      break;
    case ScanningValue:
      if (ch == ',') {
        valueEnd = ptr - 1;

        _swift_processBacktracingSetting(llvm::StringRef(key, keyEnd - key),
                                         llvm::StringRef(value,
                                                         valueEnd - value));

        key = ptr;
        state = ScanningKey;
        continue;
      }
      break;
    }
  }

  if (state == ScanningValue) {
    valueEnd = ptr - 1;
    _swift_processBacktracingSetting(llvm::StringRef(key, keyEnd - key),
                                     llvm::StringRef(value,
                                                     valueEnd - value));
  }
}

#if SWIFT_BACKTRACE_ON_CRASH_SUPPORTED
// These are the only environment variables that are passed through to
// the swift-backtrace process.  They're copied at program start, and then
// write protected so they can't be manipulated by an attacker using a buffer
// overrun.
const char * const environmentVarsToPassThrough[] = {
  "LD_LIBRARY_PATH",
  "DYLD_LIBRARY_PATH",
  "DYLD_FRAMEWORK_PATH",
  "PATH",
  "TERM",
  "LANG",
  "HOME"
};

#define BACKTRACE_MAX_ENV_VARS lengthof(environmentVarsToPassThrough)

void
_swift_backtraceSetupEnvironment()
{
  size_t remaining = sizeof(swiftBacktraceEnv);
  char *penv = swiftBacktraceEnv;

  std::memset(swiftBacktraceEnv, 0, sizeof(swiftBacktraceEnv));

  // We definitely don't want this on in the swift-backtrace program
  const char * const disable = "SWIFT_BACKTRACE=enable=no";
  const size_t disableLen = std::strlen(disable) + 1;
  std::memcpy(penv, disable, disableLen);
  penv += disableLen;
  remaining -= disableLen;

  for (unsigned n = 0; n < BACKTRACE_MAX_ENV_VARS; ++n) {
    const char *name = environmentVarsToPassThrough[n];
    const char *value = getenv(name);
    if (!value)
      continue;

    size_t nameLen = std::strlen(name);
    size_t valueLen = std::strlen(value);
    size_t totalLen = nameLen + 1 + valueLen + 1;

    if (remaining > totalLen) {
      std::memcpy(penv, name, nameLen);
      penv += nameLen;
      *penv++ = '=';
      std::memcpy(penv, value, valueLen);
      penv += valueLen;
      *penv++ = 0;

      remaining -= totalLen;
    }
  }

  *penv = 0;
}

#ifdef __linux__
struct spawn_info {
  const char *path;
  char * const *argv;
  char * const *envp;
  int memserver;
};

uint8_t spawn_stack[4096] __attribute__((aligned(SWIFT_PAGE_SIZE)));

int
do_spawn(void *ptr) {
  struct spawn_info *pinfo = (struct spawn_info *)ptr;

  /* Ensure that the memory server is always on fd 4 */
  if (pinfo->memserver != 4) {
    dup2(pinfo->memserver, 4);
    close(pinfo->memserver);
  }

  /* Clear the signal mask */
  sigset_t mask;
  sigfillset(&mask);
  sigprocmask(SIG_UNBLOCK, &mask, NULL);

  return execvpe(pinfo->path, pinfo->argv, pinfo->envp);
}

int
safe_spawn(pid_t *ppid, const char *path, int memserver,
           char * const argv[], char * const envp[])
{
  struct spawn_info info = { path, argv, envp, memserver };

  /* The CLONE_VFORK is *required* because info is on the stack; we don't
     want to return until *after* the subprocess has called execvpe(). */
  int ret = clone(do_spawn, spawn_stack + sizeof(spawn_stack),
                  CLONE_VFORK|CLONE_VM, &info);
  if (ret < 0)
    return ret;

  close(memserver);

  *ppid = ret;
  return 0;
}
#endif // defined(__linux__)

#endif // SWIFT_BACKTRACE_ON_CRASH_SUPPORTED

} // namespace

namespace swift {
namespace runtime {
namespace backtrace {

/// Test if a Swift symbol name represents a thunk function.
///
/// In backtraces, it is often desirable to omit thunk frames as they usually
/// just clutter up the backtrace unnecessarily.
///
/// @param mangledName is the symbol name to be tested.
///
/// @returns `true` if `mangledName` represents a thunk function.
SWIFT_RUNTIME_STDLIB_SPI bool
_swift_backtrace_isThunkFunction(const char *mangledName) {
  swift::Demangle::Context ctx;

  return ctx.isThunkSymbol(mangledName);
}

// Try to demangle a symbol.
SWIFT_RUNTIME_STDLIB_SPI char *
_swift_backtrace_demangle(const char *mangledName,
                          size_t mangledNameLength,
                          char *outputBuffer,
                          size_t *outputBufferSize) {
  llvm::StringRef name = llvm::StringRef(mangledName, mangledNameLength);

  // You must provide buffer size if you're providing your own output buffer
  if (outputBuffer && !outputBufferSize) {
    return nullptr;
  }

  if (Demangle::isSwiftSymbol(name)) {
    // This is a Swift mangling
    auto options = DemangleOptions::SimplifiedUIDemangleOptions();
    auto result = Demangle::demangleSymbolAsString(name, options);
    size_t bufferSize;

    if (outputBufferSize) {
      bufferSize = *outputBufferSize;
      *outputBufferSize = result.length() + 1;
    }

    if (outputBuffer == nullptr) {
      outputBuffer = (char *)::malloc(result.length() + 1);
      bufferSize = result.length() + 1;
    }

    size_t toCopy = std::min(bufferSize - 1, result.length());
    ::memcpy(outputBuffer, result.data(), toCopy);
    outputBuffer[toCopy] = '\0';

    return outputBuffer;
#ifndef _WIN32
  } else if (name.starts_with("_Z")) {
    // Try C++; note that we don't want to force callers to use malloc() to
    // allocate their buffer, which is a requirement for __cxa_demangle
    // because it may call realloc() on the incoming pointer.  As a result,
    // we never pass the caller's buffer to __cxa_demangle.
    size_t resultLen;
    int status = 0;
    char *result = abi::__cxa_demangle(mangledName, nullptr, &resultLen, &status);

    if (result) {
      size_t bufferSize;

      if (outputBufferSize) {
        bufferSize = *outputBufferSize;
        *outputBufferSize = resultLen;
      }

      if (outputBuffer == nullptr) {
        return result;
      }

      size_t toCopy = std::min(bufferSize - 1, resultLen - 1);
      ::memcpy(outputBuffer, result, toCopy);
      outputBuffer[toCopy] = '\0';

      free(result);

      return outputBuffer;
    }
#else
    // On Windows, the mangling is different.
    // ###TODO: Call __unDName()
#endif
  }

  return nullptr;
}

// N.B. THIS FUNCTION MUST BE SAFE TO USE FROM A CRASH HANDLER.  On Linux
// and macOS, that means it must be async-signal-safe.  On Windows, there
// isn't an equivalent notion but a similar restriction applies.
SWIFT_RUNTIME_STDLIB_INTERNAL bool
#ifdef __linux__
_swift_spawnBacktracer(const ArgChar * const *argv, int memserver_fd)
#else
_swift_spawnBacktracer(const ArgChar * const *argv)
#endif
{
#if !SWIFT_BACKTRACE_ON_CRASH_SUPPORTED
  return false;
#elif TARGET_OS_OSX || TARGET_OS_MACCATALYST || defined(__linux__)
  pid_t child;
  const char *env[BACKTRACE_MAX_ENV_VARS + 1];

  // Set-up the environment array
  const char *ptr = swiftBacktraceEnv;
  unsigned nEnv = 0;
  while (*ptr && nEnv < lengthof(env) - 1) {
    env[nEnv++] = ptr;
    ptr += std::strlen(ptr) + 1;
  };
  env[nEnv] = 0;

  // SUSv3 says argv and envp are "completely constant" and that the reason
  // posix_spawn() et al use char * const * is for compatibility.
#ifdef __linux__
  int ret = safe_spawn(&child, swiftBacktracePath, memserver_fd,
                       const_cast<char * const *>(argv),
                       const_cast<char * const *>(env));
#else
  int ret = posix_spawn(&child, swiftBacktracePath,
                        nullptr, nullptr,
                        const_cast<char * const *>(argv),
                        const_cast<char * const *>(env));
#endif
  if (ret < 0)
    return false;

  int wstatus;

  do {
    ret = waitpid(child, &wstatus, 0);
  } while (ret < 0 && errno == EINTR);

  if (WIFEXITED(wstatus))
    return WEXITSTATUS(wstatus) == 0;

  return false;

  // ###TODO: Windows
#endif
}

// N.B. THIS FUNCTION MUST BE SAFE TO USE FROM A CRASH HANDLER.  On Linux
// and macOS, that means it must be async-signal-safe.  On Windows, there
// isn't an equivalent notion but a similar restriction applies.
SWIFT_RUNTIME_STDLIB_INTERNAL void
_swift_displayCrashMessage(int signum, const void *pc)
{
#if !SWIFT_BACKTRACE_ON_CRASH_SUPPORTED
  return;
#else
  int fd = STDOUT_FILENO;

  if (_swift_backtraceSettings.outputTo == OutputTo::Stderr)
    fd = STDERR_FILENO;

  const char *intro;
  if (_swift_backtraceSettings.color == OnOffTty::On) {
    intro = "\n💣 \033[91mProgram crashed: ";
  } else {
    intro = "\n*** ";
  }
  write(fd, intro, strlen(intro));

  char sigbuf[30];
  strcpy(sigbuf, "Signal ");
  _swift_formatUnsigned((unsigned)signum, sigbuf + 7);
  write(fd, sigbuf, strlen(sigbuf));

  const char *message;
  if (!pc) {
    message = ": Backtracing";
  } else {
    message = ": Backtracing from 0x";
  }
  write(fd, message, strlen(message));

  if (pc) {
    char pcbuf[18];
    _swift_formatAddress(pc, pcbuf);
    write(fd, pcbuf, strlen(pcbuf));
  }

  const char *outro;
  if (_swift_backtraceSettings.color == OnOffTty::On) {
    outro = "...\033[0m";
  } else {
    outro = "...";
  }
  write(fd, outro, strlen(outro));
#endif
}

} // namespace backtrace
} // namespace runtime
} // namespace swift
