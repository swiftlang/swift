//===--- Logging.h - Logging Interface --------------------------*- C++ -*-===//
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

#ifndef LLVM_SOURCEKIT_SUPPORT_LOGGING_H
#define LLVM_SOURCEKIT_SUPPORT_LOGGING_H

#include "SourceKit/Core/LLVM.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/raw_ostream.h"
#include <string>

namespace llvm {
class format_object_base;
}

namespace SourceKit {
  class UIdent;
  class Logger;

typedef IntrusiveRefCntPtr<Logger> LogRef;

/// \brief Collects logging output and writes it to stderr when it's destructed.
/// Common use case:
/// \code
///   if (LogRef Log = Logger::make(__func__, Logger::Level::Warning)) {
///     *Log << "stuff";
///   }
/// \endcode
class Logger : public llvm::RefCountedBase<Logger> {
public:
  enum class Level : unsigned char {
    /// \brief No logging.
    None = 0,
    /// \brief Warning level.
    Warning = 1,
    /// \brief Information level for high priority messages.
    InfoHighPrio = 2,
    /// \brief Information level for medium priority messages.
    InfoMediumPrio = 3,
    /// \brief Information level for low priority messages.
    InfoLowPrio = 4
  };

private:
  std::string Name;
  Level CurrLevel;
  SmallString<64> Msg;
  llvm::raw_svector_ostream LogOS;

  static std::string LoggerName;
  static Level LoggingLevel;

public:
  static bool isLoggingEnabledForLevel(Level LogLevel) {
    return LoggingLevel >= LogLevel;
  }
  static void enableLogging(StringRef Name, Level LogLevel) {
    LoggerName = Name;
    LoggingLevel = LogLevel;
  }

  static LogRef make(llvm::StringRef Name, Level LogLevel) {
    if (isLoggingEnabledForLevel(LogLevel)) return new Logger(Name, LogLevel);
    return nullptr;
  }

  Logger(llvm::StringRef Name, Level LogLevel)
    : Name(Name), CurrLevel(LogLevel), LogOS(Msg) { }
  ~Logger();

  llvm::raw_ostream &getOS() { return LogOS; }

  Logger &operator<<(SourceKit::UIdent UID);
  Logger &operator<<(llvm::StringRef Str) { LogOS << Str; return *this; }
  Logger &operator<<(const char *Str) { if (Str) LogOS << Str; return *this; }
  Logger &operator<<(unsigned long N) { LogOS << N; return *this; }
  Logger &operator<<(long N) { LogOS << N ; return *this; }
  Logger &operator<<(unsigned N) { LogOS << N; return *this; }
  Logger &operator<<(int N) { LogOS << N; return *this; }
  Logger &operator<<(char C) { LogOS << C; return *this; }
  Logger &operator<<(unsigned char C) { LogOS << C; return *this; }
  Logger &operator<<(signed char C) { LogOS << C; return *this; }
  Logger &operator<<(const llvm::format_object_base &Fmt);
};

} // namespace SourceKit

/// \brief Macros to automate common uses of Logger. Like this:
/// \code
///   LOG_FUNC_SECTION_WARN {
///     *Log << "blah";
///   }
/// \endcode
#define LOG_SECTION(NAME, LEVEL) \
  if (LogRef Log = SourceKit::Logger::make(NAME, SourceKit::Logger::Level::LEVEL))
#define LOG_FUNC_SECTION(LEVEL) LOG_SECTION(__func__, LEVEL)
#define LOG_FUNC_SECTION_WARN LOG_FUNC_SECTION(Warning)

#define LOG(NAME, LEVEL, msg) LOG_SECTION(NAME, LEVEL) \
  do { *Log << msg; } while (0)
#define LOG_FUNC(LEVEL, msg) LOG_FUNC_SECTION(LEVEL) \
  do { *Log << msg; } while (0)
#define LOG_WARN(NAME, msg) LOG(NAME, Warning, msg)
#define LOG_WARN_FUNC(msg) LOG_FUNC(Warning, msg)
#define LOG_INFO_FUNC(PRIO, msg) LOG_FUNC(Info##PRIO##Prio, msg)
#define LOG_INFO(NAME, PRIO, msg) LOG(NAME, Info##PRIO##Prio, msg)

#endif
