//===--- LocalizationFormat.h - YAML format for Diagnostic Messages ---*-
// C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the format for localized diagnostic messages.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_LOCALIZATIONFORMAT_H
#define SWIFT_LOCALIZATIONFORMAT_H

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/Support/YAMLTraits.h"
#include <string>
#include <type_traits>

namespace swift {
enum class DiagID : uint32_t;

namespace diag {
class LocalizationProducer {
public:
  /// If the  message isn't available/localized in the current `yaml` file,
  /// return the fallback default message.
  virtual llvm::StringRef getMessageOr(swift::DiagID id,
                                       llvm::StringRef defaultMessage) const {
    return defaultMessage;
  }

  virtual ~LocalizationProducer() {}
};

class YAMLLocalizationProducer final : public LocalizationProducer {
public:
  std::vector<std::string> diagnostics;
  explicit YAMLLocalizationProducer(std::string locale, std::string path);
  llvm::StringRef getMessageOr(swift::DiagID id,
                               llvm::StringRef defaultMessage) const override;
};

class LocalizationInput : public llvm::yaml::Input {
  using Input::Input;

  /// Read diagnostics in the YAML file iteratively
  template <typename T, typename Context>
  friend typename std::enable_if<llvm::yaml::has_SequenceTraits<T>::value,
                                 void>::type
  readYAML(llvm::yaml::IO &io, T &Seq, bool, Context &Ctx);

  template <typename T>
  friend typename std::enable_if<llvm::yaml::has_SequenceTraits<T>::value,
                                 LocalizationInput &>::type
  operator>>(LocalizationInput &yin, T &diagnostics);
};

} // namespace diag
} // namespace swift

#endif
