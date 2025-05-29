//===--- PlaygroundOption.h - Playground Transform Options -----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_PLAYGROUND_OPTIONS_H
#define SWIFT_BASIC_PLAYGROUND_OPTIONS_H

#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/StringRef.h"
#include <optional>

namespace swift {

/// Enumeration describing all of the available playground options.
enum class PlaygroundOption {
#define PLAYGROUND_OPTION(OptionName, Description, DefaultOn, HighPerfOn) \
  OptionName,
#include "swift/Basic/PlaygroundOptions.def"
};

constexpr unsigned numPlaygroundOptions() {
  enum PlaygroundOptions {
#define PLAYGROUND_OPTION(OptionName, Description, DefaultOn, HighPerfOn) \
    OptionName,
#include "swift/Basic/PlaygroundOptions.def"
    NumPlaygroundOptions
  };
  return NumPlaygroundOptions;
}

/// Return the name of the given playground option.
llvm::StringRef getPlaygroundOptionName(PlaygroundOption option);

/// Get the playground option that corresponds to a given name, if there is one.
std::optional<PlaygroundOption> getPlaygroundOption(llvm::StringRef name);

/// Set of enabled playground options.
typedef llvm::SmallSet<PlaygroundOption, 8> PlaygroundOptionSet;

}

#endif // SWIFT_BASIC_PLAYGROUND_OPTIONS_H
