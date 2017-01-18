//===--- Util.h - Common Driver Utilities -----------------------*- C++ -*-===//
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

#ifndef SWIFT_DRIVER_UTIL_H
#define SWIFT_DRIVER_UTIL_H

#include "swift/Driver/Types.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/SmallVector.h"

namespace llvm {
namespace opt {
  class Arg;
} // end namespace opt
} // end namespace llvm

namespace swift {

namespace driver {
  class Action;

  /// Type used for list of Actions.
  typedef SmallVector<Action *, 3> ActionList;

  /// An input argument from the command line and its inferred type.
  typedef std::pair<types::ID, const llvm::opt::Arg *> InputPair;
  /// Type used for a list of input arguments.
  typedef SmallVector<InputPair, 16> InputFileList;

  enum class LinkKind {
    None,
    Executable,
    DynamicLibrary
  };

  /// Used by a Job to request a "filelist": a file containing a list of all
  /// input or output files of a certain type.
  ///
  /// The Compilation is responsible for generating this file before running
  /// the Job this info is attached to.
  struct FilelistInfo {
    enum WhichFiles : bool {
      Input,
      Output
    };

    StringRef path;
    types::ID type;
    WhichFiles whichFiles;
  };

} // end namespace driver
} // end namespace swift

#endif
