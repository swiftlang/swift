//===--- ModuleLoadListener.h - Module Load Listener Interface ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file implements an abstract interface for listening for 'module
/// loaded' events.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_MODULE_LOAD_LISTENER
#define SWIFT_AST_MODULE_LOAD_LISTENER

namespace swift {
class ModuleLoader;
class Module;

/// \brief Interface for listeners that can respond to 'module loaded' events.
class ModuleLoadListener {
public:
  virtual ~ModuleLoadListener();

  /// \brief A new module was loaded.
  virtual void loadedModule(ModuleLoader *Loader, Module *M) = 0;
};

} // namespace swift

#endif // LLVM_SWIFT_AST_MODULE_LOAD_LISTENER

