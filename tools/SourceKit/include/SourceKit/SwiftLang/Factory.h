//===--- Factory.h - --------------------------------------------*- C++ -*-===//
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

#ifndef LLVM_SOURCEKIT_SWIFTLANG_FACTORY_H
#define LLVM_SOURCEKIT_SWIFTLANG_FACTORY_H

#include <memory>

namespace SourceKit {
class LangSupport;
class Context;

std::unique_ptr<LangSupport> createSwiftLangSupport(Context &SKCtx);

}

#endif
