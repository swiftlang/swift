//===--- Context.cpp ------------------------------------------------------===//
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

#include "SourceKit/Core/Context.h"
#include "SourceKit/Core/LangSupport.h"
#include "SourceKit/Core/NotificationCenter.h"

using namespace SourceKit;

SourceKit::Context::Context(StringRef RuntimeLibPath)
  : RuntimeLibPath(RuntimeLibPath) {
  NotificationCtr.reset(new NotificationCenter());
  SwiftLang = std::move(LangSupport::createSwiftLangSupport(*this));
}

SourceKit::Context::~Context() {
}
