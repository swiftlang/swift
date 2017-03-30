//===--- Index.h - Swift Indexing -------------------------------*- C++ -*-===//
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

#ifndef SWIFT_INDEX_INDEX_H
#define SWIFT_INDEX_INDEX_H

#include "swift/Index/IndexDataConsumer.h"

namespace swift {
class ModuleDecl;
class SourceFile;

namespace index {

void indexSourceFile(SourceFile *SF, StringRef hash,
                     IndexDataConsumer &consumer);
void indexModule(ModuleDecl *module, StringRef hash,
                 IndexDataConsumer &consumer);
bool shouldIndex(ValueDecl *D, bool IsRef);
} // end namespace index
} // end namespace swift

#endif // SWIFT_INDEX_INDEX_H
