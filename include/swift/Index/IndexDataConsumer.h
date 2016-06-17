//===--- IndexDataConsumer.h - Consumer of indexing information -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_INDEX_INDEXDATACONSUMER_H
#define SWIFT_INDEX_INDEXDATACONSUMER_H

#include "swift/Index/IndexSymbol.h"

namespace swift {
namespace index {

class IndexDataConsumer {
  virtual void anchor();

public:
  virtual ~IndexDataConsumer() {}

  virtual void failed(StringRef error) = 0;
  virtual void warning(StringRef warning) {}
  virtual bool enableWarnings() { return false; }

  virtual bool recordHash(StringRef hash, bool isKnown) = 0;
  virtual bool startDependency(SymbolKind kind, StringRef name, StringRef path,
                               bool isSystem, StringRef hash) = 0;
  virtual bool finishDependency(SymbolKind kind) = 0;
  virtual bool startSourceEntity(const IndexSymbol &symbol) = 0;
  virtual bool recordRelatedEntity(const IndexSymbol &symbol) = 0;
  virtual bool finishSourceEntity(SymbolKind kind, SymbolSubKindSet subKinds,
                                  SymbolRoleSet roles) = 0;

  virtual void finish() {}
};

} // end namespace index
} // end namespace swift

#endif // SWIFT_INDEX_INDEXDATACONSUMER_H
