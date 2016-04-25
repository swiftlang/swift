//===--- swift-remoteast-test.cpp - RemoteAST testing application ---------===//
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
// This file supports performing target-specific remote reflection tests
// on live swift executables.
//===----------------------------------------------------------------------===//

#include "swift/RemoteAST/RemoteAST.h"
#include "swift/Remote/InProcessMemoryReader.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Frontend/Frontend.h"
#include "swift/FrontendTool/FrontendTool.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>

using namespace swift;
using namespace swift::remote;
using namespace swift::remoteAST;

/// The context for the code we're running.  Set by the observer.
static ASTContext *Context = nullptr;

// FIXME: swiftcall
/// func printType(forMetadata: Any.Type)
LLVM_ATTRIBUTE_USED
extern "C" void printMetadataType(const Metadata *typeMetadata) {
  assert(Context && "context was not set");

  std::shared_ptr<MemoryReader> reader(new InProcessMemoryReader());
  RemoteASTContext remoteAST(*Context, std::move(reader));

  auto &out = llvm::outs();

  auto result =
    remoteAST.getTypeForRemoteTypeMetadata(RemoteAddress(typeMetadata));
  if (result) {
    out << "found type: ";
    result.getValue().print(out);
    out << '\n';
  } else {
    out << result.getFailure().render() << '\n';
  }
}

namespace {

struct Observer : public FrontendObserver {
  void aboutToRunImmediately(CompilerInstance &instance) override {
    Context = &instance.getASTContext();
  }
};

}

int main(int argc, const char *argv[]) {
  unsigned numForwardedArgs = argc
      - 1  // we drop argv[0]
      + 1; // -interpret

  SmallVector<const char *, 8> forwardedArgs;
  forwardedArgs.reserve(numForwardedArgs);
  forwardedArgs.append(&argv[1], &argv[argc]);
  forwardedArgs.push_back("-interpret");
  assert(forwardedArgs.size() == numForwardedArgs);

  Observer observer;
  return performFrontend(forwardedArgs, argv[0], (void*) &printMetadataType,
                         &observer);
}
