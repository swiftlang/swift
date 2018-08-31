//===--- swift-remoteast-test.cpp - RemoteAST testing application ---------===//
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
// This file supports performing target-specific remote reflection tests
// on live swift executables.
//===----------------------------------------------------------------------===//

#include "swift/RemoteAST/RemoteAST.h"
#include "swift/Remote/InProcessMemoryReader.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Frontend/Frontend.h"
#include "swift/FrontendTool/FrontendTool.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/LLVMInitialize.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>

#if defined(__ELF__)
#define SWIFT_REMOTEAST_TEST_ABI __attribute__((__visibility__("default")))
#elif defined(__MACH__)
#define SWIFT_REMOTEAST_TEST_ABI __attribute__((__visibility__("default")))
#else
#define SWIFT_REMOTEAST_TEST_ABI __declspec(dllexport)
#endif

using namespace swift;
using namespace swift::remote;
using namespace swift::remoteAST;

/// The context for the code we're running.  Set by the observer.
static ASTContext *Context = nullptr;

// FIXME: swiftcall
/// func printType(forMetadata: Any.Type)
LLVM_ATTRIBUTE_USED SWIFT_REMOTEAST_TEST_ABI
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

// FIXME: swiftcall
/// func printDynamicType(_: AnyObject)
LLVM_ATTRIBUTE_USED SWIFT_REMOTEAST_TEST_ABI
extern "C" void printHeapMetadataType(void *object) {
  assert(Context && "context was not set");

  std::shared_ptr<MemoryReader> reader(new InProcessMemoryReader());
  RemoteASTContext remoteAST(*Context, std::move(reader));

  auto &out = llvm::outs();

  auto metadataResult =
    remoteAST.getHeapMetadataForObject(RemoteAddress(object));
  if (!metadataResult) {
    out << metadataResult.getFailure().render() << '\n';
    return;
  }
  auto metadata = metadataResult.getValue();

  auto result =
    remoteAST.getTypeForRemoteTypeMetadata(metadata, /*skipArtificial*/ true);
  if (result) {
    out << "found type: ";
    result.getValue().print(out);
    out << '\n';
  } else {
    out << result.getFailure().render() << '\n';
  }
}

static void printMemberOffset(const Metadata *typeMetadata,
                              StringRef memberName, bool passMetadata) {
  assert(Context && "context was not set");

  std::shared_ptr<MemoryReader> reader(new InProcessMemoryReader());
  RemoteASTContext remoteAST(*Context, std::move(reader));

  auto &out = llvm::outs();

  // The first thing we have to do is get the type.
  auto typeResult =
    remoteAST.getTypeForRemoteTypeMetadata(RemoteAddress(typeMetadata));
  if (!typeResult) {
    out << "failed to find type: " << typeResult.getFailure().render() << '\n';
    return;
  }

  Type type = typeResult.getValue();

  RemoteAddress address =
    (passMetadata ? RemoteAddress(typeMetadata) : RemoteAddress(nullptr));

  auto offsetResult =
    remoteAST.getOffsetOfMember(type, address, memberName);
  if (!offsetResult) {
    out << "failed to find offset: "
        << offsetResult.getFailure().render() << '\n';
    return;
  }

  out << "found offset: " << offsetResult.getValue() << '\n';
}

// FIXME: swiftcall
/// func printTypeMemberOffset(forType: Any.Type, memberName: StaticString)
LLVM_ATTRIBUTE_USED SWIFT_REMOTEAST_TEST_ABI
extern "C" void printTypeMemberOffset(const Metadata *typeMetadata,
                                      const char *memberName) {
  printMemberOffset(typeMetadata, memberName, /*pass metadata*/ false);
}

// FIXME: swiftcall
/// func printTypeMetadataMemberOffset(forType: Any.Type,
///                                    memberName: StaticString)
LLVM_ATTRIBUTE_USED SWIFT_REMOTEAST_TEST_ABI
extern "C" void printTypeMetadataMemberOffset(const Metadata *typeMetadata,
                                              const char *memberName) {
  printMemberOffset(typeMetadata, memberName, /*pass metadata*/ true);
}

// FIXME: swiftcall
/// func printDynamicTypeAndAddressForExistential<T>(_: T)
LLVM_ATTRIBUTE_USED SWIFT_REMOTEAST_TEST_ABI extern "C" void
printDynamicTypeAndAddressForExistential(void *object,
                                         const Metadata *typeMetadata) {
  assert(Context && "context was not set");
  std::shared_ptr<MemoryReader> reader(new InProcessMemoryReader());
  RemoteASTContext remoteAST(*Context, std::move(reader));

  auto &out = llvm::outs();

  // First, retrieve the static type of the existential, so we can understand
  // which kind of existential this is.
  auto staticTypeResult =
      remoteAST.getTypeForRemoteTypeMetadata(RemoteAddress(typeMetadata));
  if (!staticTypeResult) {
    out << "failed to resolve static type: "
        << staticTypeResult.getFailure().render() << '\n';
    return;
  }

  // OK, we can reconstruct the dynamic type and the address now.
  auto result = remoteAST.getDynamicTypeAndAddressForExistential(
      RemoteAddress(object), staticTypeResult.getValue());
  if (result) {
    out << "found type: ";
    result.getValue().first.print(out);
    out << "\n";
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

} // end anonymous namespace

int main(int argc, const char *argv[]) {
  PROGRAM_START(argc, argv);

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
