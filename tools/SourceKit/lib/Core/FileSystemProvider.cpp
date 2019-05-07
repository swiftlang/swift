//===--- FileSystemProvider.cpp -------------------------------------------===//
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

#include "SourceKit/Core/FileSystemProvider.h"

using namespace SourceKit;

/// A simple configurable FileSystemProvider, useful for SourceKit tests that
/// exercise the FileSystemProvider code.
class TestFileSystemProvider : public FileSystemProvider {
  /// Provides the real filesystem, overlayed with an InMemoryFileSystem that
  /// contains specified files at specified locations.
  /// \param Args The locations of the InMemoryFileSystem files, interleaved
  //              with paths on the real filesystem to fetch their contents
  //              from.
  llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem>
  getFileSystem(const llvm::SmallVectorImpl<const char *> &Args,
                llvm::SmallVectorImpl<char> &ErrBuf) override {
    auto InMemoryFS = llvm::IntrusiveRefCntPtr<llvm::vfs::InMemoryFileSystem>(
        new llvm::vfs::InMemoryFileSystem());
    for (unsigned i = 0; i < Args.size(); i += 2) {
      const char *InMemoryName = Args[i];
      const char *TargetPath = Args[i + 1];
      auto TargetBufferOrErr = llvm::MemoryBuffer::getFile(TargetPath);
      if (auto Err = TargetBufferOrErr.getError()) {
        llvm::raw_svector_ostream ErrStream(ErrBuf);
        ErrStream << "Error reading target file '" << TargetPath
                  << "': " << Err.message() << "\n";
        return nullptr;
      }
      auto RenamedTargetBuffer = llvm::MemoryBuffer::getMemBufferCopy(
          TargetBufferOrErr.get()->getBuffer(), InMemoryName);
      InMemoryFS->addFile(InMemoryName, 0, std::move(RenamedTargetBuffer));
    }

    auto OverlayFS = llvm::IntrusiveRefCntPtr<llvm::vfs::OverlayFileSystem>(
        new llvm::vfs::OverlayFileSystem(llvm::vfs::getRealFileSystem()));
    OverlayFS->pushOverlay(std::move(InMemoryFS));
    return OverlayFS;
  }
};

void SourceKit::makeAllFileSystemProviders(
    llvm::StringMap<std::unique_ptr<FileSystemProvider>> &Providers) {
  Providers.try_emplace("testvfs", llvm::make_unique<TestFileSystemProvider>());
}
