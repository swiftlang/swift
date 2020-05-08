//===- VirtualFileSystem.cpp - Virtual File System Layer ------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the VirtualFileSystem interface.
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/VirtualFileSystem.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/None.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/Twine.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Config/llvm-config.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Chrono.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Errc.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/ErrorOr.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <atomic>
#include <cassert>
#include <cstdint>
#include <iterator>
#include <limits>
#include <map>
#include <memory>
#include <mutex>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

using namespace llvm;
using namespace llvm::vfs;

using llvm::sys::fs::file_t;
using llvm::sys::fs::file_status;
using llvm::sys::fs::file_type;
using llvm::sys::fs::kInvalidFile;
using llvm::sys::fs::perms;
using llvm::sys::fs::UniqueID;

Status::Status(const file_status &Status)
    : UID(Status.getUniqueID()), MTime(Status.getLastModificationTime()),
      User(Status.getUser()), Group(Status.getGroup()), Size(Status.getSize()),
      Type(Status.type()), Perms(Status.permissions()) {}

Status::Status(const Twine &Name, UniqueID UID, sys::TimePoint<> MTime,
               uint32_t User, uint32_t Group, uint64_t Size, file_type Type,
               perms Perms)
    : Name(Name.str()), UID(UID), MTime(MTime), User(User), Group(Group),
      Size(Size), Type(Type), Perms(Perms) {}

Status Status::copyWithNewName(const Status &In, const Twine &NewName) {
  return Status(NewName, In.getUniqueID(), In.getLastModificationTime(),
                In.getUser(), In.getGroup(), In.getSize(), In.getType(),
                In.getPermissions());
}

Status Status::copyWithNewName(const file_status &In, const Twine &NewName) {
  return Status(NewName, In.getUniqueID(), In.getLastModificationTime(),
                In.getUser(), In.getGroup(), In.getSize(), In.type(),
                In.permissions());
}

bool Status::equivalent(const Status &Other) const {
  assert(isStatusKnown() && Other.isStatusKnown());
  return getUniqueID() == Other.getUniqueID();
}

bool Status::isDirectory() const { return Type == file_type::directory_file; }

bool Status::isRegularFile() const { return Type == file_type::regular_file; }

bool Status::isOther() const {
  return exists() && !isRegularFile() && !isDirectory() && !isSymlink();
}

bool Status::isSymlink() const { return Type == file_type::symlink_file; }

bool Status::isStatusKnown() const { return Type != file_type::status_error; }

bool Status::exists() const {
  return isStatusKnown() && Type != file_type::file_not_found;
}

File::~File() = default;

FileSystem::~FileSystem() = default;

ErrorOr<std::unique_ptr<MemoryBuffer>>
FileSystem::getBufferForFile(const llvm::Twine &Name, int64_t FileSize,
                             bool RequiresNullTerminator, bool IsVolatile) {
  auto F = openFileForRead(Name);
  if (!F)
    return F.getError();

  return (*F)->getBuffer(Name, FileSize, RequiresNullTerminator, IsVolatile);
}

std::error_code FileSystem::makeAbsolute(SmallVectorImpl<char> &Path) const {
  if (llvm::sys::path::is_absolute(Path))
    return {};

  auto WorkingDir = getCurrentWorkingDirectory();
  if (!WorkingDir)
    return WorkingDir.getError();

  llvm::sys::fs::make_absolute(WorkingDir.get(), Path);
  return {};
}

std::error_code FileSystem::getRealPath(const Twine &Path,
                                        SmallVectorImpl<char> &Output) const {
  return errc::operation_not_permitted;
}

std::error_code FileSystem::isLocal(const Twine &Path, bool &Result) {
  return errc::operation_not_permitted;
}

bool FileSystem::exists(const Twine &Path) {
  auto Status = status(Path);
  return Status && Status->exists();
}

#ifndef NDEBUG
static bool isTraversalComponent(StringRef Component) {
  return Component.equals("..") || Component.equals(".");
}

static bool pathHasTraversal(StringRef Path) {
  using namespace llvm::sys;

  for (StringRef Comp : llvm::make_range(path::begin(Path), path::end(Path)))
    if (isTraversalComponent(Comp))
      return true;
  return false;
}
#endif

//===-----------------------------------------------------------------------===/
// RealFileSystem implementation
//===-----------------------------------------------------------------------===/

namespace {

/// Wrapper around a raw file descriptor.
class RealFile : public File {
  friend class RealFileSystem;

  file_t FD;
  Status S;
  std::string RealName;

  RealFile(file_t RawFD, StringRef NewName, StringRef NewRealPathName)
      : FD(RawFD), S(NewName, {}, {}, {}, {}, {},
                     llvm::sys::fs::file_type::status_error, {}),
        RealName(NewRealPathName.str()) {
    assert(FD != kInvalidFile && "Invalid or inactive file descriptor");
  }

public:
  ~RealFile() override;

  ErrorOr<Status> status() override;
  ErrorOr<std::string> getName() override;
  ErrorOr<std::unique_ptr<MemoryBuffer>> getBuffer(const Twine &Name,
                                                   int64_t FileSize,
                                                   bool RequiresNullTerminator,
                                                   bool IsVolatile) override;
  std::error_code close() override;
};

} // namespace

RealFile::~RealFile() { close(); }

ErrorOr<Status> RealFile::status() {
  assert(FD != kInvalidFile && "cannot stat closed file");
  if (!S.isStatusKnown()) {
    file_status RealStatus;
    if (std::error_code EC = sys::fs::status(FD, RealStatus))
      return EC;
    S = Status::copyWithNewName(RealStatus, S.getName());
  }
  return S;
}

ErrorOr<std::string> RealFile::getName() {
  return RealName.empty() ? S.getName().str() : RealName;
}

ErrorOr<std::unique_ptr<MemoryBuffer>>
RealFile::getBuffer(const Twine &Name, int64_t FileSize,
                    bool RequiresNullTerminator, bool IsVolatile) {
  assert(FD != kInvalidFile && "cannot get buffer for closed file");
  return MemoryBuffer::getOpenFile(FD, Name, FileSize, RequiresNullTerminator,
                                   IsVolatile);
}

std::error_code RealFile::close() {
  std::error_code EC = sys::fs::closeFile(FD);
  FD = kInvalidFile;
  return EC;
}

namespace {

/// A file system according to your operating system.
/// This may be linked to the process's working directory, or maintain its own.
///
/// Currently, its own working directory is emulated by storing the path and
/// sending absolute paths to llvm::sys::fs:: functions.
/// A more principled approach would be to push this down a level, modelling
/// the working dir as an llvm::sys::fs::WorkingDir or similar.
/// This would enable the use of openat()-style functions on some platforms.
class RealFileSystem : public FileSystem {
public:
  explicit RealFileSystem(bool LinkCWDToProcess) {
    if (!LinkCWDToProcess) {
      SmallString<128> PWD, RealPWD;
      if (llvm::sys::fs::current_path(PWD))
        return; // Awful, but nothing to do here.
      if (llvm::sys::fs::real_path(PWD, RealPWD))
        WD = {PWD, PWD};
      else
        WD = {PWD, RealPWD};
    }
  }

  ErrorOr<Status> status(const Twine &Path) override;
  ErrorOr<std::unique_ptr<File>> openFileForRead(const Twine &Path) override;
  directory_iterator dir_begin(const Twine &Dir, std::error_code &EC) override;

  llvm::ErrorOr<std::string> getCurrentWorkingDirectory() const override;
  std::error_code setCurrentWorkingDirectory(const Twine &Path) override;
  std::error_code isLocal(const Twine &Path, bool &Result) override;
  std::error_code getRealPath(const Twine &Path,
                              SmallVectorImpl<char> &Output) const override;

private:
  // If this FS has its own working dir, use it to make Path absolute.
  // The returned twine is safe to use as long as both Storage and Path live.
  Twine adjustPath(const Twine &Path, SmallVectorImpl<char> &Storage) const {
    if (!WD)
      return Path;
    Path.toVector(Storage);
    sys::fs::make_absolute(WD->Resolved, Storage);
    return Storage;
  }

  struct WorkingDirectory {
    // The current working directory, without symlinks resolved. (echo $PWD).
    SmallString<128> Specified;
    // The current working directory, with links resolved. (readlink .).
    SmallString<128> Resolved;
  };
  Optional<WorkingDirectory> WD;
};

} // namespace

ErrorOr<Status> RealFileSystem::status(const Twine &Path) {
  SmallString<256> Storage;
  sys::fs::file_status RealStatus;
  if (std::error_code EC =
          sys::fs::status(adjustPath(Path, Storage), RealStatus))
    return EC;
  return Status::copyWithNewName(RealStatus, Path);
}

ErrorOr<std::unique_ptr<File>>
RealFileSystem::openFileForRead(const Twine &Name) {
  SmallString<256> RealName, Storage;
  Expected<file_t> FDOrErr = sys::fs::openNativeFileForRead(
      adjustPath(Name, Storage), sys::fs::OF_None, &RealName);
  if (!FDOrErr)
    return errorToErrorCode(FDOrErr.takeError());
  return std::unique_ptr<File>(
      new RealFile(*FDOrErr, Name.str(), RealName.str()));
}

llvm::ErrorOr<std::string> RealFileSystem::getCurrentWorkingDirectory() const {
  if (WD)
    return std::string(WD->Specified.str());

  SmallString<128> Dir;
  if (std::error_code EC = llvm::sys::fs::current_path(Dir))
    return EC;
  return std::string(Dir.str());
}

std::error_code RealFileSystem::setCurrentWorkingDirectory(const Twine &Path) {
  if (!WD)
    return llvm::sys::fs::set_current_path(Path);

  SmallString<128> Absolute, Resolved, Storage;
  adjustPath(Path, Storage).toVector(Absolute);
  bool IsDir;
  if (auto Err = llvm::sys::fs::is_directory(Absolute, IsDir))
    return Err;
  if (!IsDir)
    return std::make_error_code(std::errc::not_a_directory);
  if (auto Err = llvm::sys::fs::real_path(Absolute, Resolved))
    return Err;
  WD = {Absolute, Resolved};
  return std::error_code();
}

std::error_code RealFileSystem::isLocal(const Twine &Path, bool &Result) {
  SmallString<256> Storage;
  return llvm::sys::fs::is_local(adjustPath(Path, Storage), Result);
}

std::error_code
RealFileSystem::getRealPath(const Twine &Path,
                            SmallVectorImpl<char> &Output) const {
  SmallString<256> Storage;
  return llvm::sys::fs::real_path(adjustPath(Path, Storage), Output);
}

IntrusiveRefCntPtr<FileSystem> vfs::getRealFileSystem() {
  static IntrusiveRefCntPtr<FileSystem> FS(new RealFileSystem(true));
  return FS;
}

std::unique_ptr<FileSystem> vfs::createPhysicalFileSystem() {
  return std::make_unique<RealFileSystem>(false);
}

namespace {

class RealFSDirIter : public llvm::vfs::detail::DirIterImpl {
  llvm::sys::fs::directory_iterator Iter;

public:
  RealFSDirIter(const Twine &Path, std::error_code &EC) : Iter(Path, EC) {
    if (Iter != llvm::sys::fs::directory_iterator())
      CurrentEntry = directory_entry(Iter->path(), Iter->type());
  }

  std::error_code increment() override {
    std::error_code EC;
    Iter.increment(EC);
    CurrentEntry = (Iter == llvm::sys::fs::directory_iterator())
                       ? directory_entry()
                       : directory_entry(Iter->path(), Iter->type());
    return EC;
  }
};

} // namespace

directory_iterator RealFileSystem::dir_begin(const Twine &Dir,
                                             std::error_code &EC) {
  SmallString<128> Storage;
  return directory_iterator(
      std::make_shared<RealFSDirIter>(adjustPath(Dir, Storage), EC));
}

//===-----------------------------------------------------------------------===/
// OverlayFileSystem implementation
//===-----------------------------------------------------------------------===/

OverlayFileSystem::OverlayFileSystem(IntrusiveRefCntPtr<FileSystem> BaseFS) {
  FSList.push_back(std::move(BaseFS));
}

void OverlayFileSystem::pushOverlay(IntrusiveRefCntPtr<FileSystem> FS) {
  FSList.push_back(FS);
  // Synchronize added file systems by duplicating the working directory from
  // the first one in the list.
  FS->setCurrentWorkingDirectory(getCurrentWorkingDirectory().get());
}

ErrorOr<Status> OverlayFileSystem::status(const Twine &Path) {
  // FIXME: handle symlinks that cross file systems
  for (iterator I = overlays_begin(), E = overlays_end(); I != E; ++I) {
    ErrorOr<Status> Status = (*I)->status(Path);
    if (Status || Status.getError() != llvm::errc::no_such_file_or_directory)
      return Status;
  }
  return make_error_code(llvm::errc::no_such_file_or_directory);
}

ErrorOr<std::unique_ptr<File>>
OverlayFileSystem::openFileForRead(const llvm::Twine &Path) {
  // FIXME: handle symlinks that cross file systems
  for (iterator I = overlays_begin(), E = overlays_end(); I != E; ++I) {
    auto Result = (*I)->openFileForRead(Path);
    if (Result || Result.getError() != llvm::errc::no_such_file_or_directory)
      return Result;
  }
  return make_error_code(llvm::errc::no_such_file_or_directory);
}

llvm::ErrorOr<std::string>
OverlayFileSystem::getCurrentWorkingDirectory() const {
  // All file systems are synchronized, just take the first working directory.
  return FSList.front()->getCurrentWorkingDirectory();
}

std::error_code
OverlayFileSystem::setCurrentWorkingDirectory(const Twine &Path) {
  for (auto &FS : FSList)
    if (std::error_code EC = FS->setCurrentWorkingDirectory(Path))
      return EC;
  return {};
}

std::error_code OverlayFileSystem::isLocal(const Twine &Path, bool &Result) {
  for (auto &FS : FSList)
    if (FS->exists(Path))
      return FS->isLocal(Path, Result);
  return errc::no_such_file_or_directory;
}

std::error_code
OverlayFileSystem::getRealPath(const Twine &Path,
                               SmallVectorImpl<char> &Output) const {
  for (auto &FS : FSList)
    if (FS->exists(Path))
      return FS->getRealPath(Path, Output);
  return errc::no_such_file_or_directory;
}

llvm::vfs::detail::DirIterImpl::~DirIterImpl() = default;

namespace {

class OverlayFSDirIterImpl : public llvm::vfs::detail::DirIterImpl {
  OverlayFileSystem &Overlays;
  std::string Path;
  OverlayFileSystem::iterator CurrentFS;
  directory_iterator CurrentDirIter;
  llvm::StringSet<> SeenNames;

  std::error_code incrementFS() {
    assert(CurrentFS != Overlays.overlays_end() && "incrementing past end");
    ++CurrentFS;
    for (auto E = Overlays.overlays_end(); CurrentFS != E; ++CurrentFS) {
      std::error_code EC;
      CurrentDirIter = (*CurrentFS)->dir_begin(Path, EC);
      if (EC && EC != errc::no_such_file_or_directory)
        return EC;
      if (CurrentDirIter != directory_iterator())
        break; // found
    }
    return {};
  }

  std::error_code incrementDirIter(bool IsFirstTime) {
    assert((IsFirstTime || CurrentDirIter != directory_iterator()) &&
           "incrementing past end");
    std::error_code EC;
    if (!IsFirstTime)
      CurrentDirIter.increment(EC);
    if (!EC && CurrentDirIter == directory_iterator())
      EC = incrementFS();
    return EC;
  }

  std::error_code incrementImpl(bool IsFirstTime) {
    while (true) {
      std::error_code EC = incrementDirIter(IsFirstTime);
      if (EC || CurrentDirIter == directory_iterator()) {
        CurrentEntry = directory_entry();
        return EC;
      }
      CurrentEntry = *CurrentDirIter;
      StringRef Name = llvm::sys::path::filename(CurrentEntry.path());
      if (SeenNames.insert(Name).second)
        return EC; // name not seen before
    }
    llvm_unreachable("returned above");
  }

public:
  OverlayFSDirIterImpl(const Twine &Path, OverlayFileSystem &FS,
                       std::error_code &EC)
      : Overlays(FS), Path(Path.str()), CurrentFS(Overlays.overlays_begin()) {
    CurrentDirIter = (*CurrentFS)->dir_begin(Path, EC);
    EC = incrementImpl(true);
  }

  std::error_code increment() override { return incrementImpl(false); }
};

} // namespace

directory_iterator OverlayFileSystem::dir_begin(const Twine &Dir,
                                                std::error_code &EC) {
  return directory_iterator(
      std::make_shared<OverlayFSDirIterImpl>(Dir, *this, EC));
}

void ProxyFileSystem::anchor() {}

namespace llvm {
namespace vfs {

namespace detail {

enum InMemoryNodeKind { IME_File, IME_Directory, IME_HardLink };

/// The in memory file system is a tree of Nodes. Every node can either be a
/// file , hardlink or a directory.
class InMemoryNode {
  InMemoryNodeKind Kind;
  std::string FileName;

public:
  InMemoryNode(llvm::StringRef FileName, InMemoryNodeKind Kind)
      : Kind(Kind), FileName(std::string(llvm::sys::path::filename(FileName))) {
  }
  virtual ~InMemoryNode() = default;

  /// Get the filename of this node (the name without the directory part).
  StringRef getFileName() const { return FileName; }
  InMemoryNodeKind getKind() const { return Kind; }
  virtual std::string toString(unsigned Indent) const = 0;
};

class InMemoryFile : public InMemoryNode {
  Status Stat;
  std::unique_ptr<llvm::MemoryBuffer> Buffer;

public:
  InMemoryFile(Status Stat, std::unique_ptr<llvm::MemoryBuffer> Buffer)
      : InMemoryNode(Stat.getName(), IME_File), Stat(std::move(Stat)),
        Buffer(std::move(Buffer)) {}

  /// Return the \p Status for this node. \p RequestedName should be the name
  /// through which the caller referred to this node. It will override
  /// \p Status::Name in the return value, to mimic the behavior of \p RealFile.
  Status getStatus(const Twine &RequestedName) const {
    return Status::copyWithNewName(Stat, RequestedName);
  }
  llvm::MemoryBuffer *getBuffer() const { return Buffer.get(); }

  std::string toString(unsigned Indent) const override {
    return (std::string(Indent, ' ') + Stat.getName() + "\n").str();
  }

  static bool classof(const InMemoryNode *N) {
    return N->getKind() == IME_File;
  }
};

namespace {

class InMemoryHardLink : public InMemoryNode {
  const InMemoryFile &ResolvedFile;

public:
  InMemoryHardLink(StringRef Path, const InMemoryFile &ResolvedFile)
      : InMemoryNode(Path, IME_HardLink), ResolvedFile(ResolvedFile) {}
  const InMemoryFile &getResolvedFile() const { return ResolvedFile; }

  std::string toString(unsigned Indent) const override {
    return std::string(Indent, ' ') + "HardLink to -> " +
           ResolvedFile.toString(0);
  }

  static bool classof(const InMemoryNode *N) {
    return N->getKind() == IME_HardLink;
  }
};

/// Adapt a InMemoryFile for VFS' File interface.  The goal is to make
/// \p InMemoryFileAdaptor mimic as much as possible the behavior of
/// \p RealFile.
class InMemoryFileAdaptor : public File {
  const InMemoryFile &Node;
  /// The name to use when returning a Status for this file.
  std::string RequestedName;

public:
  explicit InMemoryFileAdaptor(const InMemoryFile &Node,
                               std::string RequestedName)
      : Node(Node), RequestedName(std::move(RequestedName)) {}

  llvm::ErrorOr<Status> status() override {
    return Node.getStatus(RequestedName);
  }

  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
  getBuffer(const Twine &Name, int64_t FileSize, bool RequiresNullTerminator,
            bool IsVolatile) override {
    llvm::MemoryBuffer *Buf = Node.getBuffer();
    return llvm::MemoryBuffer::getMemBuffer(
        Buf->getBuffer(), Buf->getBufferIdentifier(), RequiresNullTerminator);
  }

  std::error_code close() override { return {}; }
};
} // namespace

class InMemoryDirectory : public InMemoryNode {
  Status Stat;
  llvm::StringMap<std::unique_ptr<InMemoryNode>> Entries;

public:
  InMemoryDirectory(Status Stat)
      : InMemoryNode(Stat.getName(), IME_Directory), Stat(std::move(Stat)) {}

  /// Return the \p Status for this node. \p RequestedName should be the name
  /// through which the caller referred to this node. It will override
  /// \p Status::Name in the return value, to mimic the behavior of \p RealFile.
  Status getStatus(const Twine &RequestedName) const {
    return Status::copyWithNewName(Stat, RequestedName);
  }
  InMemoryNode *getChild(StringRef Name) {
    auto I = Entries.find(Name);
    if (I != Entries.end())
      return I->second.get();
    return nullptr;
  }

  InMemoryNode *addChild(StringRef Name, std::unique_ptr<InMemoryNode> Child) {
    return Entries.insert(make_pair(Name, std::move(Child)))
        .first->second.get();
  }

  using const_iterator = decltype(Entries)::const_iterator;

  const_iterator begin() const { return Entries.begin(); }
  const_iterator end() const { return Entries.end(); }

  std::string toString(unsigned Indent) const override {
    std::string Result =
        (std::string(Indent, ' ') + Stat.getName() + "\n").str();
    for (const auto &Entry : Entries)
      Result += Entry.second->toString(Indent + 2);
    return Result;
  }

  static bool classof(const InMemoryNode *N) {
    return N->getKind() == IME_Directory;
  }
};

namespace {
Status getNodeStatus(const InMemoryNode *Node, const Twine &RequestedName) {
  if (auto Dir = dyn_cast<detail::InMemoryDirectory>(Node))
    return Dir->getStatus(RequestedName);
  if (auto File = dyn_cast<detail::InMemoryFile>(Node))
    return File->getStatus(RequestedName);
  if (auto Link = dyn_cast<detail::InMemoryHardLink>(Node))
    return Link->getResolvedFile().getStatus(RequestedName);
  llvm_unreachable("Unknown node type");
}
} // namespace
} // namespace detail

InMemoryFileSystem::InMemoryFileSystem(bool UseNormalizedPaths)
    : Root(new detail::InMemoryDirectory(
          Status("", getNextVirtualUniqueID(), llvm::sys::TimePoint<>(), 0, 0,
                 0, llvm::sys::fs::file_type::directory_file,
                 llvm::sys::fs::perms::all_all))),
      UseNormalizedPaths(UseNormalizedPaths) {}

InMemoryFileSystem::~InMemoryFileSystem() = default;

std::string InMemoryFileSystem::toString() const {
  return Root->toString(/*Indent=*/0);
}

bool InMemoryFileSystem::addFile(const Twine &P, time_t ModificationTime,
                                 std::unique_ptr<llvm::MemoryBuffer> Buffer,
                                 Optional<uint32_t> User,
                                 Optional<uint32_t> Group,
                                 Optional<llvm::sys::fs::file_type> Type,
                                 Optional<llvm::sys::fs::perms> Perms,
                                 const detail::InMemoryFile *HardLinkTarget) {
  SmallString<128> Path;
  P.toVector(Path);

  // Fix up relative paths. This just prepends the current working directory.
  std::error_code EC = makeAbsolute(Path);
  assert(!EC);
  (void)EC;

  if (useNormalizedPaths())
    llvm::sys::path::remove_dots(Path, /*remove_dot_dot=*/true);

  if (Path.empty())
    return false;

  detail::InMemoryDirectory *Dir = Root.get();
  auto I = llvm::sys::path::begin(Path), E = sys::path::end(Path);
  const auto ResolvedUser = User.getValueOr(0);
  const auto ResolvedGroup = Group.getValueOr(0);
  const auto ResolvedType = Type.getValueOr(sys::fs::file_type::regular_file);
  const auto ResolvedPerms = Perms.getValueOr(sys::fs::all_all);
  assert(!(HardLinkTarget && Buffer) && "HardLink cannot have a buffer");
  // Any intermediate directories we create should be accessible by
  // the owner, even if Perms says otherwise for the final path.
  const auto NewDirectoryPerms = ResolvedPerms | sys::fs::owner_all;
  while (true) {
    StringRef Name = *I;
    detail::InMemoryNode *Node = Dir->getChild(Name);
    ++I;
    if (!Node) {
      if (I == E) {
        // End of the path.
        std::unique_ptr<detail::InMemoryNode> Child;
        if (HardLinkTarget)
          Child.reset(new detail::InMemoryHardLink(P.str(), *HardLinkTarget));
        else {
          // Create a new file or directory.
          Status Stat(P.str(), getNextVirtualUniqueID(),
                      llvm::sys::toTimePoint(ModificationTime), ResolvedUser,
                      ResolvedGroup, Buffer->getBufferSize(), ResolvedType,
                      ResolvedPerms);
          if (ResolvedType == sys::fs::file_type::directory_file) {
            Child.reset(new detail::InMemoryDirectory(std::move(Stat)));
          } else {
            Child.reset(
                new detail::InMemoryFile(std::move(Stat), std::move(Buffer)));
          }
        }
        Dir->addChild(Name, std::move(Child));
        return true;
      }

      // Create a new directory. Use the path up to here.
      Status Stat(
          StringRef(Path.str().begin(), Name.end() - Path.str().begin()),
          getNextVirtualUniqueID(), llvm::sys::toTimePoint(ModificationTime),
          ResolvedUser, ResolvedGroup, 0, sys::fs::file_type::directory_file,
          NewDirectoryPerms);
      Dir = cast<detail::InMemoryDirectory>(Dir->addChild(
          Name, std::make_unique<detail::InMemoryDirectory>(std::move(Stat))));
      continue;
    }

    if (auto *NewDir = dyn_cast<detail::InMemoryDirectory>(Node)) {
      Dir = NewDir;
    } else {
      assert((isa<detail::InMemoryFile>(Node) ||
              isa<detail::InMemoryHardLink>(Node)) &&
             "Must be either file, hardlink or directory!");

      // Trying to insert a directory in place of a file.
      if (I != E)
        return false;

      // Return false only if the new file is different from the existing one.
      if (auto Link = dyn_cast<detail::InMemoryHardLink>(Node)) {
        return Link->getResolvedFile().getBuffer()->getBuffer() ==
               Buffer->getBuffer();
      }
      return cast<detail::InMemoryFile>(Node)->getBuffer()->getBuffer() ==
             Buffer->getBuffer();
    }
  }
}

bool InMemoryFileSystem::addFile(const Twine &P, time_t ModificationTime,
                                 std::unique_ptr<llvm::MemoryBuffer> Buffer,
                                 Optional<uint32_t> User,
                                 Optional<uint32_t> Group,
                                 Optional<llvm::sys::fs::file_type> Type,
                                 Optional<llvm::sys::fs::perms> Perms) {
  return addFile(P, ModificationTime, std::move(Buffer), User, Group, Type,
                 Perms, /*HardLinkTarget=*/nullptr);
}

bool InMemoryFileSystem::addFileNoOwn(const Twine &P, time_t ModificationTime,
                                      llvm::MemoryBuffer *Buffer,
                                      Optional<uint32_t> User,
                                      Optional<uint32_t> Group,
                                      Optional<llvm::sys::fs::file_type> Type,
                                      Optional<llvm::sys::fs::perms> Perms) {
  return addFile(P, ModificationTime,
                 llvm::MemoryBuffer::getMemBuffer(
                     Buffer->getBuffer(), Buffer->getBufferIdentifier()),
                 std::move(User), std::move(Group), std::move(Type),
                 std::move(Perms));
}

static ErrorOr<const detail::InMemoryNode *>
lookupInMemoryNode(const InMemoryFileSystem &FS, detail::InMemoryDirectory *Dir,
                   const Twine &P) {
  SmallString<128> Path;
  P.toVector(Path);

  // Fix up relative paths. This just prepends the current working directory.
  std::error_code EC = FS.makeAbsolute(Path);
  assert(!EC);
  (void)EC;

  if (FS.useNormalizedPaths())
    llvm::sys::path::remove_dots(Path, /*remove_dot_dot=*/true);

  if (Path.empty())
    return Dir;

  auto I = llvm::sys::path::begin(Path), E = llvm::sys::path::end(Path);
  while (true) {
    detail::InMemoryNode *Node = Dir->getChild(*I);
    ++I;
    if (!Node)
      return errc::no_such_file_or_directory;

    // Return the file if it's at the end of the path.
    if (auto File = dyn_cast<detail::InMemoryFile>(Node)) {
      if (I == E)
        return File;
      return errc::no_such_file_or_directory;
    }

    // If Node is HardLink then return the resolved file.
    if (auto File = dyn_cast<detail::InMemoryHardLink>(Node)) {
      if (I == E)
        return &File->getResolvedFile();
      return errc::no_such_file_or_directory;
    }
    // Traverse directories.
    Dir = cast<detail::InMemoryDirectory>(Node);
    if (I == E)
      return Dir;
  }
}

bool InMemoryFileSystem::addHardLink(const Twine &FromPath,
                                     const Twine &ToPath) {
  auto FromNode = lookupInMemoryNode(*this, Root.get(), FromPath);
  auto ToNode = lookupInMemoryNode(*this, Root.get(), ToPath);
  // FromPath must not have been added before. ToPath must have been added
  // before. Resolved ToPath must be a File.
  if (!ToNode || FromNode || !isa<detail::InMemoryFile>(*ToNode))
    return false;
  return this->addFile(FromPath, 0, nullptr, None, None, None, None,
                       cast<detail::InMemoryFile>(*ToNode));
}

llvm::ErrorOr<Status> InMemoryFileSystem::status(const Twine &Path) {
  auto Node = lookupInMemoryNode(*this, Root.get(), Path);
  if (Node)
    return detail::getNodeStatus(*Node, Path);
  return Node.getError();
}

llvm::ErrorOr<std::unique_ptr<File>>
InMemoryFileSystem::openFileForRead(const Twine &Path) {
  auto Node = lookupInMemoryNode(*this, Root.get(), Path);
  if (!Node)
    return Node.getError();

  // When we have a file provide a heap-allocated wrapper for the memory buffer
  // to match the ownership semantics for File.
  if (auto *F = dyn_cast<detail::InMemoryFile>(*Node))
    return std::unique_ptr<File>(
        new detail::InMemoryFileAdaptor(*F, Path.str()));

  // FIXME: errc::not_a_file?
  return make_error_code(llvm::errc::invalid_argument);
}

namespace {

/// Adaptor from InMemoryDir::iterator to directory_iterator.
class InMemoryDirIterator : public llvm::vfs::detail::DirIterImpl {
  detail::InMemoryDirectory::const_iterator I;
  detail::InMemoryDirectory::const_iterator E;
  std::string RequestedDirName;

  void setCurrentEntry() {
    if (I != E) {
      SmallString<256> Path(RequestedDirName);
      llvm::sys::path::append(Path, I->second->getFileName());
      sys::fs::file_type Type = sys::fs::file_type::type_unknown;
      switch (I->second->getKind()) {
      case detail::IME_File:
      case detail::IME_HardLink:
        Type = sys::fs::file_type::regular_file;
        break;
      case detail::IME_Directory:
        Type = sys::fs::file_type::directory_file;
        break;
      }
      CurrentEntry = directory_entry(std::string(Path.str()), Type);
    } else {
      // When we're at the end, make CurrentEntry invalid and DirIterImpl will
      // do the rest.
      CurrentEntry = directory_entry();
    }
  }

public:
  InMemoryDirIterator() = default;

  explicit InMemoryDirIterator(const detail::InMemoryDirectory &Dir,
                               std::string RequestedDirName)
      : I(Dir.begin()), E(Dir.end()),
        RequestedDirName(std::move(RequestedDirName)) {
    setCurrentEntry();
  }

  std::error_code increment() override {
    ++I;
    setCurrentEntry();
    return {};
  }
};

} // namespace

directory_iterator InMemoryFileSystem::dir_begin(const Twine &Dir,
                                                 std::error_code &EC) {
  auto Node = lookupInMemoryNode(*this, Root.get(), Dir);
  if (!Node) {
    EC = Node.getError();
    return directory_iterator(std::make_shared<InMemoryDirIterator>());
  }

  if (auto *DirNode = dyn_cast<detail::InMemoryDirectory>(*Node))
    return directory_iterator(
        std::make_shared<InMemoryDirIterator>(*DirNode, Dir.str()));

  EC = make_error_code(llvm::errc::not_a_directory);
  return directory_iterator(std::make_shared<InMemoryDirIterator>());
}

std::error_code InMemoryFileSystem::setCurrentWorkingDirectory(const Twine &P) {
  SmallString<128> Path;
  P.toVector(Path);

  // Fix up relative paths. This just prepends the current working directory.
  std::error_code EC = makeAbsolute(Path);
  assert(!EC);
  (void)EC;

  if (useNormalizedPaths())
    llvm::sys::path::remove_dots(Path, /*remove_dot_dot=*/true);

  if (!Path.empty())
    WorkingDirectory = std::string(Path.str());
  return {};
}

std::error_code
InMemoryFileSystem::getRealPath(const Twine &Path,
                                SmallVectorImpl<char> &Output) const {
  auto CWD = getCurrentWorkingDirectory();
  if (!CWD || CWD->empty())
    return errc::operation_not_permitted;
  Path.toVector(Output);
  if (auto EC = makeAbsolute(Output))
    return EC;
  llvm::sys::path::remove_dots(Output, /*remove_dot_dot=*/true);
  return {};
}

std::error_code InMemoryFileSystem::isLocal(const Twine &Path, bool &Result) {
  Result = false;
  return {};
}

} // namespace vfs
} // namespace llvm

//===-----------------------------------------------------------------------===/
// RedirectingFileSystem implementation
//===-----------------------------------------------------------------------===/

namespace {

/// Removes leading "./" as well as path components like ".." and ".".
static llvm::SmallString<256> canonicalize(llvm::StringRef Path) {
  // First detect the path style in use by checking the first separator.
  llvm::sys::path::Style style = llvm::sys::path::Style::native;
  const size_t n = Path.find_first_of("/\\");
  if (n != static_cast<size_t>(-1))
    style = (Path[n] == '/') ? llvm::sys::path::Style::posix
                             : llvm::sys::path::Style::windows;

  // Now remove the dots.  Explicitly specifying the path style prevents the
  // direction of the slashes from changing.
  llvm::SmallString<256> result =
      llvm::sys::path::remove_leading_dotslash(Path, style);
  llvm::sys::path::remove_dots(result, /*remove_dot_dot=*/true, style);
  return result;
}

} // anonymous namespace


RedirectingFileSystem::RedirectingFileSystem(IntrusiveRefCntPtr<FileSystem> FS)
    : ExternalFS(std::move(FS)) {
  if (ExternalFS)
    if (auto ExternalWorkingDirectory =
            ExternalFS->getCurrentWorkingDirectory()) {
      WorkingDirectory = *ExternalWorkingDirectory;
      ExternalFSValidWD = true;
    }
}

// FIXME: reuse implementation common with OverlayFSDirIterImpl as these
// iterators are conceptually similar.
class llvm::vfs::VFSFromYamlDirIterImpl
    : public llvm::vfs::detail::DirIterImpl {
  std::string Dir;
  RedirectingFileSystem::RedirectingDirectoryEntry::iterator Current, End;

  // To handle 'fallthrough' mode we need to iterate at first through
  // RedirectingDirectoryEntry and then through ExternalFS. These operations are
  // done sequentially, we just need to keep a track of what kind of iteration
  // we are currently performing.

  /// Flag telling if we should iterate through ExternalFS or stop at the last
  /// RedirectingDirectoryEntry::iterator.
  bool IterateExternalFS;
  /// Flag telling if we have switched to iterating through ExternalFS.
  bool IsExternalFSCurrent = false;
  FileSystem &ExternalFS;
  directory_iterator ExternalDirIter;
  llvm::StringSet<> SeenNames;

  /// To combine multiple iterations, different methods are responsible for
  /// different iteration steps.
  /// @{

  /// Responsible for dispatching between RedirectingDirectoryEntry iteration
  /// and ExternalFS iteration.
  std::error_code incrementImpl(bool IsFirstTime);
  /// Responsible for RedirectingDirectoryEntry iteration.
  std::error_code incrementContent(bool IsFirstTime);
  /// Responsible for ExternalFS iteration.
  std::error_code incrementExternal();
  /// @}

public:
  VFSFromYamlDirIterImpl(
      const Twine &Path,
      RedirectingFileSystem::RedirectingDirectoryEntry::iterator Begin,
      RedirectingFileSystem::RedirectingDirectoryEntry::iterator End,
      bool IterateExternalFS, FileSystem &ExternalFS, std::error_code &EC);

  std::error_code increment() override;
};

llvm::ErrorOr<std::string>
RedirectingFileSystem::getCurrentWorkingDirectory() const {
  return WorkingDirectory;
}

std::error_code
RedirectingFileSystem::setCurrentWorkingDirectory(const Twine &Path) {
  // Don't change the working directory if the path doesn't exist.
  if (!exists(Path))
    return errc::no_such_file_or_directory;

  // Always change the external FS but ignore its result.
  if (ExternalFS) {
    auto EC = ExternalFS->setCurrentWorkingDirectory(Path);
    ExternalFSValidWD = !static_cast<bool>(EC);
  }

  SmallString<128> AbsolutePath;
  Path.toVector(AbsolutePath);
  if (std::error_code EC = makeAbsolute(AbsolutePath))
    return EC;
  WorkingDirectory = std::string(AbsolutePath.str());
  return {};
}

std::error_code RedirectingFileSystem::isLocal(const Twine &Path,
                                               bool &Result) {
  return ExternalFS->isLocal(Path, Result);
}

std::error_code RedirectingFileSystem::makeAbsolute(SmallVectorImpl<char> &Path) const {
  if (llvm::sys::path::is_absolute(Path, llvm::sys::path::Style::posix) ||
      llvm::sys::path::is_absolute(Path, llvm::sys::path::Style::windows))
    return {};

  auto WorkingDir = getCurrentWorkingDirectory();
  if (!WorkingDir)
    return WorkingDir.getError();

  // We can't use sys::fs::make_absolute because that assumes the path style
  // is native and there is no way to override that.  Since we know WorkingDir
  // is absolute, we can use it to determine which style we actually have and
  // append Path ourselves.
  sys::path::Style style = sys::path::Style::windows;
  if (sys::path::is_absolute(WorkingDir.get(), sys::path::Style::posix)) {
    style = sys::path::Style::posix;
  }

  std::string Result = WorkingDir.get();
  StringRef Dir(Result);
  if (!Dir.endswith(sys::path::get_separator(style))) {
    Result += sys::path::get_separator(style);
  }
  Result.append(Path.data(), Path.size());
  Path.assign(Result.begin(), Result.end());

  return {};
}

directory_iterator RedirectingFileSystem::dir_begin(const Twine &Dir,
                                                    std::error_code &EC) {
  ErrorOr<RedirectingFileSystem::Entry *> E = lookupPath(Dir);
  if (!E) {
    EC = E.getError();
    if (shouldUseExternalFS() && EC == errc::no_such_file_or_directory)
      return ExternalFS->dir_begin(Dir, EC);
    return {};
  }
  ErrorOr<Status> S = status(Dir, *E);
  if (!S) {
    EC = S.getError();
    return {};
  }
  if (!S->isDirectory()) {
    EC = std::error_code(static_cast<int>(errc::not_a_directory),
                         std::system_category());
    return {};
  }

  auto *D = cast<RedirectingFileSystem::RedirectingDirectoryEntry>(*E);
  return directory_iterator(std::make_shared<VFSFromYamlDirIterImpl>(
      Dir, D->contents_begin(), D->contents_end(),
      /*IterateExternalFS=*/shouldUseExternalFS(), *ExternalFS, EC));
}

void RedirectingFileSystem::setExternalContentsPrefixDir(StringRef PrefixDir) {
  ExternalContentsPrefixDir = PrefixDir.str();
}

StringRef RedirectingFileSystem::getExternalContentsPrefixDir() const {
  return ExternalContentsPrefixDir;
}

void RedirectingFileSystem::dump(raw_ostream &OS) const {
  for (const auto &Root : Roots)
    dumpEntry(OS, Root.get());
}

void RedirectingFileSystem::dumpEntry(raw_ostream &OS,
                                      RedirectingFileSystem::Entry *E,
                                      int NumSpaces) const {
  StringRef Name = E->getName();
  for (int i = 0, e = NumSpaces; i < e; ++i)
    OS << " ";
  OS << "'" << Name.str().c_str() << "'"
     << "\n";

  if (E->getKind() == RedirectingFileSystem::EK_Directory) {
    auto *DE = dyn_cast<RedirectingFileSystem::RedirectingDirectoryEntry>(E);
    assert(DE && "Should be a directory");

    for (std::unique_ptr<Entry> &SubEntry :
         llvm::make_range(DE->contents_begin(), DE->contents_end()))
      dumpEntry(OS, SubEntry.get(), NumSpaces + 2);
  }
}

#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
LLVM_DUMP_METHOD void RedirectingFileSystem::dump() const { dump(dbgs()); }
#endif

/// A helper class to hold the common YAML parsing state.
class llvm::vfs::RedirectingFileSystemParser {
  yaml::Stream &Stream;

  void error(yaml::Node *N, const Twine &Msg) { Stream.printError(N, Msg); }

  // false on error
  bool parseScalarString(yaml::Node *N, StringRef &Result,
                         SmallVectorImpl<char> &Storage) {
    const auto *S = dyn_cast<yaml::ScalarNode>(N);

    if (!S) {
      error(N, "expected string");
      return false;
    }
    Result = S->getValue(Storage);
    return true;
  }

  // false on error
  bool parseScalarBool(yaml::Node *N, bool &Result) {
    SmallString<5> Storage;
    StringRef Value;
    if (!parseScalarString(N, Value, Storage))
      return false;

    if (Value.equals_lower("true") || Value.equals_lower("on") ||
        Value.equals_lower("yes") || Value == "1") {
      Result = true;
      return true;
    } else if (Value.equals_lower("false") || Value.equals_lower("off") ||
               Value.equals_lower("no") || Value == "0") {
      Result = false;
      return true;
    }

    error(N, "expected boolean value");
    return false;
  }

  struct KeyStatus {
    bool Required;
    bool Seen = false;

    KeyStatus(bool Required = false) : Required(Required) {}
  };

  using KeyStatusPair = std::pair<StringRef, KeyStatus>;

  // false on error
  bool checkDuplicateOrUnknownKey(yaml::Node *KeyNode, StringRef Key,
                                  DenseMap<StringRef, KeyStatus> &Keys) {
    if (!Keys.count(Key)) {
      error(KeyNode, "unknown key");
      return false;
    }
    KeyStatus &S = Keys[Key];
    if (S.Seen) {
      error(KeyNode, Twine("duplicate key '") + Key + "'");
      return false;
    }
    S.Seen = true;
    return true;
  }

  // false on error
  bool checkMissingKeys(yaml::Node *Obj, DenseMap<StringRef, KeyStatus> &Keys) {
    for (const auto &I : Keys) {
      if (I.second.Required && !I.second.Seen) {
        error(Obj, Twine("missing key '") + I.first + "'");
        return false;
      }
    }
    return true;
  }

  RedirectingFileSystem::Entry *
  lookupOrCreateEntry(RedirectingFileSystem *FS, StringRef Name,
                      RedirectingFileSystem::Entry *ParentEntry = nullptr) {
    if (!ParentEntry) { // Look for a existent root
      for (const auto &Root : FS->Roots) {
        if (Name.equals(Root->getName())) {
          ParentEntry = Root.get();
          return ParentEntry;
        }
      }
    } else { // Advance to the next component
      auto *DE = dyn_cast<RedirectingFileSystem::RedirectingDirectoryEntry>(
          ParentEntry);
      for (std::unique_ptr<RedirectingFileSystem::Entry> &Content :
           llvm::make_range(DE->contents_begin(), DE->contents_end())) {
        auto *DirContent =
            dyn_cast<RedirectingFileSystem::RedirectingDirectoryEntry>(
                Content.get());
        if (DirContent && Name.equals(Content->getName()))
          return DirContent;
      }
    }

    // ... or create a new one
    std::unique_ptr<RedirectingFileSystem::Entry> E =
        std::make_unique<RedirectingFileSystem::RedirectingDirectoryEntry>(
            Name, Status("", getNextVirtualUniqueID(),
                         std::chrono::system_clock::now(), 0, 0, 0,
                         file_type::directory_file, sys::fs::all_all));

    if (!ParentEntry) { // Add a new root to the overlay
      FS->Roots.push_back(std::move(E));
      ParentEntry = FS->Roots.back().get();
      return ParentEntry;
    }

    auto *DE =
        cast<RedirectingFileSystem::RedirectingDirectoryEntry>(ParentEntry);
    DE->addContent(std::move(E));
    return DE->getLastContent();
  }

  void uniqueOverlayTree(RedirectingFileSystem *FS,
                         RedirectingFileSystem::Entry *SrcE,
                         RedirectingFileSystem::Entry *NewParentE = nullptr) {
    StringRef Name = SrcE->getName();
    switch (SrcE->getKind()) {
    case RedirectingFileSystem::EK_Directory: {
      auto *DE = cast<RedirectingFileSystem::RedirectingDirectoryEntry>(SrcE);
      // Empty directories could be present in the YAML as a way to
      // describe a file for a current directory after some of its subdir
      // is parsed. This only leads to redundant walks, ignore it.
      if (!Name.empty())
        NewParentE = lookupOrCreateEntry(FS, Name, NewParentE);
      for (std::unique_ptr<RedirectingFileSystem::Entry> &SubEntry :
           llvm::make_range(DE->contents_begin(), DE->contents_end()))
        uniqueOverlayTree(FS, SubEntry.get(), NewParentE);
      break;
    }
    case RedirectingFileSystem::EK_File: {
      assert(NewParentE && "Parent entry must exist");
      auto *FE = cast<RedirectingFileSystem::RedirectingFileEntry>(SrcE);
      auto *DE =
          cast<RedirectingFileSystem::RedirectingDirectoryEntry>(NewParentE);
      DE->addContent(
          std::make_unique<RedirectingFileSystem::RedirectingFileEntry>(
              Name, FE->getExternalContentsPath(), FE->getUseName()));
      break;
    }
    }
  }

  std::unique_ptr<RedirectingFileSystem::Entry>
  parseEntry(yaml::Node *N, RedirectingFileSystem *FS, bool IsRootEntry) {
    auto *M = dyn_cast<yaml::MappingNode>(N);
    if (!M) {
      error(N, "expected mapping node for file or directory entry");
      return nullptr;
    }

    KeyStatusPair Fields[] = {
        KeyStatusPair("name", true),
        KeyStatusPair("type", true),
        KeyStatusPair("contents", false),
        KeyStatusPair("external-contents", false),
        KeyStatusPair("use-external-name", false),
    };

    DenseMap<StringRef, KeyStatus> Keys(std::begin(Fields), std::end(Fields));

    bool HasContents = false; // external or otherwise
    std::vector<std::unique_ptr<RedirectingFileSystem::Entry>>
        EntryArrayContents;
    SmallString<256> ExternalContentsPath;
    SmallString<256> Name;
    yaml::Node *NameValueNode = nullptr;
    auto UseExternalName =
        RedirectingFileSystem::RedirectingFileEntry::NK_NotSet;
    RedirectingFileSystem::EntryKind Kind;

    for (auto &I : *M) {
      StringRef Key;
      // Reuse the buffer for key and value, since we don't look at key after
      // parsing value.
      SmallString<256> Buffer;
      if (!parseScalarString(I.getKey(), Key, Buffer))
        return nullptr;

      if (!checkDuplicateOrUnknownKey(I.getKey(), Key, Keys))
        return nullptr;

      StringRef Value;
      if (Key == "name") {
        if (!parseScalarString(I.getValue(), Value, Buffer))
          return nullptr;

        NameValueNode = I.getValue();
        // Guarantee that old YAML files containing paths with ".." and "."
        // are properly canonicalized before read into the VFS.
        Name = canonicalize(Value).str();
      } else if (Key == "type") {
        if (!parseScalarString(I.getValue(), Value, Buffer))
          return nullptr;
        if (Value == "file")
          Kind = RedirectingFileSystem::EK_File;
        else if (Value == "directory")
          Kind = RedirectingFileSystem::EK_Directory;
        else {
          error(I.getValue(), "unknown value for 'type'");
          return nullptr;
        }
      } else if (Key == "contents") {
        if (HasContents) {
          error(I.getKey(),
                "entry already has 'contents' or 'external-contents'");
          return nullptr;
        }
        HasContents = true;
        auto *Contents = dyn_cast<yaml::SequenceNode>(I.getValue());
        if (!Contents) {
          // FIXME: this is only for directories, what about files?
          error(I.getValue(), "expected array");
          return nullptr;
        }

        for (auto &I : *Contents) {
          if (std::unique_ptr<RedirectingFileSystem::Entry> E =
                  parseEntry(&I, FS, /*IsRootEntry*/ false))
            EntryArrayContents.push_back(std::move(E));
          else
            return nullptr;
        }
      } else if (Key == "external-contents") {
        if (HasContents) {
          error(I.getKey(),
                "entry already has 'contents' or 'external-contents'");
          return nullptr;
        }
        HasContents = true;
        if (!parseScalarString(I.getValue(), Value, Buffer))
          return nullptr;

        SmallString<256> FullPath;
        if (FS->IsRelativeOverlay) {
          FullPath = FS->getExternalContentsPrefixDir();
          assert(!FullPath.empty() &&
                 "External contents prefix directory must exist");
          llvm::sys::path::append(FullPath, Value);
        } else {
          FullPath = Value;
        }

        // Guarantee that old YAML files containing paths with ".." and "."
        // are properly canonicalized before read into the VFS.
        FullPath = canonicalize(FullPath);
        ExternalContentsPath = FullPath.str();
      } else if (Key == "use-external-name") {
        bool Val;
        if (!parseScalarBool(I.getValue(), Val))
          return nullptr;
        UseExternalName =
            Val ? RedirectingFileSystem::RedirectingFileEntry::NK_External
                : RedirectingFileSystem::RedirectingFileEntry::NK_Virtual;
      } else {
        llvm_unreachable("key missing from Keys");
      }
    }

    if (Stream.failed())
      return nullptr;

    // check for missing keys
    if (!HasContents) {
      error(N, "missing key 'contents' or 'external-contents'");
      return nullptr;
    }
    if (!checkMissingKeys(N, Keys))
      return nullptr;

    // check invalid configuration
    if (Kind == RedirectingFileSystem::EK_Directory &&
        UseExternalName !=
            RedirectingFileSystem::RedirectingFileEntry::NK_NotSet) {
      error(N, "'use-external-name' is not supported for directories");
      return nullptr;
    }

    sys::path::Style path_style = sys::path::Style::native;
    if (IsRootEntry) {
      // VFS root entries may be in either Posix or Windows style.  Figure out
      // which style we have, and use it consistently.
      if (sys::path::is_absolute(Name, sys::path::Style::posix)) {
        path_style = sys::path::Style::posix;
      } else if (sys::path::is_absolute(Name, sys::path::Style::windows)) {
        path_style = sys::path::Style::windows;
      } else {
        assert(NameValueNode && "Name presence should be checked earlier");
        error(NameValueNode,
              "entry with relative path at the root level is not discoverable");
        return nullptr;
      }
    }

    // Remove trailing slash(es), being careful not to remove the root path
    StringRef Trimmed(Name);
    size_t RootPathLen = sys::path::root_path(Trimmed, path_style).size();
    while (Trimmed.size() > RootPathLen &&
           sys::path::is_separator(Trimmed.back(), path_style))
      Trimmed = Trimmed.slice(0, Trimmed.size() - 1);

    // Get the last component
    StringRef LastComponent = sys::path::filename(Trimmed, path_style);

    std::unique_ptr<RedirectingFileSystem::Entry> Result;
    switch (Kind) {
    case RedirectingFileSystem::EK_File:
      Result = std::make_unique<RedirectingFileSystem::RedirectingFileEntry>(
          LastComponent, std::move(ExternalContentsPath), UseExternalName);
      break;
    case RedirectingFileSystem::EK_Directory:
      Result =
          std::make_unique<RedirectingFileSystem::RedirectingDirectoryEntry>(
              LastComponent, std::move(EntryArrayContents),
              Status("", getNextVirtualUniqueID(),
                     std::chrono::system_clock::now(), 0, 0, 0,
                     file_type::directory_file, sys::fs::all_all));
      break;
    }

    StringRef Parent = sys::path::parent_path(Trimmed, path_style);
    if (Parent.empty())
      return Result;

    // if 'name' contains multiple components, create implicit directory entries
    for (sys::path::reverse_iterator I = sys::path::rbegin(Parent, path_style),
                                     E = sys::path::rend(Parent);
         I != E; ++I) {
      std::vector<std::unique_ptr<RedirectingFileSystem::Entry>> Entries;
      Entries.push_back(std::move(Result));
      Result =
          std::make_unique<RedirectingFileSystem::RedirectingDirectoryEntry>(
              *I, std::move(Entries),
              Status("", getNextVirtualUniqueID(),
                     std::chrono::system_clock::now(), 0, 0, 0,
                     file_type::directory_file, sys::fs::all_all));
    }
    return Result;
  }

public:
  RedirectingFileSystemParser(yaml::Stream &S) : Stream(S) {}

  // false on error
  bool parse(yaml::Node *Root, RedirectingFileSystem *FS) {
    auto *Top = dyn_cast<yaml::MappingNode>(Root);
    if (!Top) {
      error(Root, "expected mapping node");
      return false;
    }

    KeyStatusPair Fields[] = {
        KeyStatusPair("version", true),
        KeyStatusPair("case-sensitive", false),
        KeyStatusPair("use-external-names", false),
        KeyStatusPair("overlay-relative", false),
        KeyStatusPair("fallthrough", false),
        KeyStatusPair("roots", true),
    };

    DenseMap<StringRef, KeyStatus> Keys(std::begin(Fields), std::end(Fields));
    std::vector<std::unique_ptr<RedirectingFileSystem::Entry>> RootEntries;

    // Parse configuration and 'roots'
    for (auto &I : *Top) {
      SmallString<10> KeyBuffer;
      StringRef Key;
      if (!parseScalarString(I.getKey(), Key, KeyBuffer))
        return false;

      if (!checkDuplicateOrUnknownKey(I.getKey(), Key, Keys))
        return false;

      if (Key == "roots") {
        auto *Roots = dyn_cast<yaml::SequenceNode>(I.getValue());
        if (!Roots) {
          error(I.getValue(), "expected array");
          return false;
        }

        for (auto &I : *Roots) {
          if (std::unique_ptr<RedirectingFileSystem::Entry> E =
                  parseEntry(&I, FS, /*IsRootEntry*/ true))
            RootEntries.push_back(std::move(E));
          else
            return false;
        }
      } else if (Key == "version") {
        StringRef VersionString;
        SmallString<4> Storage;
        if (!parseScalarString(I.getValue(), VersionString, Storage))
          return false;
        int Version;
        if (VersionString.getAsInteger<int>(10, Version)) {
          error(I.getValue(), "expected integer");
          return false;
        }
        if (Version < 0) {
          error(I.getValue(), "invalid version number");
          return false;
        }
        if (Version != 0) {
          error(I.getValue(), "version mismatch, expected 0");
          return false;
        }
      } else if (Key == "case-sensitive") {
        if (!parseScalarBool(I.getValue(), FS->CaseSensitive))
          return false;
      } else if (Key == "overlay-relative") {
        if (!parseScalarBool(I.getValue(), FS->IsRelativeOverlay))
          return false;
      } else if (Key == "use-external-names") {
        if (!parseScalarBool(I.getValue(), FS->UseExternalNames))
          return false;
      } else if (Key == "fallthrough") {
        if (!parseScalarBool(I.getValue(), FS->IsFallthrough))
          return false;
      } else {
        llvm_unreachable("key missing from Keys");
      }
    }

    if (Stream.failed())
      return false;

    if (!checkMissingKeys(Top, Keys))
      return false;

    // Now that we sucessefully parsed the YAML file, canonicalize the internal
    // representation to a proper directory tree so that we can search faster
    // inside the VFS.
    for (auto &E : RootEntries)
      uniqueOverlayTree(FS, E.get());

    return true;
  }
};

RedirectingFileSystem *
RedirectingFileSystem::create(std::unique_ptr<MemoryBuffer> Buffer,
                              SourceMgr::DiagHandlerTy DiagHandler,
                              StringRef YAMLFilePath, void *DiagContext,
                              IntrusiveRefCntPtr<FileSystem> ExternalFS) {
  SourceMgr SM;
  yaml::Stream Stream(Buffer->getMemBufferRef(), SM);

  SM.setDiagHandler(DiagHandler, DiagContext);
  yaml::document_iterator DI = Stream.begin();
  yaml::Node *Root = DI->getRoot();
  if (DI == Stream.end() || !Root) {
    SM.PrintMessage(SMLoc(), SourceMgr::DK_Error, "expected root node");
    return nullptr;
  }

  RedirectingFileSystemParser P(Stream);

  std::unique_ptr<RedirectingFileSystem> FS(
      new RedirectingFileSystem(ExternalFS));

  if (!YAMLFilePath.empty()) {
    // Use the YAML path from -ivfsoverlay to compute the dir to be prefixed
    // to each 'external-contents' path.
    //
    // Example:
    //    -ivfsoverlay dummy.cache/vfs/vfs.yaml
    // yields:
    //  FS->ExternalContentsPrefixDir => /<absolute_path_to>/dummy.cache/vfs
    //
    SmallString<256> OverlayAbsDir = sys::path::parent_path(YAMLFilePath);
    std::error_code EC = llvm::sys::fs::make_absolute(OverlayAbsDir);
    assert(!EC && "Overlay dir final path must be absolute");
    (void)EC;
    FS->setExternalContentsPrefixDir(OverlayAbsDir);
  }

  if (!P.parse(Root, FS.get()))
    return nullptr;

  return FS.release();
}

ErrorOr<RedirectingFileSystem::Entry *>
RedirectingFileSystem::lookupPath(const Twine &Path_) const {
  SmallString<256> Path;
  Path_.toVector(Path);

  // Handle relative paths
  if (std::error_code EC = makeAbsolute(Path))
    return EC;

  // Canonicalize path by removing ".", "..", "./", components. This is
  // a VFS request, do not bother about symlinks in the path components
  // but canonicalize in order to perform the correct entry search.
  Path = canonicalize(Path);
  if (Path.empty())
    return make_error_code(llvm::errc::invalid_argument);

  sys::path::const_iterator Start = sys::path::begin(Path);
  sys::path::const_iterator End = sys::path::end(Path);
  for (const auto &Root : Roots) {
    ErrorOr<RedirectingFileSystem::Entry *> Result =
        lookupPath(Start, End, Root.get());
    if (Result || Result.getError() != llvm::errc::no_such_file_or_directory)
      return Result;
  }
  return make_error_code(llvm::errc::no_such_file_or_directory);
}

ErrorOr<RedirectingFileSystem::Entry *>
RedirectingFileSystem::lookupPath(sys::path::const_iterator Start,
                                  sys::path::const_iterator End,
                                  RedirectingFileSystem::Entry *From) const {
  assert(!isTraversalComponent(*Start) &&
         !isTraversalComponent(From->getName()) &&
         "Paths should not contain traversal components");

  StringRef FromName = From->getName();

  // Forward the search to the next component in case this is an empty one.
  if (!FromName.empty()) {
    if (!pathComponentMatches(*Start, FromName))
      return make_error_code(llvm::errc::no_such_file_or_directory);

    ++Start;

    if (Start == End) {
      // Match!
      return From;
    }
  }

  auto *DE = dyn_cast<RedirectingFileSystem::RedirectingDirectoryEntry>(From);
  if (!DE)
    return make_error_code(llvm::errc::not_a_directory);

  for (const std::unique_ptr<RedirectingFileSystem::Entry> &DirEntry :
       llvm::make_range(DE->contents_begin(), DE->contents_end())) {
    ErrorOr<RedirectingFileSystem::Entry *> Result =
        lookupPath(Start, End, DirEntry.get());
    if (Result || Result.getError() != llvm::errc::no_such_file_or_directory)
      return Result;
  }

  return make_error_code(llvm::errc::no_such_file_or_directory);
}

static Status getRedirectedFileStatus(const Twine &Path, bool UseExternalNames,
                                      Status ExternalStatus) {
  Status S = ExternalStatus;
  if (!UseExternalNames)
    S = Status::copyWithNewName(S, Path);
  S.IsVFSMapped = true;
  return S;
}

ErrorOr<Status> RedirectingFileSystem::status(const Twine &Path,
                                              RedirectingFileSystem::Entry *E) {
  assert(E != nullptr);
  if (auto *F = dyn_cast<RedirectingFileSystem::RedirectingFileEntry>(E)) {
    ErrorOr<Status> S = ExternalFS->status(F->getExternalContentsPath());
    assert(!S || S->getName() == F->getExternalContentsPath());
    if (S)
      return getRedirectedFileStatus(Path, F->useExternalName(UseExternalNames),
                                     *S);
    return S;
  } else { // directory
    auto *DE = cast<RedirectingFileSystem::RedirectingDirectoryEntry>(E);
    return Status::copyWithNewName(DE->getStatus(), Path);
  }
}

ErrorOr<Status> RedirectingFileSystem::status(const Twine &Path) {
  ErrorOr<RedirectingFileSystem::Entry *> Result = lookupPath(Path);
  if (!Result) {
    if (shouldUseExternalFS() &&
        Result.getError() == llvm::errc::no_such_file_or_directory) {
      return ExternalFS->status(Path);
    }
    return Result.getError();
  }
  return status(Path, *Result);
}

namespace {

/// Provide a file wrapper with an overriden status.
class FileWithFixedStatus : public File {
  std::unique_ptr<File> InnerFile;
  Status S;

public:
  FileWithFixedStatus(std::unique_ptr<File> InnerFile, Status S)
      : InnerFile(std::move(InnerFile)), S(std::move(S)) {}

  ErrorOr<Status> status() override { return S; }
  ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>

  getBuffer(const Twine &Name, int64_t FileSize, bool RequiresNullTerminator,
            bool IsVolatile) override {
    return InnerFile->getBuffer(Name, FileSize, RequiresNullTerminator,
                                IsVolatile);
  }

  std::error_code close() override { return InnerFile->close(); }
};

} // namespace

ErrorOr<std::unique_ptr<File>>
RedirectingFileSystem::openFileForRead(const Twine &Path) {
  ErrorOr<RedirectingFileSystem::Entry *> E = lookupPath(Path);
  if (!E) {
    if (shouldUseExternalFS() &&
        E.getError() == llvm::errc::no_such_file_or_directory) {
      return ExternalFS->openFileForRead(Path);
    }
    return E.getError();
  }

  auto *F = dyn_cast<RedirectingFileSystem::RedirectingFileEntry>(*E);
  if (!F) // FIXME: errc::not_a_file?
    return make_error_code(llvm::errc::invalid_argument);

  auto Result = ExternalFS->openFileForRead(F->getExternalContentsPath());
  if (!Result)
    return Result;

  auto ExternalStatus = (*Result)->status();
  if (!ExternalStatus)
    return ExternalStatus.getError();

  // FIXME: Update the status with the name and VFSMapped.
  Status S = getRedirectedFileStatus(Path, F->useExternalName(UseExternalNames),
                                     *ExternalStatus);
  return std::unique_ptr<File>(
      std::make_unique<FileWithFixedStatus>(std::move(*Result), S));
}

std::error_code
RedirectingFileSystem::getRealPath(const Twine &Path,
                                   SmallVectorImpl<char> &Output) const {
  ErrorOr<RedirectingFileSystem::Entry *> Result = lookupPath(Path);
  if (!Result) {
    if (shouldUseExternalFS() &&
        Result.getError() == llvm::errc::no_such_file_or_directory) {
      return ExternalFS->getRealPath(Path, Output);
    }
    return Result.getError();
  }

  if (auto *F =
          dyn_cast<RedirectingFileSystem::RedirectingFileEntry>(*Result)) {
    return ExternalFS->getRealPath(F->getExternalContentsPath(), Output);
  }
  // Even if there is a directory entry, fall back to ExternalFS if allowed,
  // because directories don't have a single external contents path.
  return shouldUseExternalFS() ? ExternalFS->getRealPath(Path, Output)
                               : llvm::errc::invalid_argument;
}

IntrusiveRefCntPtr<FileSystem>
vfs::getVFSFromYAML(std::unique_ptr<MemoryBuffer> Buffer,
                    SourceMgr::DiagHandlerTy DiagHandler,
                    StringRef YAMLFilePath, void *DiagContext,
                    IntrusiveRefCntPtr<FileSystem> ExternalFS) {
  return RedirectingFileSystem::create(std::move(Buffer), DiagHandler,
                                       YAMLFilePath, DiagContext,
                                       std::move(ExternalFS));
}

static void getVFSEntries(RedirectingFileSystem::Entry *SrcE,
                          SmallVectorImpl<StringRef> &Path,
                          SmallVectorImpl<YAMLVFSEntry> &Entries) {
  auto Kind = SrcE->getKind();
  if (Kind == RedirectingFileSystem::EK_Directory) {
    auto *DE = dyn_cast<RedirectingFileSystem::RedirectingDirectoryEntry>(SrcE);
    assert(DE && "Must be a directory");
    for (std::unique_ptr<RedirectingFileSystem::Entry> &SubEntry :
         llvm::make_range(DE->contents_begin(), DE->contents_end())) {
      Path.push_back(SubEntry->getName());
      getVFSEntries(SubEntry.get(), Path, Entries);
      Path.pop_back();
    }
    return;
  }

  assert(Kind == RedirectingFileSystem::EK_File && "Must be a EK_File");
  auto *FE = dyn_cast<RedirectingFileSystem::RedirectingFileEntry>(SrcE);
  assert(FE && "Must be a file");
  SmallString<128> VPath;
  for (auto &Comp : Path)
    llvm::sys::path::append(VPath, Comp);
  Entries.push_back(YAMLVFSEntry(VPath.c_str(), FE->getExternalContentsPath()));
}

void vfs::collectVFSFromYAML(std::unique_ptr<MemoryBuffer> Buffer,
                             SourceMgr::DiagHandlerTy DiagHandler,
                             StringRef YAMLFilePath,
                             SmallVectorImpl<YAMLVFSEntry> &CollectedEntries,
                             void *DiagContext,
                             IntrusiveRefCntPtr<FileSystem> ExternalFS) {
  RedirectingFileSystem *VFS = RedirectingFileSystem::create(
      std::move(Buffer), DiagHandler, YAMLFilePath, DiagContext,
      std::move(ExternalFS));
  ErrorOr<RedirectingFileSystem::Entry *> RootE = VFS->lookupPath("/");
  if (!RootE)
    return;
  SmallVector<StringRef, 8> Components;
  Components.push_back("/");
  getVFSEntries(*RootE, Components, CollectedEntries);
}

UniqueID vfs::getNextVirtualUniqueID() {
  static std::atomic<unsigned> UID;
  unsigned ID = ++UID;
  // The following assumes that uint64_t max will never collide with a real
  // dev_t value from the OS.
  return UniqueID(std::numeric_limits<uint64_t>::max(), ID);
}

void YAMLVFSWriter::addEntry(StringRef VirtualPath, StringRef RealPath,
                             bool IsDirectory) {
  assert(sys::path::is_absolute(VirtualPath) && "virtual path not absolute");
  assert(sys::path::is_absolute(RealPath) && "real path not absolute");
  assert(!pathHasTraversal(VirtualPath) && "path traversal is not supported");
  Mappings.emplace_back(VirtualPath, RealPath, IsDirectory);
}

void YAMLVFSWriter::addFileMapping(StringRef VirtualPath, StringRef RealPath) {
  addEntry(VirtualPath, RealPath, /*IsDirectory=*/false);
}

void YAMLVFSWriter::addDirectoryMapping(StringRef VirtualPath,
                                        StringRef RealPath) {
  addEntry(VirtualPath, RealPath, /*IsDirectory=*/true);
}

namespace {

class JSONWriter {
  llvm::raw_ostream &OS;
  SmallVector<StringRef, 16> DirStack;

  unsigned getDirIndent() { return 4 * DirStack.size(); }
  unsigned getFileIndent() { return 4 * (DirStack.size() + 1); }
  bool containedIn(StringRef Parent, StringRef Path);
  StringRef containedPart(StringRef Parent, StringRef Path);
  void startDirectory(StringRef Path);
  void endDirectory();
  void writeEntry(StringRef VPath, StringRef RPath);

public:
  JSONWriter(llvm::raw_ostream &OS) : OS(OS) {}

  void write(ArrayRef<YAMLVFSEntry> Entries, Optional<bool> UseExternalNames,
             Optional<bool> IsCaseSensitive, Optional<bool> IsOverlayRelative,
             StringRef OverlayDir);
};

} // namespace

bool JSONWriter::containedIn(StringRef Parent, StringRef Path) {
  using namespace llvm::sys;

  // Compare each path component.
  auto IParent = path::begin(Parent), EParent = path::end(Parent);
  for (auto IChild = path::begin(Path), EChild = path::end(Path);
       IParent != EParent && IChild != EChild; ++IParent, ++IChild) {
    if (*IParent != *IChild)
      return false;
  }
  // Have we exhausted the parent path?
  return IParent == EParent;
}

StringRef JSONWriter::containedPart(StringRef Parent, StringRef Path) {
  assert(!Parent.empty());
  assert(containedIn(Parent, Path));
  return Path.slice(Parent.size() + 1, StringRef::npos);
}

void JSONWriter::startDirectory(StringRef Path) {
  StringRef Name =
      DirStack.empty() ? Path : containedPart(DirStack.back(), Path);
  DirStack.push_back(Path);
  unsigned Indent = getDirIndent();
  OS.indent(Indent) << "{\n";
  OS.indent(Indent + 2) << "'type': 'directory',\n";
  OS.indent(Indent + 2) << "'name': \"" << llvm::yaml::escape(Name) << "\",\n";
  OS.indent(Indent + 2) << "'contents': [\n";
}

void JSONWriter::endDirectory() {
  unsigned Indent = getDirIndent();
  OS.indent(Indent + 2) << "]\n";
  OS.indent(Indent) << "}";

  DirStack.pop_back();
}

void JSONWriter::writeEntry(StringRef VPath, StringRef RPath) {
  unsigned Indent = getFileIndent();
  OS.indent(Indent) << "{\n";
  OS.indent(Indent + 2) << "'type': 'file',\n";
  OS.indent(Indent + 2) << "'name': \"" << llvm::yaml::escape(VPath) << "\",\n";
  OS.indent(Indent + 2) << "'external-contents': \""
                        << llvm::yaml::escape(RPath) << "\"\n";
  OS.indent(Indent) << "}";
}

void JSONWriter::write(ArrayRef<YAMLVFSEntry> Entries,
                       Optional<bool> UseExternalNames,
                       Optional<bool> IsCaseSensitive,
                       Optional<bool> IsOverlayRelative,
                       StringRef OverlayDir) {
  using namespace llvm::sys;

  OS << "{\n"
        "  'version': 0,\n";
  if (IsCaseSensitive.hasValue())
    OS << "  'case-sensitive': '"
       << (IsCaseSensitive.getValue() ? "true" : "false") << "',\n";
  if (UseExternalNames.hasValue())
    OS << "  'use-external-names': '"
       << (UseExternalNames.getValue() ? "true" : "false") << "',\n";
  bool UseOverlayRelative = false;
  if (IsOverlayRelative.hasValue()) {
    UseOverlayRelative = IsOverlayRelative.getValue();
    OS << "  'overlay-relative': '" << (UseOverlayRelative ? "true" : "false")
       << "',\n";
  }
  OS << "  'roots': [\n";

  if (!Entries.empty()) {
    const YAMLVFSEntry &Entry = Entries.front();
    bool first_entry_is_directory = Entry.IsDirectory;
    StringRef Dir =
        first_entry_is_directory ? Entry.VPath : path::parent_path(Entry.VPath);
    startDirectory(Dir);

    StringRef RPath = Entry.RPath;
    if (UseOverlayRelative) {
      unsigned OverlayDirLen = OverlayDir.size();
      assert(RPath.substr(0, OverlayDirLen) == OverlayDir &&
             "Overlay dir must be contained in RPath");
      RPath = RPath.slice(OverlayDirLen, RPath.size());
    }

    if (!first_entry_is_directory)
      writeEntry(path::filename(Entry.VPath), RPath);

    for (const auto &Entry : Entries.slice(1)) {
      StringRef Dir =
          Entry.IsDirectory ? Entry.VPath : path::parent_path(Entry.VPath);
      if (Dir == DirStack.back()) {
        if (!first_entry_is_directory) {
          OS << ",\n";
          first_entry_is_directory = false;
        }
      } else {
        while (!DirStack.empty() && !containedIn(DirStack.back(), Dir)) {
          OS << "\n";
          endDirectory();
        }
        OS << ",\n";
        startDirectory(Dir);
      }
      StringRef RPath = Entry.RPath;
      if (UseOverlayRelative) {
        unsigned OverlayDirLen = OverlayDir.size();
        assert(RPath.substr(0, OverlayDirLen) == OverlayDir &&
               "Overlay dir must be contained in RPath");
        RPath = RPath.slice(OverlayDirLen, RPath.size());
      }
      if (!Entry.IsDirectory)
        writeEntry(path::filename(Entry.VPath), RPath);
    }

    while (!DirStack.empty()) {
      OS << "\n";
      endDirectory();
    }
    OS << "\n";
  }

  OS << "  ]\n"
     << "}\n";
}

void YAMLVFSWriter::write(llvm::raw_ostream &OS) {
  llvm::sort(Mappings, [](const YAMLVFSEntry &LHS, const YAMLVFSEntry &RHS) {
    return LHS.VPath < RHS.VPath;
  });

  JSONWriter(OS).write(Mappings, UseExternalNames, IsCaseSensitive,
                       IsOverlayRelative, OverlayDir);
}

VFSFromYamlDirIterImpl::VFSFromYamlDirIterImpl(
    const Twine &_Path,
    RedirectingFileSystem::RedirectingDirectoryEntry::iterator Begin,
    RedirectingFileSystem::RedirectingDirectoryEntry::iterator End,
    bool IterateExternalFS, FileSystem &ExternalFS, std::error_code &EC)
    : Dir(_Path.str()), Current(Begin), End(End),
      IterateExternalFS(IterateExternalFS), ExternalFS(ExternalFS) {
  EC = incrementImpl(/*IsFirstTime=*/true);
}

std::error_code VFSFromYamlDirIterImpl::increment() {
  return incrementImpl(/*IsFirstTime=*/false);
}

std::error_code VFSFromYamlDirIterImpl::incrementExternal() {
  assert(!(IsExternalFSCurrent && ExternalDirIter == directory_iterator()) &&
         "incrementing past end");
  std::error_code EC;
  if (IsExternalFSCurrent) {
    ExternalDirIter.increment(EC);
  } else if (IterateExternalFS) {
    ExternalDirIter = ExternalFS.dir_begin(Dir, EC);
    IsExternalFSCurrent = true;
    if (EC && EC != errc::no_such_file_or_directory)
      return EC;
    EC = {};
  }
  if (EC || ExternalDirIter == directory_iterator()) {
    CurrentEntry = directory_entry();
  } else {
    CurrentEntry = *ExternalDirIter;
  }
  return EC;
}

std::error_code VFSFromYamlDirIterImpl::incrementContent(bool IsFirstTime) {
  assert((IsFirstTime || Current != End) && "cannot iterate past end");
  if (!IsFirstTime)
    ++Current;
  while (Current != End) {
    SmallString<128> PathStr(Dir);
    llvm::sys::path::append(PathStr, (*Current)->getName());
    sys::fs::file_type Type = sys::fs::file_type::type_unknown;
    switch ((*Current)->getKind()) {
    case RedirectingFileSystem::EK_Directory:
      Type = sys::fs::file_type::directory_file;
      break;
    case RedirectingFileSystem::EK_File:
      Type = sys::fs::file_type::regular_file;
      break;
    }
    CurrentEntry = directory_entry(std::string(PathStr.str()), Type);
    return {};
  }
  return incrementExternal();
}

std::error_code VFSFromYamlDirIterImpl::incrementImpl(bool IsFirstTime) {
  while (true) {
    std::error_code EC = IsExternalFSCurrent ? incrementExternal()
                                             : incrementContent(IsFirstTime);
    if (EC || CurrentEntry.path().empty())
      return EC;
    StringRef Name = llvm::sys::path::filename(CurrentEntry.path());
    if (SeenNames.insert(Name).second)
      return EC; // name not seen before
  }
  llvm_unreachable("returned above");
}

vfs::recursive_directory_iterator::recursive_directory_iterator(
    FileSystem &FS_, const Twine &Path, std::error_code &EC)
    : FS(&FS_) {
  directory_iterator I = FS->dir_begin(Path, EC);
  if (I != directory_iterator()) {
    State = std::make_shared<detail::RecDirIterState>();
    State->Stack.push(I);
  }
}

vfs::recursive_directory_iterator &
recursive_directory_iterator::increment(std::error_code &EC) {
  assert(FS && State && !State->Stack.empty() && "incrementing past end");
  assert(!State->Stack.top()->path().empty() && "non-canonical end iterator");
  vfs::directory_iterator End;

  if (State->HasNoPushRequest)
    State->HasNoPushRequest = false;
  else {
    if (State->Stack.top()->type() == sys::fs::file_type::directory_file) {
      vfs::directory_iterator I = FS->dir_begin(State->Stack.top()->path(), EC);
      if (I != End) {
        State->Stack.push(I);
        return *this;
      }
    }
  }

  while (!State->Stack.empty() && State->Stack.top().increment(EC) == End)
    State->Stack.pop();

  if (State->Stack.empty())
    State.reset(); // end iterator

  return *this;
}
