//===----------------------------------------------------------------------===//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

const char *usage =
    "  swift-stdlib-tool --print [options...]\n"
    "    Find and print the Swift libraries required by an app.\n"
    "\n"
    "  swift-stdlib-tool --copy [options...]\n"
    "    Copy the Swift libraries into an app bundle, and optionally sign them.\n"
    "\n"
    "  --verbose\n"
    "      Print progress.\n"
    "  --verbose --verbose\n"
    "      Print debugging details.\n"
    "  --help\n"
    "      Print usage.\n"
    "\n"
    "  Options for lookup:\n"
    "  --scan-executable <path>\n"
    "      Scan the executable at <path> for references to Swift libraries.\n"
    "      This option may be set multiple times.\n"
    "  --scan-folder <path>\n"
    "      Scan any executables inside <path> for references to Swift libraries.\n"
    "      This option may be set multiple times.\n"
    "  --platform <macosx|iphoneos|iphonesimulator>\n"
    "      Use the Swift libraries for <platform>.\n"
    "  --source-libraries <path>\n"
    "      Search <path> for Swift libraries.\n"
    "      The default is /path/to/swift-stdlib-tool/../../lib/swift/<platform>/\n"
    "\n"
    "  Options for copying and signing:\n"
    "  --destination <path>\n"
    "      Copy Swift libraries into <path>.\n"
    "  --unsigned-destination <path>\n"
    "      Copy Swift libraries into <path> without signing them.\n"
    "  --sign <identity>\n"
    "      Sign copied Swift libraries using <identity>.\n"
    "  --keychain <keychain>\n"
    "      Search <keychain> for the code signing identity.\n"
    "  --Xcodesign <option>\n"
    "      Pass <option> to the codesign tool.\n"
    "  --strip-bitcode\n"
    "      Remove embedded bitcode from libraries copied to --destination.\n"
    "      Libraries copied to --unsigned-destination are unmodified.\n"
    "\n"
    "  Options for libraries copied as resources\n"
    "  --resource-library <library>\n"
    "      Copy <library> and its dependencies as resources without signing\n"
    "      them. These copies are in addition to any libraries copied as a result\n"
    "      of the --scan-executable option.\n"
    "      Any library in the Swift library search path can be specified for\n"
    "      <library>.\n"
    "      This option may be set multiple times.\n"
    "  --resource-destination <path>\n"
    "      The <path> to copy Swift resource libraries to.\n"
    "\n";

#include <copyfile.h>
#include <dirent.h>
#include <dispatch/dispatch.h>
#include <errno.h>
#include <fcntl.h>
#include <libgen.h>
#include <libkern/OSByteOrder.h>
#include <mach-o/dyld.h>
#include <mach-o/fat.h>
#include <mach-o/loader.h>
#include <os/overflow.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <uuid/uuid.h>

#include <algorithm>
#include <mutex>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#pragma clang diagnostic ignored "-Wgcc-compat"

#ifndef CPU_TYPE_ARM64
#define CPU_TYPE_ARM64 ((cpu_type_t)(CPU_TYPE_ARM | CPU_ARCH_ABI64))
#endif

static int Verbose = 0;

#ifdef __OPTIMIZE__
#define INLINE __attribute__((always_inline))
#else
#define INLINE
#endif

//
// This abstraction layer is for use with file formats that have 64-bit/32-bit and Big-Endian/Little-Endian variants
//
// For example: to make a utility that handles 32-bit little enidan files use:  Pointer32<LittleEndian>
//
//
//		get16()			read a 16-bit number from an E endian struct
//		set16()			write a 16-bit number to an E endian struct
//		get32()			read a 32-bit number from an E endian struct
//		set32()			write a 32-bit number to an E endian struct
//		get64()			read a 64-bit number from an E endian struct
//		set64()			write a 64-bit number to an E endian struct
//
//		getBits()		read a bit field from an E endian struct (bitCount=number of bits in field, firstBit=bit index of field)
//		setBits()		write a bit field to an E endian struct (bitCount=number of bits in field, firstBit=bit index of field)
//
//		getBitsRaw()	read a bit field from a struct with native endianness
//		setBitsRaw()	write a bit field from a struct with native endianness
//

class BigEndian {
public:
  static uint16_t get16(const uint16_t &from) INLINE {
    return OSReadBigInt16(&from, 0);
  }
  static void set16(uint16_t &into, uint16_t value) INLINE {
    OSWriteBigInt16(&into, 0, value);
  }

  static uint32_t get32(const uint32_t &from) INLINE {
    return OSReadBigInt32(&from, 0);
  }
  static void set32(uint32_t &into, uint32_t value) INLINE {
    OSWriteBigInt32(&into, 0, value);
  }

  static uint64_t get64(const uint64_t &from) INLINE {
    return OSReadBigInt64(&from, 0);
  }
  static void set64(uint64_t &into, uint64_t value) INLINE {
    OSWriteBigInt64(&into, 0, value);
  }

  static uint32_t getBits(const uint32_t &from, uint8_t firstBit,
                          uint8_t bitCount) INLINE {
    return getBitsRaw(get32(from), firstBit, bitCount);
  }
  static void setBits(uint32_t &into, uint32_t value, uint8_t firstBit,
                      uint8_t bitCount) INLINE {
    uint32_t temp = get32(into);
    setBitsRaw(temp, value, firstBit, bitCount);
    set32(into, temp);
  }

  static uint32_t getBitsRaw(const uint32_t &from, uint8_t firstBit,
                             uint8_t bitCount) INLINE {
    return ((from >> (32 - firstBit - bitCount)) & ((1 << bitCount) - 1));
  }
  static void setBitsRaw(uint32_t &into, uint32_t value, uint8_t firstBit,
                         uint8_t bitCount) INLINE {
    uint32_t temp = into;
    const uint32_t mask = ((1 << bitCount) - 1);
    temp &= ~(mask << (32 - firstBit - bitCount));
    temp |= ((value & mask) << (32 - firstBit - bitCount));
    into = temp;
  }
  enum { little_endian = 0 };
};

class LittleEndian {
public:
  static uint16_t get16(const uint16_t &from) INLINE {
    return OSReadLittleInt16(&from, 0);
  }
  static void set16(uint16_t &into, uint16_t value) INLINE {
    OSWriteLittleInt16(&into, 0, value);
  }

  static uint32_t get32(const uint32_t &from) INLINE {
    return OSReadLittleInt32(&from, 0);
  }
  static void set32(uint32_t &into, uint32_t value) INLINE {
    OSWriteLittleInt32(&into, 0, value);
  }

  static uint64_t get64(const uint64_t &from) INLINE {
    return OSReadLittleInt64(&from, 0);
  }
  static void set64(uint64_t &into, uint64_t value) INLINE {
    OSWriteLittleInt64(&into, 0, value);
  }

  static uint32_t getBits(const uint32_t &from, uint8_t firstBit,
                          uint8_t bitCount) INLINE {
    return getBitsRaw(get32(from), firstBit, bitCount);
  }
  static void setBits(uint32_t &into, uint32_t value, uint8_t firstBit,
                      uint8_t bitCount) INLINE {
    uint32_t temp = get32(into);
    setBitsRaw(temp, value, firstBit, bitCount);
    set32(into, temp);
  }

  static uint32_t getBitsRaw(const uint32_t &from, uint8_t firstBit,
                             uint8_t bitCount) INLINE {
    return ((from >> firstBit) & ((1 << bitCount) - 1));
  }
  static void setBitsRaw(uint32_t &into, uint32_t value, uint8_t firstBit,
                         uint8_t bitCount) INLINE {
    uint32_t temp = into;
    const uint32_t mask = ((1 << bitCount) - 1);
    temp &= ~(mask << firstBit);
    temp |= ((value & mask) << firstBit);
    into = temp;
  }
  enum { little_endian = 1 };
};

#if __BIG_ENDIAN__
typedef BigEndian CurrentEndian;
typedef LittleEndian OtherEndian;
#elif __LITTLE_ENDIAN__
typedef LittleEndian CurrentEndian;
typedef BigEndian OtherEndian;
#else
#error unknown endianness
#endif

template <typename _E>
class Pointer32 {
public:
  typedef uint32_t uint_t;
  typedef int32_t sint_t;
  typedef _E E;

  static uint64_t getP(const uint_t &from) INLINE { return _E::get32(from); }
  static void setP(uint_t &into, uint64_t value) INLINE {
    _E::set32(into, value);
  }
};

template <typename _E>
class Pointer64 {
public:
  typedef uint64_t uint_t;
  typedef int64_t sint_t;
  typedef _E E;

  static uint64_t getP(const uint_t &from) INLINE { return _E::get64(from); }
  static void setP(uint_t &into, uint64_t value) INLINE {
    _E::set64(into, value);
  }
};

//
// mach-o file header
//
template <typename P>
struct macho_header_content {};
template <>
struct macho_header_content<Pointer32<BigEndian>> {
  mach_header fields;
};
template <>
struct macho_header_content<Pointer64<BigEndian>> {
  mach_header_64 fields;
};
template <>
struct macho_header_content<Pointer32<LittleEndian>> {
  mach_header fields;
};
template <>
struct macho_header_content<Pointer64<LittleEndian>> {
  mach_header_64 fields;
};

template <typename P>
class macho_header {
public:
  uint32_t magic() const INLINE { return E::get32(header.fields.magic); }
  void set_magic(uint32_t value) INLINE {
    E::set32(header.fields.magic, value);
  }

  uint32_t cputype() const INLINE { return E::get32(header.fields.cputype); }
  void set_cputype(uint32_t value) INLINE {
    E::set32((uint32_t &)header.fields.cputype, value);
  }

  uint32_t cpusubtype() const INLINE {
    return E::get32(header.fields.cpusubtype);
  }
  void set_cpusubtype(uint32_t value) INLINE {
    E::set32((uint32_t &)header.fields.cpusubtype, value);
  }

  uint32_t filetype() const INLINE { return E::get32(header.fields.filetype); }
  void set_filetype(uint32_t value) INLINE {
    E::set32(header.fields.filetype, value);
  }

  uint32_t ncmds() const INLINE { return E::get32(header.fields.ncmds); }
  void set_ncmds(uint32_t value) INLINE {
    E::set32(header.fields.ncmds, value);
  }

  uint32_t sizeofcmds() const INLINE {
    return E::get32(header.fields.sizeofcmds);
  }
  void set_sizeofcmds(uint32_t value) INLINE {
    E::set32(header.fields.sizeofcmds, value);
  }

  uint32_t flags() const INLINE { return E::get32(header.fields.flags); }
  void set_flags(uint32_t value) INLINE {
    E::set32(header.fields.flags, value);
  }

  uint32_t reserved() const INLINE { return E::get32(header.fields.reserved); }
  void set_reserved(uint32_t value) INLINE {
    E::set32(header.fields.reserved, value);
  }

  typedef typename P::E E;

private:
  macho_header_content<P> header;
};

//
// mach-o load command
//
template <typename P>
class macho_load_command {
public:
  uint32_t cmd() const INLINE { return E::get32(command.cmd); }
  void set_cmd(uint32_t value) INLINE { E::set32(command.cmd, value); }

  uint32_t cmdsize() const INLINE { return E::get32(command.cmdsize); }
  void set_cmdsize(uint32_t value) INLINE { E::set32(command.cmdsize, value); }

  typedef typename P::E E;

private:
  load_command command;
};

//
// mach-o uuid load command
//
template <typename P>
class macho_uuid_command {
public:
  uint32_t cmd() const INLINE { return E::get32(fields.cmd); }
  void set_cmd(uint32_t value) INLINE { E::set32(fields.cmd, value); }

  uint32_t cmdsize() const INLINE { return E::get32(fields.cmdsize); }
  void set_cmdsize(uint32_t value) INLINE { E::set32(fields.cmdsize, value); }

  const uint8_t *uuid() const INLINE { return fields.uuid; }
  void set_uuid(uint8_t value[16]) INLINE { memcpy(&fields.uuid, value, 16); }

  typedef typename P::E E;

private:
  uuid_command fields;
};

//
// mach-o dylib load command
//
template <typename P>
class macho_dylib_command {
public:
  uint32_t cmd() const INLINE { return E::get32(fields.cmd); }
  void set_cmd(uint32_t value) INLINE { E::set32(fields.cmd, value); }

  uint32_t cmdsize() const INLINE { return E::get32(fields.cmdsize); }
  void set_cmdsize(uint32_t value) INLINE { E::set32(fields.cmdsize, value); }

  uint32_t name_offset() const INLINE {
    return E::get32(fields.dylib.name.offset);
  }
  void set_name_offset(uint32_t value) INLINE {
    E::set32(fields.dylib.name.offset, value);
  }

  uint32_t timestamp() const INLINE { return E::get32(fields.dylib.timestamp); }
  void set_timestamp(uint32_t value) INLINE {
    E::set32(fields.dylib.timestamp, value);
  }

  uint32_t current_version() const INLINE {
    return E::get32(fields.dylib.current_version);
  }
  void set_current_version(uint32_t value) INLINE {
    E::set32(fields.dylib.current_version, value);
  }

  uint32_t compatibility_version() const INLINE {
    return E::get32(fields.dylib.compatibility_version);
  }
  void set_compatibility_version(uint32_t value) INLINE {
    E::set32(fields.dylib.compatibility_version, value);
  }

  const char *name() const INLINE {
    return (const char *)&fields + name_offset();
  }
  void set_name_offset() INLINE { set_name_offset(sizeof(fields)); }

  typedef typename P::E E;

private:
  dylib_command fields;
};

// Print everything to standard output.
// Mixing stdout and stderr looks bad when the output is reprinted by Xcode.

void printUsage() { fprintf(stdout, "%s", usage); }

void fail(const char *msg, ...) __attribute__((format(printf, 1, 2)))
__attribute__((noreturn)) {
  va_list args;
  va_start(args, msg);
  char *msg2;
  asprintf(&msg2, "*** error: %s\n", msg);
  vfprintf(stdout, msg2, args);
  exit(1);
}

void fail_errno(const char *msg, ...) __attribute__((format(printf, 1, 2)))
__attribute__((noreturn)) {
  va_list args;
  va_start(args, msg);
  char *msg2;
  asprintf(&msg2, "*** error: %s: %s\n", msg, strerror(errno));
  vfprintf(stdout, msg2, args);
  exit(1);
}

void fail_usage(const char *msg, ...) __attribute__((format(printf, 1, 2)))
__attribute__((noreturn)) {
  va_list args;
  va_start(args, msg);
  char *msg2;
  asprintf(&msg2, "*** error: %s\n\n", msg);
  vfprintf(stdout, msg2, args);
  printUsage();
  exit(1);
}

void log_vn(int verbosity, const char *msg, va_list args)
    __attribute__((format(printf, 2, 0))) {
  if (verbosity <= Verbose) {
    char *msg2;
    asprintf(&msg2, "%s\n", msg);
    vfprintf(stdout, msg2, args);
    free(msg2);
  }
}

int log_v(const char *msg, ...) __attribute__((format(printf, 1, 2))) {
  va_list args;
  va_start(args, msg);
  log_vn(1, msg, args);
  return -1;
}

int log_vv(const char *msg, ...) __attribute__((format(printf, 1, 2))) {
  va_list args;
  va_start(args, msg);
  log_vn(2, msg, args);
  return -1;
}

ssize_t pread_all(int fd, void *buf, size_t count, off_t offset) {
  size_t total = 0;
  while (total < count) {
    ssize_t readed =
        pread(fd, (void *)((char *)buf + total), count - total, offset + total);
    if (readed > 0)
      total += readed; // got data
    else if (readed == 0)
      return total; // EOF: done
    else if (readed == -1 && errno != EINTR)
      return -1;
    // error but not EINTR: fail
  }

  return total;
}

template <typename T>
int parse_macho(int fd, uint32_t offset, uint32_t size,
                void (^dylibVisitor)(const std::string &path),
                void (^uuidVisitor)(const uuid_t uuid)) {
  ssize_t readed;

  macho_header<T> mh;
  if (size < sizeof(mh))
    return log_vv("file is too small");
  readed = pread_all(fd, &mh, sizeof(mh), offset);
  if (readed != sizeof(mh))
    return log_vv("pread failed");

  uint32_t sizeofcmds = mh.sizeofcmds();
  size -= sizeof(mh);
  offset += sizeof(mh);
  if (size < sizeofcmds)
    return log_vv("file is badly formed");

  uint8_t *cmdp = (uint8_t *)malloc(sizeofcmds);
  if (!cmdp)
    return log_vv("malloc(sizeofcmds) failed");

  readed = pread_all(fd, cmdp, sizeofcmds, offset);
  if (readed == sizeofcmds) {
    uint8_t *cmds = cmdp;
    for (uint32_t c = 0; c < mh.ncmds(); c++) {
      macho_load_command<T> *cmd;
      if (size < sizeof(*cmd))
        return log_vv("file is badly formed");
      cmd = (macho_load_command<T> *)cmds;
      if (size < cmd->cmdsize())
        return log_vv("file is badly formed");
      cmds += cmd->cmdsize();
      size -= cmd->cmdsize();

      if (dylibVisitor &&
          (cmd->cmd() == LC_LOAD_DYLIB || cmd->cmd() == LC_LOAD_WEAK_DYLIB ||
           cmd->cmd() == LC_LAZY_LOAD_DYLIB)) {
        macho_dylib_command<T> *dylib = (macho_dylib_command<T> *)cmd;
        if (dylib->cmdsize() < dylib->name_offset())
          continue;
        char *name = (char *)dylib + dylib->name_offset();
        size_t name_len =
            strnlen(name, dylib->cmdsize() - dylib->name_offset());
        log_vv("  loads %.*s", (int)name_len, name);

#define PREPREFIX "@rpath/"
#define PREFIX PREPREFIX "libswift"
        if (0 == strncmp(name, PREFIX, strlen(PREFIX))) {
          dylibVisitor(name + strlen(PREPREFIX));
        }
      } else if (uuidVisitor && cmd->cmd() == LC_UUID) {
        macho_uuid_command<T> *uuid_cmd = (macho_uuid_command<T> *)cmd;
        if (uuid_cmd->cmdsize() < sizeof(uuid_command))
          continue;
        uuidVisitor(uuid_cmd->uuid());
      }
    }
  }
  free(cmdp);

  return 0;
}

int parse_macho(int fd, uint32_t offset, uint32_t size,
                void (^dylibVisitor)(const std::string &path),
                void (^uuidVisitor)(const uuid_t uuid)) {
  uint32_t magic;
  if (size < sizeof(magic))
    return log_vv("file is too small");
  ssize_t readed = pread_all(fd, &magic, sizeof(magic), offset);
  if (readed != sizeof(magic))
    return log_vv("pread failed");

  switch (magic) {
  case MH_MAGIC_64:
    return parse_macho<Pointer64<CurrentEndian>>(fd, offset, size, dylibVisitor,
                                                 uuidVisitor);
  case MH_MAGIC:
    return parse_macho<Pointer32<CurrentEndian>>(fd, offset, size, dylibVisitor,
                                                 uuidVisitor);
  case MH_CIGAM_64:
    return parse_macho<Pointer64<OtherEndian>>(fd, offset, size, dylibVisitor,
                                               uuidVisitor);
  case MH_CIGAM:
    return parse_macho<Pointer32<OtherEndian>>(fd, offset, size, dylibVisitor,
                                               uuidVisitor);
  default:
    return log_vv("file is not mach-o");
  }
}

int parse_fat(int fd, off_t fsize, char *buffer, size_t size,
              void (^dylibVisitor)(const std::string &path),
              void (^uuidVisitor)(const uuid_t uuid)) {
  uint32_t magic;

  if (size < sizeof(magic)) {
    return log_vv("file is too small");
  }

  magic = *(uint32_t *)buffer;
  if (magic == FAT_MAGIC || magic == FAT_CIGAM) {
    struct fat_header *fh;
    uint32_t fat_nfat_arch;
    struct fat_arch *archs;
    uint32_t i;

    if (size < sizeof(struct fat_header)) {
      return log_vv("file is too small");
    }

    fh = (struct fat_header *)buffer;
    fat_nfat_arch = OSSwapBigToHostInt32(fh->nfat_arch);

    size_t fat_arch_size;
    // fat_nfat_arch * sizeof(struct fat_arch) + sizeof(struct fat_header)
    if (os_mul_and_add_overflow(fat_nfat_arch, sizeof(struct fat_arch),
                                sizeof(struct fat_header), &fat_arch_size)) {
      return log_vv("too many fat archs\n");
    }
    if (size < fat_arch_size) {
      return log_vv("file is too small");
    }

    archs = (struct fat_arch *)(buffer + sizeof(struct fat_header));

    /* Special case hidden CPU_TYPE_ARM64 */
    size_t fat_arch_plus_one_size;
    if (os_add_overflow(fat_arch_size, sizeof(struct fat_arch),
                        &fat_arch_plus_one_size)) {
      return log_vv("too many fat archs\n");
    }
    if (size >= fat_arch_plus_one_size) {
      if (fat_nfat_arch > 0 &&
          OSSwapBigToHostInt32(archs[fat_nfat_arch].cputype) ==
              CPU_TYPE_ARM64) {
        fat_nfat_arch++;
      }
    }
    /* End special case hidden CPU_TYPE_ARM64 */

    for (i = 0; i < fat_nfat_arch; i++) {
      int ret;
      uint32_t arch_offset, arch_size;

      arch_offset = OSSwapBigToHostInt32(archs[i].offset);
      arch_size = OSSwapBigToHostInt32(archs[i].size);

      /* Check that slice data is after all fat headers and archs */
      if (arch_offset < fat_arch_size) {
        return log_vv("file is badly formed");
      }

      /* Check that the slice ends before the file does */
      if (arch_offset > fsize) {
        return log_vv("file is badly formed");
      }

      if (arch_size > fsize) {
        return log_vv("file is badly formed");
      }

      if (arch_offset > (fsize - arch_size)) {
        return log_vv("file is badly formed");
      }

      ret = parse_macho(fd, arch_offset, arch_size, dylibVisitor, uuidVisitor);
      if (ret != 0) {
        return ret;
      }
    }
    return 0;
  } else {
    /* Not a fat file */
    return parse_macho(fd, 0, fsize, dylibVisitor, uuidVisitor);
  }
}

void process(const std::string &path, void (^dylibVisitor)(const std::string &),
             void (^uuidVisitor)(const uuid_t)) {
  log_vv("Scanning %s...", path.c_str());

  int fd = open(path.c_str(), O_RDONLY);
  if (fd < 0)
    log_vv("%s: open failed: %s", path.c_str(), strerror(errno));

  struct stat st;
  if (fstat(fd, &st) < 0) {
    log_vv("%s: stat failed: %s", path.c_str(), strerror(errno));
  } else {
    const int len = 4096;
    char buf[len];
    ssize_t readed = pread_all(fd, buf, len, 0);
    if (readed != len) {
      log_vv("%s: pread failed: %s", path.c_str(), strerror(errno));
    } else {
      parse_fat(fd, st.st_size, buf, len, dylibVisitor, uuidVisitor);
    }
  }
  close(fd);
}

bool operator<=(const struct timespec &lhs, const struct timespec &rhs) {
  if (lhs.tv_sec == rhs.tv_sec)
    return lhs.tv_nsec <= rhs.tv_nsec;
  return lhs.tv_sec <= rhs.tv_sec;
}

std::string parentPath(std::string path) {
  const char *pathCstr = path.c_str();
  char parent[MAXPATHLEN];

  return dirname_r(pathCstr, parent) ? parent : pathCstr;
}

std::string filename(std::string path) {
  const char *pathCstr = path.c_str();
  char filename[MAXPATHLEN];

  return basename_r(pathCstr, filename) ? filename : pathCstr;
}

bool directory_exists(const std::string &path) {
  struct stat st;
  return stat(path.c_str(), &st) == 0 && S_ISDIR(st.st_mode);
}

// This executable's own path.
std::string self_executable = []() -> std::string {
  char path[MAXPATHLEN] = {0};
  uint32_t len = sizeof(path);
  _NSGetExecutablePath(path, &len);
  return std::string(path);
}();

// This executable's own xctoolchain path.
std::string self_toolchain = []() -> std::string {
  auto result = self_executable;

  // Remove the executable name.
  result = parentPath(result);

  // Remove trailing /usr/bin, if any
  if (filename(result) == "bin") {
    result = parentPath(result);
  }
  if (filename(result) == "usr") {
    result = parentPath(result);
  }
  return result;
}();

std::vector<uint8_t> readToEOF(int fd) {
  std::vector<uint8_t> retData;
#define BUFFER_SIZE 1024
  uint8_t readBuffer[BUFFER_SIZE];
  ssize_t readSize = 0;
  while ((readSize = read(fd, readBuffer, BUFFER_SIZE)) > 0) {
    retData.reserve(retData.size() + readSize);
    std::copy_n(readBuffer, readSize, std::back_inserter(retData));
  }
  return retData;
}

// Runs a tool with `xcrun`.
// Returns the tool's termination status.
// Prints the tool's command line if we are verbose.
// Prints the tool's stdout and stderr if terminationStatus is non-zero
//   or if we are very verbose.
typedef void (^XcrunToolBlock)(std::vector<uint8_t> stdOutData,
                               std::vector<uint8_t> stdErrorData, int err);

int xcrunToolCommand(std::vector<std::string> commandAndArguments,
                     XcrunToolBlock block = 0) {
  const char *launchPath = "/usr/bin/xcrun";

  // Tell xcrun to search our toolchain first.
  std::vector<const char *> arguments;
  arguments.push_back(launchPath);
  arguments.push_back("--toolchain");
  arguments.push_back(self_toolchain.c_str());

  // Tell xcrun to print its command if we are very verbose.
  if (Verbose > 1) {
    arguments.push_back("--log");
  }

  for (const auto &string : commandAndArguments) {
    arguments.push_back(string.c_str());
  }
  arguments.push_back(NULL);

  int outPipe[2];
  int errPipe[2];

  pipe(outPipe);
  pipe(errPipe);

  log_v("  %s", launchPath);

  int childPid = fork();

  if (childPid == 0) {
    dup2(outPipe[1], STDOUT_FILENO);
    dup2(errPipe[1], STDERR_FILENO);

    close(outPipe[0]);
    close(errPipe[0]);

    execv(launchPath, const_cast<char **>(arguments.data()));
  }
  close(outPipe[1]);
  close(errPipe[1]);

  // Read stdout and stderr in parallel, then wait for the task
  // to exit. Anything else risks deadlock if the task fills
  // one of the output buffers.

  int errPipeReadFd = errPipe[1];
  __block std::vector<uint8_t> stdErrData;
  dispatch_semaphore_t gotStdErr = dispatch_semaphore_create(0);
  dispatch_queue_t concurrentQueue =
      dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0);
  dispatch_async(concurrentQueue, ^{
    stdErrData = readToEOF(errPipeReadFd);
    dispatch_semaphore_signal(gotStdErr);
  });

  const auto stdOutData = readToEOF(outPipe[0]);
  close(outPipe[0]);

  dispatch_semaphore_wait(gotStdErr, DISPATCH_TIME_FOREVER);
  dispatch_release(gotStdErr);
  close(errPipe[0]);

  int status = 0;
  waitpid(childPid, &status, 0);
  status = WIFSIGNALED(status) ? WTERMSIG(status)
                               : (WIFEXITED(status) ? WEXITSTATUS(status) : 0);

  // Task is finished and we have its stdout and stderr output.

  // Print its stdout and stderr if it failed or we are verbose.
  // (Print nothing by default because codesign is noisy.)

  if (status || Verbose > 1) {
    fwrite(stdErrData.data(), stdErrData.size(), 1, stdout);
    fwrite(stdOutData.data(), stdOutData.size(), 1, stdout);
  }
  if (block) {
    block(stdOutData, stdErrData, status);
  }
  return status;
}

void copyAndStripBitcode(std::string src, std::string dst) {
  // -r removes bitcode
  std::vector<std::string> commandAndArgs = {"bitcode_strip", src, "-r", "-o",
                                             dst};
  int err = xcrunToolCommand(commandAndArgs);

  // Fail if bitcode_strip failed.
  if (err) {
    fail("Couldn't copy and strip bitcode %s to %s: bitcode_strip failed "
         "with exit code %d",
         src.c_str(), dst.c_str(), err);
  }
}

void copyFile(std::string src, std::string dst, bool stripBitcode) {
  if (stripBitcode) {
    copyAndStripBitcode(src, dst);
  } else {
    if (copyfile(src.c_str(), dst.c_str(), NULL, COPYFILE_ALL) != 0) {
      fail("Couldn't copy %s to %s: %s", src.c_str(), dst.c_str(),
           strerror(errno));
    }
  }
}

std::string uuidString(const uuid_t uuid) {
  char buffer[37];
  uuid_unparse(uuid, buffer);
  return buffer;
}

void copyLibraries(
    std::string dst_dir,
    const std::unordered_map<std::string, std::unordered_set<std::string>>
        &libs,
    bool stripBitcode) {
  mkpath_np(dst_dir.c_str(), S_IRWXU | S_IRWXG | S_IRWXO);

  for (const auto &pair : libs) {
    const auto &src = pair.first;
    const auto &srcUUIDs = pair.second;

    std::string dst = dst_dir + "/" + filename(src);

    // Compare UUIDs of src and dst and don't copy if they're the same.
    // Do not use mod times for this task: the dst copy gets code-signed
    // and bitcode-stripped so it can look newer than it really is.
    __block std::unordered_set<std::string> dstUUIDs;
    process(dst, NULL, ^(const uuid_t uuid) {
      dstUUIDs.insert(uuidString(uuid));
    });

    std::string srcUUIDsString;
    srcUUIDsString.reserve(37 * srcUUIDs.size());

    for (const auto &uuidString : srcUUIDs) {
      srcUUIDsString.append(uuidString + std::string(" "));
    }

    std::string dstUUIDsString;
    dstUUIDsString.reserve(37 * dstUUIDs.size());

    for (const auto &uuidString : dstUUIDs) {
      dstUUIDsString.append(uuidString + " ");
    }

    log_vv("Source UUIDs %s: %s", src.c_str(), srcUUIDsString.c_str());
    log_vv("Destination UUIDs %s: %s", dst.c_str(), dstUUIDsString.c_str());

    if (srcUUIDs == dstUUIDs) {
      log_v("%s is up to date at %s", src.c_str(), dst.c_str());
      continue;
    }

    // Perform the copy.

    log_v("Copying %s to %s", src.c_str(), dst_dir.c_str());

    unlink(dst.c_str());
    copyFile(src, dst, stripBitcode);
  }
}

std::vector<uint8_t> query_code_signature(std::string file) {
  __block std::vector<uint8_t> d;
  std::vector<std::string> command = {"codesign", "-r-", "--display", file};
  log_v("Probing signature of %s", file.c_str());
  xcrunToolCommand(command, ^(std::vector<uint8_t> stdOutData,
                              std::vector<uint8_t> stdErrData, int err) {
    if (!err) {
      d = stdOutData;
    }
  });
  return d;
}

template <typename F>
void listDirectoryContents(std::string directory, F &&func) {
  DIR *dir = opendir(directory.c_str());
  if (dir == NULL) {
    return;
  }

  struct dirent *entry;
  while ((entry = readdir(dir))) {
    func(directory + "/" + entry->d_name);
  }
  closedir(dir);
}

template <typename F>
void recursivelyListFiles(std::string directory, F &&func) {
  DIR *dir = opendir(directory.c_str());
  if (dir == NULL) {
    return;
  }

  std::vector<std::string> subpaths;
  struct dirent *entry;
  while ((entry = readdir(dir)) != NULL) {
    std::string path = directory + "/" + entry->d_name;
    if (entry->d_type == DT_REG) {
      func(path);
    } else if (entry->d_type == DT_DIR) {
      // check if . or ..
      if (strncmp(entry->d_name, "..", entry->d_namlen) == 0) {
        continue;
      }
      subpaths.push_back(path);
    }
  }
  closedir(dir);
  for (const auto &path : subpaths) {
    recursivelyListFiles(path, func);
  }
}

int main(int argc, const char *argv[]) {
  // Executables to scan for Swift references.
  // --scan-executable
  std::vector<std::string> executables;

  // Directories to scan for more executables.
  // --scan-folder
  std::vector<std::string> embedDirs;

  // Platform name.
  // --platform
  // or the last path component of --source-libraries
  std::string platform;

  // Copy source.
  // --source-libraries
  // or /path/to/swift-stdlib-tool/../../lib/swift/<--platform>
  std::vector<std::string> src_dirs;

  // Copy destinations, signed and unsigned.
  // --destination and --unsigned-destination
  std::string dst_dir;
  std::string unsigned_dst_dir;

  // Resource copy destination.
  // --resource-destination
  std::string resource_dst_dir;

  // Resource libraries.
  // --resource-library
  std::vector<std::string> resourceLibraries;

  // Code signing options.
  std::string ident;
  std::string keychain;
  std::vector<std::string> otherCodesignFlags;

  // Read arguments
  bool print = false;
  bool copy = false;
  bool stripBitcode = false;
  for (int i = 1; i < argc; i++) {
    if (0 == strcmp(argv[i], "--print")) {
      print = true;
    } else if (0 == strcmp(argv[i], "--copy")) {
      copy = true;
    } else if (0 == strcmp(argv[i], "--verbose")) {
      Verbose++;
    } else if (0 == strcmp(argv[i], "--help")) {
      printUsage();
      exit(0);
    } else if (0 == strcmp(argv[i], "--scan-executable")) {
      executables.emplace_back(argv[++i]);
    } else if (0 == strcmp(argv[i], "--scan-folder")) {
      embedDirs.emplace_back(argv[++i]);
    } else if (0 == strcmp(argv[i], "--source-libraries")) {
      src_dirs.emplace_back(argv[++i]);
    } else if (0 == strcmp(argv[i], "--platform")) {
      platform = std::string(argv[++i]);
    } else if (0 == strcmp(argv[i], "--destination")) {
      dst_dir = std::string(argv[++i]);
    } else if (0 == strcmp(argv[i], "--unsigned-destination")) {
      unsigned_dst_dir = std::string(argv[++i]);
    } else if (0 == strcmp(argv[i], "--sign")) {
      ident = std::string(argv[++i]);
    } else if (0 == strcmp(argv[i], "--keychain")) {
      keychain = std::string(argv[++i]);
    } else if (0 == strcmp(argv[i], "--Xcodesign")) {
      otherCodesignFlags.push_back(std::string(argv[++i]));
    } else if (0 == strcmp(argv[i], "--strip-bitcode")) {
      stripBitcode = true;
    } else if (0 == strcmp(argv[i], "--resource-destination")) {
      resource_dst_dir = std::string(argv[++i]);
    } else if (0 == strcmp(argv[i], "--resource-library")) {
      resourceLibraries.push_back(std::string(argv[++i]));
    } else {
      fail("Unknown argument: %s", argv[i]);
    }
  }

  // Fix up src_dirs and platform values.
  if (src_dirs.empty() && platform.empty()) {
    // Neither src_dirs nor platform is set. Die.
    fail_usage("At least one of --source-libraries and --platform "
               "must be set.");
  } else if (src_dirs.empty()) {
    // platform is set but src_dirs is not.
    // Use platform to set src_dirs relative to us.
    std::string root_path =
        parentPath(parentPath(self_executable)) + "/" + "lib";

    listDirectoryContents(root_path, [&](std::string entry) {
      if (filename(entry).compare(0, strlen("swift-"), "swift-") == 0) {
        src_dirs.push_back(entry + "/" + platform);
      }
    });

    if (src_dirs.empty()) {
      fail("Couldn't discover Swift library directories in: %s",
           root_path.c_str());
    }
  } else if (platform.empty()) {
    // src_dirs is set but platform is not.
    // Pick platform from any src_dirs's name.
    platform = filename(src_dirs.front());
  }

  for (const auto &src_dir : src_dirs) {
    if (!directory_exists(src_dir))
      fail("Source directory does not exist: %s", src_dir.c_str());
  }

  // Add the platform to unsigned_dst_dir if it is not already present.
  if (!unsigned_dst_dir.empty()) {
    const auto unsigned_platform = unsigned_dst_dir;
    if (platform != unsigned_platform) {
      unsigned_dst_dir = unsigned_dst_dir + "/" + platform;
    }
  }

  // If the user specifies --strip-bitcode but not --sign, this
  // will cause the dylibs to get copied, stripped, but not resigned.
  // This will cause apps to fail to launch because the code signature
  // is invalid.  In this case, ignore --strip-bitcode.
  if (stripBitcode && ident.empty()) {
    stripBitcode = false;
  }

  // Collect executables from the --scan-folder locations.
  for (const auto &embedDir : embedDirs) {
    recursivelyListFiles(embedDir, [&](std::string entry) {
      if (0 == access(entry.c_str(), X_OK)) {
        executables.push_back(entry);
      } else {
        log_vv("%s is not an executable file", entry.c_str());
      }
    });
  }

  // Collect Swift library names from the input files.
  // If the library does not exist in src_dirs then assume the user wrote
  // their own library named libswift* and is handling it elsewhere.
  __block std::unordered_map<std::string, std::unordered_set<std::string>>
      swiftLibs;
  for (const auto &path : executables) {
    process(
        path,
        ^(const std::string &linkedLib) {
          for (const auto &src_dir : src_dirs) {
            const auto linkedSrc = src_dir + "/" + linkedLib;
            if (access(linkedSrc.c_str(), F_OK) == 0) {
              swiftLibs[linkedSrc] = std::unordered_set<std::string>();
              break;
            }
          }
        },
        NULL);
  }

  // Collect more Swift library names from the Swift libraries themselves.
  // Also collect the Swift libraries' UUIDs.
  __block std::vector<std::string> worklist;
  worklist.reserve(swiftLibs.size());
  for (const auto &pair : swiftLibs) {
    worklist.push_back(pair.first);
  }
  while (worklist.size()) {
    const auto path = worklist.back();
    worklist.pop_back();
    process(
        path,
        ^(const std::string &linkedLib) {
          for (const auto &src_dir : src_dirs) {
            const auto linkedSrc = src_dir + "/" + linkedLib;
            if (swiftLibs.count(linkedSrc) == 0 &&
                access(linkedSrc.c_str(), F_OK) == 0) {
              swiftLibs[linkedSrc] = std::unordered_set<std::string>();
              worklist.push_back(linkedSrc);
              break;
            }
          }
        },
        ^(const uuid_t uuid) {
          swiftLibs[path].insert(uuidString(uuid));
        });
  }

  // Collect all the Swift libraries that the user requested
  // with --resource-library.
  __block std::unordered_map<std::string, std::unordered_set<std::string>>
      swiftLibsForResources;
  for (const auto &lib : resourceLibraries) {
    for (const auto &src_dir : src_dirs) {
      const auto libSrc = src_dir + "/" + lib;
      if (access(libSrc.c_str(), F_OK) == 0) {
        swiftLibsForResources[libSrc] = std::unordered_set<std::string>();
      }
    }
  }

  // Collect dependencies of --resource-library libs.
  worklist.clear();
  for (const auto &pair : swiftLibsForResources) {
    worklist.push_back(pair.first);
  }
  while (worklist.size()) {
    const auto path = worklist.back();
    worklist.pop_back();
    process(
        path,
        ^(const std::string &linkedLib) {
          for (const auto &src_dir : src_dirs) {
            const auto linkedSrc = src_dir + "/" + linkedLib;
            if (swiftLibsForResources.count(linkedSrc) == 0 &&
                access(linkedSrc.c_str(), F_OK) == 0) {
              swiftLibsForResources[linkedSrc] =
                  std::unordered_set<std::string>();
              worklist.push_back(linkedSrc);
            }
          }
        },
        ^(const uuid_t uuid) {
          swiftLibsForResources[path].insert(uuidString(uuid));
        });
  }

  // Print the Swift libraries (full path to toolchain's copy)
  if (print) {
    for (const auto &lib : swiftLibs) {
      printf("%s\n", lib.first.c_str());
    }
  }

  // Copy the Swift libraries to $build_dir/$frameworks
  // and $build_dir/$unsigned_frameworks
  if (copy) {
    copyLibraries(dst_dir, swiftLibs, stripBitcode);
    if (!unsigned_dst_dir.empty()) {
      // Never strip bitcode from the unsigned libraries.
      // Their existing signatures must be preserved.
      copyLibraries(unsigned_dst_dir, swiftLibs, false);
    }

    if (!resource_dst_dir.empty()) {
      // Never strip bitcode from resources libraries, for
      // the same reason as the libraries copied to
      // unsigned_dst_dir.
      copyLibraries(resource_dst_dir, swiftLibsForResources, false);
    }
  }

  // Codesign the Swift libraries in $build_dir/$frameworks
  // but not the libraries in $build_dir/$unsigned_frameworks.
  if (!ident.empty()) {
    // Swift libraries that are up-to-date get codesigned anyway
    // (in case options changed or a previous build was incomplete).
    // We do employ an optimization, however, if resigning the dylib
    // results in getting the same signing identity and credentials
    // then we keep the original file to optimize for delta updates
    // to the device.

    __block bool signedOne = false;
    std::mutex signingLock;

    for (const auto &pair : swiftLibs) {
      const auto &lib = pair.first;
      // Work around authentication UI problems
      // by signing one synchronously and then signing the rest.
      signingLock.lock();
      if (signedOne) {
        // First signer is complete. Proceed concurrently.
        signingLock.unlock();
      } else {
        // We are the first signer. Hold the lock until we finish.
      }

      // Get the code signature, and copy the dylib to the side
      // to preserve it in case it does not change.  We can use
      // this to avoid unnecessary copies during delta installs
      // to devices.
      const auto dst = dst_dir + "/" + filename(lib);
      const auto oldSignatureData = query_code_signature(dst);
      const char *tmpFilePath = 0;
      if (!oldSignatureData.empty()) {
        // Make a copy of the existing file, with permissions and
        // mtime preserved.
        auto tmpFile = dst + ".original";
        tmpFilePath = tmpFile.c_str();
        xcrunToolCommand({"cp", "-p", dst, tmpFile});
      }

      // Proceed with (re-)codesigning.
      log_v("Codesigning %s at %s", lib.c_str(), dst_dir.c_str());

      // Build the codesign invocation.
      std::vector<std::string> commandAndArguments{
          "codesign", "--force", "--sign", ident, "--verbose"};

      if (!keychain.empty()) {
        commandAndArguments.push_back("--keychain");
        commandAndArguments.push_back(keychain);
      }

      // Other codesign flags come later
      // so they can override the default flags.
      std::copy(otherCodesignFlags.begin(), otherCodesignFlags.end(),
                std::back_inserter(commandAndArguments));

      commandAndArguments.push_back(dst);

      int err = xcrunToolCommand(commandAndArguments);

      // Fail if codesign failed.
      if (err) {
        // Clean up any temporary files.
        if (tmpFilePath)
          unlink(tmpFilePath);
        fail("Couldn't codesign %s: codesign failed with "
             "exit code %d",
             dst.c_str(), err);
      }

      // If we have an existing code signature data, query the new one and
      // compare it with the code signature of the file before we re-signed it.
      // If they are the same, use the original file instead.  This preserves
      // the contents of the file and mtime for use with delta installs.
      if (!oldSignatureData.empty()) {
        const auto newSignatureData = query_code_signature(dst);

#if 0
        // For Debugging.
        fprintf(stdout, "oldSignature (%lu bytes)\n",
                (unsigned long)oldSignatureData.size());
        fwrite(oldSignatureData.data(), oldSignatureData.size(), 1, stdout);
        fprintf(stdout, "\nnewSignature (%lu bytes)\n",
                (unsigned long)newSignatureData.size());
        fwrite(newSignatureData.data(), newSignatureData.size(), 1, stdout);
        fprintf(stdout, "\n");
        fflush(stdout);
#endif

        const auto newLength = newSignatureData.size();
        if (newLength == oldSignatureData.size() &&
            memcmp(newSignatureData.data(), oldSignatureData.data(),
                   newLength) == 0) {
          log_v("Code signature of %s is unchanged; keeping original",
                lib.c_str());
          // The two signatures match.  Unlink the new file, and re-link the old
          // file.
          const char *filePath = dst.c_str();
          unlink(filePath);
          link(tmpFilePath, filePath);
        }
      }
      // Clean up any temporary files.
      if (tmpFilePath) {
        unlink(tmpFilePath);
      }

      if (!signedOne) {
        // We are the first signer. Allow the others to proceed now.
        signedOne = true;
        signingLock.unlock();
      }
    };
  }
  exit(0);
}
