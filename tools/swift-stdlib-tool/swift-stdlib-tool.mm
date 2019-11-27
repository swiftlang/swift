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
    "\n"
    ;

#include <sys/types.h>
#include <sys/uio.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <os/overflow.h>
#include <mach-o/fat.h>
#include <mach-o/dyld.h>
#include <mach-o/loader.h>
#include <libkern/OSByteOrder.h>

#include <algorithm>
#include <vector>
using namespace std;

#include <Foundation/Foundation.h>

#pragma clang diagnostic ignored "-Wgcc-compat"

#ifndef CPU_TYPE_ARM64
#   define CPU_TYPE_ARM64  ((cpu_type_t) (CPU_TYPE_ARM | CPU_ARCH_ABI64))
#endif

static int Verbose = 0;

#ifdef __OPTIMIZE__
#define INLINE	__attribute__((always_inline))
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

class BigEndian
{
public:
	static uint16_t	get16(const uint16_t& from)				INLINE { return OSReadBigInt16(&from, 0); }
	static void		set16(uint16_t& into, uint16_t value)	INLINE { OSWriteBigInt16(&into, 0, value); }
	
	static uint32_t	get32(const uint32_t& from)				INLINE { return OSReadBigInt32(&from, 0); }
	static void		set32(uint32_t& into, uint32_t value)	INLINE { OSWriteBigInt32(&into, 0, value); }
	
	static uint64_t get64(const uint64_t& from)				INLINE { return OSReadBigInt64(&from, 0); }
	static void		set64(uint64_t& into, uint64_t value)	INLINE { OSWriteBigInt64(&into, 0, value); }
	
	static uint32_t	getBits(const uint32_t& from, 
						uint8_t firstBit, uint8_t bitCount)	INLINE { return getBitsRaw(get32(from), firstBit, bitCount); }
	static void		setBits(uint32_t& into, uint32_t value,
						uint8_t firstBit, uint8_t bitCount)	INLINE { uint32_t temp = get32(into); setBitsRaw(temp, value, firstBit, bitCount); set32(into, temp); }

	static uint32_t	getBitsRaw(const uint32_t& from, 
						uint8_t firstBit, uint8_t bitCount)	INLINE { return ((from >> (32-firstBit-bitCount)) & ((1<<bitCount)-1)); }
	static void		setBitsRaw(uint32_t& into, uint32_t value,
						uint8_t firstBit, uint8_t bitCount)	INLINE { uint32_t temp = into; 
																							const uint32_t mask = ((1<<bitCount)-1); 
																							temp &= ~(mask << (32-firstBit-bitCount)); 
																							temp |= ((value & mask) << (32-firstBit-bitCount)); 
																							into = temp; }
	enum { little_endian = 0 };
};


class LittleEndian
{
public:
	static uint16_t	get16(const uint16_t& from)				INLINE { return OSReadLittleInt16(&from, 0); }
	static void		set16(uint16_t& into, uint16_t value)	INLINE { OSWriteLittleInt16(&into, 0, value); }
	
	static uint32_t	get32(const uint32_t& from)				INLINE { return OSReadLittleInt32(&from, 0); }
	static void		set32(uint32_t& into, uint32_t value)	INLINE { OSWriteLittleInt32(&into, 0, value); }
	
	static uint64_t get64(const uint64_t& from)				INLINE { return OSReadLittleInt64(&from, 0); }
	static void		set64(uint64_t& into, uint64_t value)	INLINE { OSWriteLittleInt64(&into, 0, value); }

	static uint32_t	getBits(const uint32_t& from,
						uint8_t firstBit, uint8_t bitCount)	INLINE { return getBitsRaw(get32(from), firstBit, bitCount); }
	static void		setBits(uint32_t& into, uint32_t value,
						uint8_t firstBit, uint8_t bitCount)	INLINE { uint32_t temp = get32(into); setBitsRaw(temp, value, firstBit, bitCount); set32(into, temp); }

	static uint32_t	getBitsRaw(const uint32_t& from,
						uint8_t firstBit, uint8_t bitCount)	INLINE { return ((from >> firstBit) & ((1<<bitCount)-1)); }
	static void		setBitsRaw(uint32_t& into, uint32_t value,
						uint8_t firstBit, uint8_t bitCount)	INLINE {  uint32_t temp = into; 
																							const uint32_t mask = ((1<<bitCount)-1); 
																							temp &= ~(mask << firstBit); 
																							temp |= ((value & mask) << firstBit); 
																							into = temp; }
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
class Pointer32
{
public:
	typedef uint32_t	uint_t;
	typedef int32_t		sint_t;
	typedef _E			E;
	
	static uint64_t	getP(const uint_t& from)				INLINE { return _E::get32(from); }
	static void		setP(uint_t& into, uint64_t value)		INLINE { _E::set32(into, value); }
};


template <typename _E>
class Pointer64
{
public:
	typedef uint64_t	uint_t;
	typedef int64_t		sint_t;
	typedef _E			E;
	
	static uint64_t	getP(const uint_t& from)				INLINE { return _E::get64(from); }
	static void		setP(uint_t& into, uint64_t value)		INLINE { _E::set64(into, value); }
};


//
// mach-o file header
//
template <typename P> struct macho_header_content {};
template <> struct macho_header_content<Pointer32<BigEndian> >    { mach_header		fields; };
template <> struct macho_header_content<Pointer64<BigEndian> >	  { mach_header_64	fields; };
template <> struct macho_header_content<Pointer32<LittleEndian> > { mach_header		fields; };
template <> struct macho_header_content<Pointer64<LittleEndian> > { mach_header_64	fields; };

template <typename P>
class macho_header {
public:
	uint32_t		magic() const					INLINE { return E::get32(header.fields.magic); }
	void			set_magic(uint32_t value)		INLINE { E::set32(header.fields.magic, value); }

	uint32_t		cputype() const					INLINE { return E::get32(header.fields.cputype); }
	void			set_cputype(uint32_t value)		INLINE { E::set32((uint32_t&)header.fields.cputype, value); }

	uint32_t		cpusubtype() const				INLINE { return E::get32(header.fields.cpusubtype); }
	void			set_cpusubtype(uint32_t value)	INLINE { E::set32((uint32_t&)header.fields.cpusubtype, value); }

	uint32_t		filetype() const				INLINE { return E::get32(header.fields.filetype); }
	void			set_filetype(uint32_t value)	INLINE { E::set32(header.fields.filetype, value); }

	uint32_t		ncmds() const					INLINE { return E::get32(header.fields.ncmds); }
	void			set_ncmds(uint32_t value)		INLINE { E::set32(header.fields.ncmds, value); }

	uint32_t		sizeofcmds() const				INLINE { return E::get32(header.fields.sizeofcmds); }
	void			set_sizeofcmds(uint32_t value)	INLINE { E::set32(header.fields.sizeofcmds, value); }

	uint32_t		flags() const					INLINE { return E::get32(header.fields.flags); }
	void			set_flags(uint32_t value)		INLINE { E::set32(header.fields.flags, value); }

	uint32_t		reserved() const				INLINE { return E::get32(header.fields.reserved); }
	void			set_reserved(uint32_t value)	INLINE { E::set32(header.fields.reserved, value); }

	typedef typename P::E		E;
private:
	macho_header_content<P>	header;
};


//
// mach-o load command
//
template <typename P>
class macho_load_command {
public:
	uint32_t		cmd() const						INLINE { return E::get32(command.cmd); }
	void			set_cmd(uint32_t value)			INLINE { E::set32(command.cmd, value); }

	uint32_t		cmdsize() const					INLINE { return E::get32(command.cmdsize); }
	void			set_cmdsize(uint32_t value)		INLINE { E::set32(command.cmdsize, value); }

	typedef typename P::E		E;
private:
	load_command	command;
};


//
// mach-o uuid load command
//
template <typename P>
class macho_uuid_command {
public:
	uint32_t		cmd() const								INLINE { return E::get32(fields.cmd); }
	void			set_cmd(uint32_t value)					INLINE { E::set32(fields.cmd, value); }

	uint32_t		cmdsize() const							INLINE { return E::get32(fields.cmdsize); }
	void			set_cmdsize(uint32_t value)				INLINE { E::set32(fields.cmdsize, value); }

	const uint8_t*	uuid() const							INLINE { return fields.uuid; }
	void			set_uuid(uint8_t value[16])				INLINE { memcpy(&fields.uuid, value, 16); }
			
	typedef typename P::E		E;
private:
	uuid_command	fields;
};


//
// mach-o dylib load command
//
template <typename P>
class macho_dylib_command {
public:
	uint32_t		cmd() const									INLINE { return E::get32(fields.cmd); }
	void			set_cmd(uint32_t value)						INLINE { E::set32(fields.cmd, value); }

	uint32_t		cmdsize() const								INLINE { return E::get32(fields.cmdsize); }
	void			set_cmdsize(uint32_t value)					INLINE { E::set32(fields.cmdsize, value); }

	uint32_t		name_offset() const							INLINE { return E::get32(fields.dylib.name.offset); }
	void			set_name_offset(uint32_t value)				INLINE { E::set32(fields.dylib.name.offset, value);  }
	
	uint32_t		timestamp() const							INLINE { return E::get32(fields.dylib.timestamp); }
	void			set_timestamp(uint32_t value)				INLINE { E::set32(fields.dylib.timestamp, value); }

	uint32_t		current_version() const						INLINE { return E::get32(fields.dylib.current_version); }
	void			set_current_version(uint32_t value)			INLINE { E::set32(fields.dylib.current_version, value); }

	uint32_t		compatibility_version() const				INLINE { return E::get32(fields.dylib.compatibility_version); }
	void			set_compatibility_version(uint32_t value)	INLINE { E::set32(fields.dylib.compatibility_version, value); }

	const char*		name() const								INLINE { return (const char*)&fields + name_offset(); }
	void			set_name_offset()							INLINE { set_name_offset(sizeof(fields)); }
	
	typedef typename P::E		E;
private:
	dylib_command	fields;
};


// Print everything to standard output.
// Mixing stdout and stderr looks bad when the output is reprinted by Xcode.

void printUsage() 
{
    fprintf(stdout, "%s", usage);
}

void fail(const char *msg, ...) 
    __attribute__((format(printf, 1, 2))) __attribute__((noreturn))
{
    va_list args;
    va_start(args, msg);
    char *msg2;
    asprintf(&msg2, "*** error: %s\n", msg);
    vfprintf(stdout, msg2, args);
    exit(1);
}

void fail_errno(const char *msg, ...) 
    __attribute__((format(printf, 1, 2))) __attribute__((noreturn))
{
    va_list args;
    va_start(args, msg);
    char *msg2;
    asprintf(&msg2, "*** error: %s: %s\n", msg, strerror(errno));
    vfprintf(stdout, msg2, args);
    exit(1);
}

void fail_usage(const char *msg, ...) 
    __attribute__((format(printf, 1, 2))) __attribute__((noreturn))
{
    va_list args;
    va_start(args, msg);
    char *msg2;
    asprintf(&msg2, "*** error: %s\n\n", msg);
    vfprintf(stdout, msg2, args);
    printUsage();
    exit(1);
}


void log_vn(int verbosity, const char *msg, va_list args)
    __attribute__((format(printf, 2, 0)))
{
    if (verbosity <= Verbose) {
        char *msg2;
        asprintf(&msg2, "%s\n", msg);
        vfprintf(stdout, msg2, args);
        free(msg2);
    }
}

int log_v(const char *msg, ...) 
    __attribute__((format(printf, 1, 2)))
{
    va_list args;
    va_start(args, msg);
    log_vn(1, msg, args);
    return -1;
}

int log_vv(const char *msg, ...) 
    __attribute__((format(printf, 1, 2)))
{
    va_list args;
    va_start(args, msg);
    log_vn(2, msg, args);
    return -1;
}


ssize_t pread_all(int fd, void *buf, size_t count, off_t offset)
{
    size_t total = 0;
    while (total < count) {
        ssize_t readed = pread(fd, (void *)((char *)buf+total), 
                               count-total, offset+total);
        if (readed > 0) total += readed;     // got data
        else if (readed == 0) return total;  // EOF: done
        else if (readed == -1  &&  errno != EINTR) return -1;  
                                                   // error but not EINTR: fail
    }

    return total;
}


template <typename T> 
int parse_macho(int fd, uint32_t offset, uint32_t size, 
                void (^dylibVisitor)(NSString *path), 
                void (^uuidVisitor)(NSUUID *uuid))
{
    ssize_t readed;

    macho_header<T> mh;
    if (size < sizeof(mh)) return log_vv("file is too small");
    readed = pread_all(fd, &mh, sizeof(mh), offset);
    if (readed != sizeof(mh)) return log_vv("pread failed");

    uint32_t sizeofcmds = mh.sizeofcmds();
    size -= sizeof(mh);
    offset += sizeof(mh);
    if (size < sizeofcmds) return log_vv("file is badly formed");

    uint8_t *cmdp = (uint8_t *)malloc(sizeofcmds);
    if (!cmdp) return log_vv("malloc(sizeofcmds) failed");

    readed = pread_all(fd, cmdp, sizeofcmds, offset);
    if (readed == sizeofcmds) {
        uint8_t *cmds = cmdp;
        for (uint32_t c = 0; c < mh.ncmds(); c++) {
            macho_load_command<T> *cmd;
            if (size < sizeof(*cmd)) return log_vv("file is badly formed");
            cmd = (macho_load_command<T>*) cmds;
            if (size < cmd->cmdsize()) return log_vv("file is badly formed");
            cmds += cmd->cmdsize();
            size -= cmd->cmdsize();

            if (dylibVisitor  &&  
                (cmd->cmd() == LC_LOAD_DYLIB  ||  
                 cmd->cmd() == LC_LOAD_WEAK_DYLIB  ||  
                 cmd->cmd() == LC_LAZY_LOAD_DYLIB))
            {
                macho_dylib_command<T>* dylib = (macho_dylib_command<T>*)cmd;
                if (dylib->cmdsize() < dylib->name_offset()) continue;
                char *name = (char *)dylib + dylib->name_offset();
                size_t name_len =
                    strnlen(name, dylib->cmdsize() - dylib->name_offset());
                log_vv("  loads %.*s", (int)name_len, name);

                #define PREPREFIX "@rpath/"
                #define PREFIX    PREPREFIX "libswift"
                if (0 == strncmp(name, PREFIX, strlen(PREFIX))) {
                    NSString *nsname = 
                        [[[[NSString alloc] initWithBytes:name length:name_len 
                           encoding:NSUTF8StringEncoding] autorelease]
                         substringFromIndex:strlen(PREPREFIX)];
                    if (nsname) dylibVisitor(nsname);
                }
            }
            else if (uuidVisitor  &&  cmd->cmd() == LC_UUID) {
                macho_uuid_command<T>* uuid_cmd = (macho_uuid_command<T>*)cmd;
                if (uuid_cmd->cmdsize() < sizeof(uuid_command)) continue;
                CFUUIDBytes uuid_bytes = *(const CFUUIDBytes *)uuid_cmd->uuid();
                NSUUID *uuid = (NSUUID *)
                    CFUUIDCreateFromUUIDBytes(nil, uuid_bytes);
                uuidVisitor(uuid);
                [uuid release];
            }
        }
    }
    free(cmdp);

    return 0;
}


int parse_macho(int fd, uint32_t offset, uint32_t size, 
              void (^dylibVisitor)(NSString *path), 
              void (^uuidVisitor)(NSUUID *uuid))
{
    uint32_t magic;
    if (size < sizeof(magic)) return log_vv("file is too small");
    ssize_t readed = pread_all(fd, &magic, sizeof(magic), offset);
    if (readed != sizeof(magic)) return log_vv("pread failed");
    
    switch (magic) {
    case MH_MAGIC_64:
        return parse_macho<Pointer64<CurrentEndian>>(fd, offset, size, 
                                                     dylibVisitor, uuidVisitor);
    case MH_MAGIC:
        return parse_macho<Pointer32<CurrentEndian>>(fd, offset, size, 
                                                     dylibVisitor, uuidVisitor);
    case MH_CIGAM_64:
        return parse_macho<Pointer64<OtherEndian>>(fd, offset, size, 
                                                   dylibVisitor, uuidVisitor);
    case MH_CIGAM:
        return parse_macho<Pointer32<OtherEndian>>(fd, offset, size, 
                                                   dylibVisitor, uuidVisitor);
    default:
        return log_vv("file is not mach-o");
    }
}


int parse_fat(int fd, off_t fsize, char *buffer, size_t size, 
              void (^dylibVisitor)(NSString *path), 
              void (^uuidVisitor)(NSUUID *uuid))
{
    uint32_t magic;

    if (size < sizeof(magic)) {
        return log_vv("file is too small");
    }

    magic = *(uint32_t *)buffer;
    if (magic == FAT_MAGIC || magic == FAT_CIGAM) {
        struct fat_header *fh;
        uint32_t fat_magic, fat_nfat_arch;
        struct fat_arch *archs;
        uint32_t i;
        
        if (size < sizeof(struct fat_header)) {
            return log_vv("file is too small");
        }

        fh = (struct fat_header *)buffer;
        fat_magic = OSSwapBigToHostInt32(fh->magic);
        fat_nfat_arch = OSSwapBigToHostInt32(fh->nfat_arch);

        size_t fat_arch_size;
        // fat_nfat_arch * sizeof(struct fat_arch) + sizeof(struct fat_header)
        if (os_mul_and_add_overflow(fat_nfat_arch, sizeof(struct fat_arch),
                                    sizeof(struct fat_header), &fat_arch_size))
        {
            return log_vv("too many fat archs\n");
        }
        if (size < fat_arch_size) {
            return log_vv("file is too small");
        }

        archs = (struct fat_arch *)(buffer + sizeof(struct fat_header));

        /* Special case hidden CPU_TYPE_ARM64 */
        size_t fat_arch_plus_one_size;
        if (os_add_overflow(fat_arch_size, sizeof(struct fat_arch),
                            &fat_arch_plus_one_size))
        {
            return log_vv("too many fat archs\n");
        }
        if (size >= fat_arch_plus_one_size) {
            if (fat_nfat_arch > 0
                && OSSwapBigToHostInt32(archs[fat_nfat_arch].cputype) == CPU_TYPE_ARM64) {
                fat_nfat_arch++;
            }
        }
        /* End special case hidden CPU_TYPE_ARM64 */

        for (i=0; i < fat_nfat_arch; i++) {
            int ret;
            uint32_t arch_cputype, arch_cpusubtype, arch_offset, arch_size, arch_align;

            arch_cputype = OSSwapBigToHostInt32(archs[i].cputype);
            arch_cpusubtype = OSSwapBigToHostInt32(archs[i].cpusubtype);
            arch_offset = OSSwapBigToHostInt32(archs[i].offset);
            arch_size = OSSwapBigToHostInt32(archs[i].size);
            arch_align = OSSwapBigToHostInt32(archs[i].align);

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

            ret = parse_macho(fd, arch_offset, arch_size,
                              dylibVisitor, uuidVisitor);
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


void process(NSString *path, void(^dylibVisitor)(NSString *), 
             void (^uuidVisitor)(NSUUID *))
{
    log_vv("Scanning %s...", path.fileSystemRepresentation);

    int fd = open(path.fileSystemRepresentation, O_RDONLY);
    if (fd < 0) log_vv("%s: open failed: %s", 
                       path.fileSystemRepresentation, strerror(errno));

    struct stat st;
    if (fstat(fd, &st) < 0) {
        log_vv("%s: stat failed: %s", 
               path.fileSystemRepresentation, strerror(errno));
    } else {
        const int len = 4096;
        char buf[len];
        ssize_t readed = pread_all(fd, buf, len, 0);
        if (readed != len) {
            log_vv("%s: pread failed: %s", 
                   path.fileSystemRepresentation, strerror(errno));
        } else {
            parse_fat(fd, st.st_size, buf, len, dylibVisitor, uuidVisitor);
        }
    }
    close(fd);
}


@implementation NSString (sst)
-(NSString *)sst_stringByAppendingPathComponents:(NSArray *)components
{
    NSString *result = self;
    @autoreleasepool {
        for (NSString *component in components) {
            result = [result stringByAppendingPathComponent:component];
        }
        [result retain];
    }
    return [result autorelease];
}
@end

@implementation NSTask (sst)
-(NSString *)sst_command {
    NSMutableString *command = [self.launchPath mutableCopy];
    for (NSString *arg in self.arguments) {
        [command appendFormat:@" '%@'", arg];
    }
    return command;
}
@end


bool operator <= (const struct timespec &lhs, const struct timespec &rhs) 
{
    if (lhs.tv_sec == rhs.tv_sec) return lhs.tv_nsec <= rhs.tv_nsec;
    return lhs.tv_sec <= rhs.tv_sec;
}


// This executable's own path.
NSString *self_executable = []() -> NSString * {
    uint32_t len = 0;
    _NSGetExecutablePath(nil, &len);
    std::vector<char> buffer;
    buffer.reserve(len);
    _NSGetExecutablePath(buffer.data(), &len);
    return [[NSString alloc] initWithUTF8String:buffer.data()];
}();


// This executable's own xctoolchain path.
NSString *self_toolchain = []() -> NSString * {
    @autoreleasepool {
        NSString *result = self_executable;
        
        // Remove the executable name.
        result = result.stringByDeletingLastPathComponent;
        
        // Remove trailing /usr/bin, if any
        if ([result.lastPathComponent isEqualToString:@"bin"]) {
            result = result.stringByDeletingLastPathComponent;
        }
        if ([result.lastPathComponent isEqualToString:@"usr"]) {
            result = result.stringByDeletingLastPathComponent;
        }
        return [result retain];
    }
}();


// Runs a tool with `xcrun`. 
// Returns NSTask.terminationStatus.
// Prints the tool's command line if we are verbose.
// Prints the tool's stdout and stderr if terminationStatus is non-zero
//   or if we are very verbose.
typedef void (^XcrunToolBlock)(NSData *stdOutData, NSData *stdErrorData, int err);

int xcrunToolCommand(NSArray *commandAndArguments, XcrunToolBlock block = 0)
{
    @autoreleasepool {
        NSTask *task = [[[NSTask alloc] init] autorelease];
        task.launchPath = @"/usr/bin/xcrun";

        // Tell xcrun to search our toolchain first.
        NSMutableArray *arguments = 
            [[commandAndArguments mutableCopy] autorelease];
        [arguments insertObject:@"--toolchain" atIndex:0];
        [arguments insertObject:self_toolchain atIndex:1];

        // Tell xcrun to print its command if we are very verbose.
        if (Verbose > 1) {
            [arguments insertObject:@"--log" atIndex:2];
        }

        task.arguments = arguments;
        
        NSPipe *outPipe = [NSPipe pipe];
        NSPipe *errPipe = [NSPipe pipe];
        task.standardOutput = outPipe;
        task.standardError = errPipe;
        
        log_v("  %s", task.sst_command.fileSystemRepresentation);
        [task launch];

        // Read stdout and stderr in parallel, then wait for the task 
        // to exit. Anything else risks deadlock if the task fills 
        // one of the output buffers.

        __block NSData *stdErrData = nil;
        dispatch_semaphore_t gotStdErr = dispatch_semaphore_create(0);
        dispatch_queue_t concurrentQueue = 
            dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0);
        dispatch_async(concurrentQueue, ^{
            stdErrData = 
                [[errPipe.fileHandleForReading readDataToEndOfFile] retain];
            dispatch_semaphore_signal(gotStdErr);
        });

        NSData *stdOutData = [outPipe.fileHandleForReading readDataToEndOfFile];

        dispatch_semaphore_wait(gotStdErr, DISPATCH_TIME_FOREVER);
        dispatch_release(gotStdErr);
        [stdErrData autorelease];

        [task waitUntilExit];
        
        // Task is finished and we have its stdout and stderr output.

        // Print its stdout and stderr if it failed or we are verbose.
        // (Print nothing by default because codesign is noisy.)

        int err = task.terminationStatus;
        if (err  ||  Verbose > 1) {
            fwrite(stdErrData.bytes, stdErrData.length, 1, stdout);
            fwrite(stdOutData.bytes, stdOutData.length, 1, stdout);
        }
        if (block) { block(stdOutData, stdErrData, err); }
        return err;
    }
}


void copyAndStripBitcode(NSString *src, NSString *dst)
{
    // -r removes bitcode
    int err = xcrunToolCommand(@[@"bitcode_strip", src, @"-r", @"-o", dst]);

    // Fail if bitcode_strip failed.
    if (err) {
        fail("Couldn't copy and strip bitcode %s to %s: bitcode_strip failed "
             "with exit code %d", 
             src.fileSystemRepresentation, dst.fileSystemRepresentation, err);
    }
}


void 
copyFile(NSFileManager *fm, NSString *src, NSString *dst, bool stripBitcode)
{
    if (stripBitcode) {
        copyAndStripBitcode(src, dst);
    } else {
        NSError *nserr;
        if (![fm copyItemAtPath:src toPath:dst error:&nserr]) {
            fail("Couldn't copy %s to %s: %s", 
                 src.fileSystemRepresentation, 
                 dst.fileSystemRepresentation, 
                 nserr.localizedFailureReason.UTF8String);
        }
    }
}


void copyLibraries(NSString *src_dir, NSString *dst_dir, 
                   NSMutableDictionary *libs, bool stripBitcode)
{
    NSFileManager *fm = NSFileManager.defaultManager;

    [fm createDirectoryAtPath: dst_dir withIntermediateDirectories:YES 
     attributes:nil error:nil];
    
    for (NSString *lib in libs) @autoreleasepool {
        NSString *src = [src_dir stringByAppendingPathComponent:lib]; 
        NSString *dst = [dst_dir stringByAppendingPathComponent:lib];
        
        // Compare UUIDs of src and dst and don't copy if they're the same.
        // Do not use mod times for this task: the dst copy gets code-signed 
        // and bitcode-stripped so it can look newer than it really is.
        NSSet *srcUUIDs = libs[lib];
        NSMutableSet *dstUUIDs = [NSMutableSet set];
        process(dst, nil, ^(NSUUID *uuid) {
            [dstUUIDs addObject:uuid];
        });

        log_vv("Source UUIDs %s: %s", src.fileSystemRepresentation, 
               srcUUIDs.description.UTF8String);
        log_vv("Destination UUIDs %s: %s", dst.fileSystemRepresentation, 
               dstUUIDs.description.UTF8String);

        if ([srcUUIDs isEqualToSet:dstUUIDs]) {
            log_v("%s is up to date at %s", 
                  lib.fileSystemRepresentation, dst.fileSystemRepresentation);
            continue;
        }
        
        // Perform the copy.
        
        log_v("Copying %s from %s to %s", 
              lib.fileSystemRepresentation, 
              src_dir.fileSystemRepresentation, 
              dst_dir.fileSystemRepresentation);
        
        [fm removeItemAtPath:dst error:nil];  // fixme report this err?
        copyFile(fm, src.stringByResolvingSymlinksInPath, dst, stripBitcode);
    }
}

NSString *relative_path(NSString *base, NSString *suffix_name)
{
    NSDictionary *env = NSProcessInfo.processInfo.environment;
    NSString *suffix = env[suffix_name];
    if (base && suffix) {
        return [base stringByAppendingPathComponent:suffix];
    }
    return nil;
}

void add_relative_path(NSMutableArray *paths, 
                       NSString *base, NSString *suffix_name)
{
    NSString *path = relative_path(base, suffix_name);
    if (path) [paths addObject:path];
}

NSData *query_code_signature(NSString *file) {
    __block NSData *d = 0;
    @autoreleasepool {
          NSArray *command = @[ @"codesign", @"-r-", @"--display", file];
          log_v("Probing signature of %s", file.fileSystemRepresentation);
          int err = xcrunToolCommand(command,
             ^(NSData *stdOutData, NSData *stdErrData, int err) {
                if (!err) { d = [stdOutData retain]; }
          });
          if (err) { return 0; }
    }
    return [d autorelease];
}

int main(int argc, const char *argv[])
{
    @autoreleasepool {
        NSFileManager *fm = [NSFileManager defaultManager];

        // Exxecutables to scan for Swift references.
        // --scan-executable
        NSMutableArray *executables = [NSMutableArray array];

        // Directories to scan for more executables.
        // --scan-folder
        NSMutableArray *embedDirs = [NSMutableArray array];

        // Platform name.
        // --platform
        // or the last path component of --source-libraries
        NSString *platform = nil;

        // Copy source.
        // --source-libraries
        // or /path/to/swift-stdlib-tool/../../lib/swift/<--platform>
        NSString *src_dir = nil;

        // Copy destinations, signed and unsigned.
        // --destination and --unsigned-destination
        NSString *dst_dir = nil;
        NSString *unsigned_dst_dir = nil;

        // Resource copy destination.
        // --resource-destination
        NSString *resource_dst_dir = nil;

        // Resource libraries.
        // --resource-library
        NSMutableArray *resourceLibraries = [NSMutableArray new];

        // Code signing options.
        NSString *ident = nil;
        NSString *keychain = nil;
        NSMutableArray *otherCodesignFlags = [NSMutableArray array];

        // Read arguments
        bool print = false;
        bool copy = false;
        bool stripBitcode = false;
        for (int i = 1; i < argc; i++) {
            if (0 == strcmp(argv[i], "--print")) print = true;
            if (0 == strcmp(argv[i], "--copy")) copy = true;
            if (0 == strcmp(argv[i], "--verbose")) Verbose++;
            if (0 == strcmp(argv[i], "--help")) {
                printUsage();
                exit(0);
            }

            if (0 == strcmp(argv[i], "--scan-executable")) {
                [executables addObject:[NSString stringWithUTF8String:argv[++i]]];
            }
            if (0 == strcmp(argv[i], "--scan-folder")) {
                [embedDirs addObject:[NSString stringWithUTF8String:argv[++i]]];
            }
            if (0 == strcmp(argv[i], "--source-libraries")) {
                src_dir = [NSString stringWithUTF8String:argv[++i]];
            }
            if (0 == strcmp(argv[i], "--platform")) {
                platform = [NSString stringWithUTF8String:argv[++i]];
            }

            if (0 == strcmp(argv[i], "--destination")) {
                dst_dir = [NSString stringWithUTF8String:argv[++i]];
            }
            if (0 == strcmp(argv[i], "--unsigned-destination")) {
                unsigned_dst_dir = [NSString stringWithUTF8String:argv[++i]];
            }

            if (0 == strcmp(argv[i], "--sign")) {
                ident = [NSString stringWithUTF8String:argv[++i]];
            }
            if (0 == strcmp(argv[i], "--keychain")) {
                keychain = [NSString stringWithUTF8String:argv[++i]];
            }
            if (0 == strcmp(argv[i], "--Xcodesign")) {
                [otherCodesignFlags addObject:
                 [NSString stringWithUTF8String:argv[++i]]];
            }

            if (0 == strcmp(argv[i], "--strip-bitcode")) {
                stripBitcode = true;
            }

            if (0 == strcmp(argv[i], "--resource-destination")) {
                resource_dst_dir = [NSString stringWithUTF8String:argv[++i]];
            }
            if (0 == strcmp(argv[i], "--resource-library")) {
                [resourceLibraries addObject:[NSString stringWithUTF8String:argv[++i]]];
            }
        }

        // Fix up src_dir and platform values.
        if (!src_dir && !platform) {
            // Neither src_dir nor platform is set. Die.
            fail_usage("At least one of --source-libraries and --platform "
                       "must be set.");
        }
        else if (!src_dir) {
            // platform is set but src_dir is not. 
            // Use platform to set src_dir relative to us.
            src_dir = [[[self_executable stringByDeletingLastPathComponent]
                        stringByDeletingLastPathComponent]
                       sst_stringByAppendingPathComponents:
                       @[ @"lib", @"swift-5.0", platform ]];
        } else if (!platform) {
            // src_dir is set but platform is not. 
            // Pick platform from src_dir's name.
            platform = src_dir.lastPathComponent;
        }

        // Add the platform to unsigned_dst_dir if it is not already present.
        if (unsigned_dst_dir) {
            NSString *unsigned_platform = unsigned_dst_dir.lastPathComponent;
            if (![platform isEqualToString:unsigned_platform]) {
                unsigned_dst_dir = 
                    [unsigned_dst_dir stringByAppendingPathComponent:platform];
            }
        }
        
        // If the user specifies --strip-bitcode but not --sign, this
        // will cause the dylibs to get copied, stripped, but not resigned.
        // This will cause apps to fail to launch because the code signature
        // is invalid.  In this case, ignore --strip-bitcode.
        if (stripBitcode && !ident) {
            stripBitcode = false;
        }

        // Collect executables from the --scan-folder locations.
        for (NSString *embedDir in embedDirs) @autoreleasepool {
            NSDirectoryEnumerator *dir = [fm enumeratorAtPath:embedDir];
            for (NSString *p in dir) @autoreleasepool {
                BOOL isDir;
                NSString *p2 = [embedDir stringByAppendingPathComponent:p];
                if ([fm fileExistsAtPath:p2 isDirectory:&isDir]  &&  
                    !isDir  &&  
                    [fm isExecutableFileAtPath:p2])
                {
                    [executables addObject:p2];
                } else {
                    log_vv("%s is not an executable file", 
                           p2.fileSystemRepresentation);
                }
            }
        }
             
        // Collect Swift library names from the input files.
        // If the library does not exist in src_dir then assume the user wrote 
        // their own library named libswift* and is handling it elsewhere.
        NSMutableDictionary *swiftLibs = [NSMutableDictionary new];
        for (NSString *path in executables) @autoreleasepool {
            process(path, 
               ^(NSString *linkedLib) { 
                    @autoreleasepool {
                        NSString *linkedSrc =
                            [src_dir stringByAppendingPathComponent:linkedLib];
                        if ([fm fileExistsAtPath:linkedSrc]) {
                            swiftLibs[linkedLib] = [NSMutableSet set]; 
                        }
                    }
                }, 
                nil);
        }

        // Collect more Swift library names from the Swift libraries themselves.
        // Also collect the Swift libraries' UUIDs.
        NSMutableArray *worklist = [swiftLibs.allKeys mutableCopy];
        while (worklist.count) @autoreleasepool {
            NSString *lib = [worklist lastObject];
            [worklist removeLastObject];
            NSString *path = [src_dir stringByAppendingPathComponent:lib];
            process(path, 
                ^(NSString *linkedLib) {
                    @autoreleasepool {
                        NSString *linkedSrc = 
                            [src_dir stringByAppendingPathComponent:linkedLib];
                        if (!swiftLibs[linkedLib]  &&  
                            [fm fileExistsAtPath:linkedSrc]) 
                        {
                            swiftLibs[linkedLib] = [NSMutableSet set];
                            [worklist addObject:linkedLib];
                        }
                    }
                }, 
                ^(NSUUID *uuid) {
                    NSMutableSet *uuids = swiftLibs[lib];
                    [uuids addObject:uuid];
                });
        }
        [worklist release];

        // Collect all the Swift libraries that the user requested
        // with --resource-library.
        NSMutableDictionary *swiftLibsForResources = [NSMutableDictionary new];
        for (NSString *lib in resourceLibraries) @autoreleasepool {
            NSString *libSrc = [src_dir stringByAppendingPathComponent:lib];
            if ([fm fileExistsAtPath:libSrc]) {
                swiftLibsForResources[lib] = [NSMutableSet set];
            }
        }

        // Collect dependencies of --resource-library libs.
        worklist = [swiftLibsForResources.allKeys mutableCopy];
        while (worklist.count) @autoreleasepool {
            NSString *lib = [worklist lastObject];
            [worklist removeLastObject];
            NSString *path = [src_dir stringByAppendingPathComponent:lib];
            process(path,
                ^(NSString *linkedLib) {
                    NSString *linkedSrc =
                        [src_dir stringByAppendingPathComponent:linkedLib];
                    if (!swiftLibsForResources[linkedLib] &&
                        [fm fileExistsAtPath:linkedSrc])
                    {
                        swiftLibsForResources[linkedLib] = [NSMutableSet set];
                        [worklist addObject:linkedLib];
                    }
                },
                ^(NSUUID *uuid) {
                    NSMutableSet *uuids = swiftLibsForResources[lib];
                    [uuids addObject:uuid];
                });
        }
        [worklist release];

        // Print the Swift libraries (full path to toolchain's copy)
        if (print) {
            for (NSString *lib in swiftLibs) {
                printf("%s\n", [[src_dir stringByAppendingPathComponent:lib] 
                                fileSystemRepresentation]);
            }
        }

        // Copy the Swift libraries to $build_dir/$frameworks
        // and $build_dir/$unsigned_frameworks
        if (copy) {
            copyLibraries(src_dir, dst_dir, swiftLibs, stripBitcode);
            if (unsigned_dst_dir) {
                // Never strip bitcode from the unsigned libraries. 
                // Their existing signatures must be preserved.
                copyLibraries(src_dir, unsigned_dst_dir, swiftLibs, false);
            }

            if (resource_dst_dir) {
                // Never strip bitcode from resources libraries, for
                // the same reason as the libraries copied to
                // unsigned_dst_dir.
                copyLibraries(src_dir, resource_dst_dir, swiftLibsForResources, false);
            }
        }

        // Codesign the Swift libraries in $build_dir/$frameworks
        // but not the libraries in $build_dir/$unsigned_frameworks.
        if (ident) {
            // Swift libraries that are up-to-date get codesigned anyway
            // (in case options changed or a previous build was incomplete).
            // We do employ an optimization, however, if resigning the dylib
            // results in getting the same signing identity and credentials
            // then we keep the original file to optimize for delta updates
            // to the device.

            __block bool signedOne = false;
            NSLock *signingLock = [NSLock new];

            [swiftLibs enumerateKeysAndObjectsWithOptions: NSEnumerationConcurrent usingBlock: ^(id key, id value, BOOL *stop) {

                // Work around authentication UI problems
                // by signing one synchronously and then signing the rest.
                [signingLock lock];
                if (signedOne) {
                    // First signer is complete. Proceed concurrently.
                    [signingLock unlock];
                } else {
                    // We are the first signer. Hold the lock until we finish.
                }

                NSString *lib = key;
                NSString *dst = [dst_dir stringByAppendingPathComponent:lib];

                // Get the code signature, and copy the dylib to the side
                // to preserve it in case it does not change.  We can use
                // this to avoid unnecessary copies during delta installs
                // to devices.
                NSData *oldSignatureData = query_code_signature(dst);
                const char *tmpFilePath = 0;
                if (oldSignatureData) {
                    // Make a copy of the existing file, with permissions and
                    // mtime preserved.
                    NSString *tmpFile = [dst stringByAppendingPathExtension:@"original"];
                    tmpFilePath = tmpFile.fileSystemRepresentation;
                    xcrunToolCommand(@[@"cp", @"-p", dst, tmpFile]);
                }

                // Proceed with (re-)codesigning.
                log_v("Codesigning %s at %s", lib.fileSystemRepresentation, dst_dir.fileSystemRepresentation);

                // Build the codesign invocation.
                NSMutableArray *commandAndArguments = 
                    [NSMutableArray arrayWithObjects:
                     @"codesign", 
                     @"--force", @"--sign", ident, @"--verbose", nil];

                if (keychain) {
                    [commandAndArguments addObject:@"--keychain"];
                    [commandAndArguments addObject:keychain];
                }

                // Other codesign flags come later
                // so they can override the default flags.
                [commandAndArguments addObjectsFromArray:otherCodesignFlags];

                [commandAndArguments addObject:dst];

                int err = xcrunToolCommand(commandAndArguments);

                // Fail if codesign failed.
                if (err) {
                    // Clean up any temporary files.
                    if (tmpFilePath)
                        unlink(tmpFilePath);
                    fail("Couldn't codesign %s: codesign failed with "
                         "exit code %d", dst.fileSystemRepresentation, err);
                }

                // If we have an existing code signature data, query the new one and compare
                // it with the code signature of the file before we re-signed it.
                // If they are the same, use the original file instead.  This preserves
                // the contents of the file and mtime for use with delta installs.
                if (oldSignatureData) {
                    NSData *newSignatureData = query_code_signature(dst);

#if 0
                    // For Debugging.
                    fprintf(stdout, "oldSignature (%lu bytes)\n", (unsigned long)oldSignatureData.length);
                    fwrite(oldSignatureData.bytes, oldSignatureData.length, 1, stdout);
                    fprintf(stdout, "\nnewSignature (%lu bytes)\n", (unsigned long)newSignatureData.length);
                    fwrite(newSignatureData.bytes, newSignatureData.length, 1, stdout);
                    fprintf(stdout, "\n");
                    fflush(stdout);
#endif

                    unsigned newLength = newSignatureData.length;
                    if (newLength == oldSignatureData.length && 
                        memcmp(newSignatureData.bytes, oldSignatureData.bytes, newLength) == 0) {
                        log_v("Code signature of %s is unchanged; keeping original", lib.fileSystemRepresentation);
                        // The two signatures match.  Unlink the new file, and re-link the old file.
                        const char *filePath = dst.fileSystemRepresentation;
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
                    [signingLock unlock];
                }
            }];
            [signingLock release];
        }
    }
    exit(0);
}
