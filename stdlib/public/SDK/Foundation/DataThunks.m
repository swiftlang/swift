//===----------------------------------------------------------------------===//
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

#import <Foundation/Foundation.h>
#import <sys/fcntl.h>

#include "swift/Runtime/Config.h"

#if TARGET_OS_IPHONE && !TARGET_OS_SIMULATOR
static int __NSFileProtectionClassForOptions(NSUInteger options) {
    int result;
    switch (options & NSDataWritingFileProtectionMask) {
        case NSDataWritingFileProtectionComplete:  // Class A
            result = 1;
            break;
        case NSDataWritingFileProtectionCompleteUnlessOpen:  // Class B
            result = 2;
            break;
        case NSDataWritingFileProtectionCompleteUntilFirstUserAuthentication:  // Class C
            result = 3;
            break;
        case NSDataWritingFileProtectionNone:  // Class D
            result = 4;
            break;
        default:
            result = 0;
            break;
    }
    return result;
}
#endif

static int32_t _NSOpenFileDescriptor(const char *path, NSInteger flags, int protectionClass, NSInteger mode) {
    int fd = -1;
    if (protectionClass != 0) {
        fd = open_dprotected_np(path, flags, protectionClass, 0, mode);
    } else {
        fd = open(path, flags, mode);
    }
    return fd;
}

static NSInteger _NSWriteToFileDescriptor(int32_t fd, const void *buffer, NSUInteger length) {
    size_t preferredChunkSize = (size_t)length;
    size_t numBytesRemaining = (size_t)length;
    while (numBytesRemaining > 0UL) {
        size_t numBytesRequested = (preferredChunkSize < (1LL << 31)) ? preferredChunkSize : ((1LL << 31) - 1);
        if (numBytesRequested > numBytesRemaining) numBytesRequested = numBytesRemaining;
        ssize_t numBytesWritten;
        do {
            numBytesWritten = write(fd, buffer, numBytesRequested);
        } while (numBytesWritten < 0L && errno == EINTR);
        if (numBytesWritten < 0L) {
            return -1;
        } else if (numBytesWritten == 0L) {
            break;
        } else {
            numBytesRemaining -= numBytesWritten;
            if ((size_t)numBytesWritten < numBytesRequested) break;
            buffer = (char *)buffer + numBytesWritten;
        }
    }
    return length - numBytesRemaining;
}

static NSError *_NSErrorWithFilePath(NSInteger code, id pathOrURL) {
    NSString *key = [pathOrURL isKindOfClass:[NSURL self]] ? NSURLErrorKey : NSFilePathErrorKey;
    return [NSError errorWithDomain:NSCocoaErrorDomain code:code userInfo:[NSDictionary dictionaryWithObjectsAndKeys:pathOrURL, key, nil]];
}

static NSError *_NSErrorWithFilePathAndErrno(NSInteger posixErrno, id pathOrURL, BOOL reading) {
    NSInteger code;
    if (reading) {
        switch (posixErrno) {
            case EFBIG:          code = NSFileReadTooLargeError; break;
            case ENOENT:         code = NSFileReadNoSuchFileError; break;
            case EPERM:          // fallthrough
            case EACCES:         code = NSFileReadNoPermissionError; break;
            case ENAMETOOLONG:   code = NSFileReadInvalidFileNameError; break;
            default:             code = NSFileReadUnknownError; break;
        }
    } else {
        switch (posixErrno) {
            case ENOENT:         code = NSFileNoSuchFileError; break;
            case EPERM:          // fallthrough
            case EACCES:         code = NSFileWriteNoPermissionError; break;
            case ENAMETOOLONG:   code = NSFileWriteInvalidFileNameError; break;
#if DEPLOYMENT_TARGET_MACOSX || DEPLOYMENT_TARGET_EMBEDDED
            case EDQUOT:
#endif
            case ENOSPC:         code = NSFileWriteOutOfSpaceError; break;
            case EROFS:          code = NSFileWriteVolumeReadOnlyError; break;
            case EEXIST:         code = NSFileWriteFileExistsError; break;
            default:             code = NSFileWriteUnknownError; break;
        }
    }
    
    NSString *key = [pathOrURL isKindOfClass:[NSURL self]] ? NSURLErrorKey : NSFilePathErrorKey;
    NSDictionary *userInfo = [[NSDictionary alloc] initWithObjectsAndKeys:pathOrURL, key, [NSError errorWithDomain:NSPOSIXErrorDomain code:posixErrno userInfo:nil], NSUnderlyingErrorKey, nil];
    NSError *error = [NSError errorWithDomain:NSCocoaErrorDomain code:code userInfo:userInfo];
    [userInfo release];
    return error;
}

SWIFT_CC(swift)
BOOL __NSDataWriteToURL(NSData *NS_RELEASES_ARGUMENT data, NSURL *NS_RELEASES_ARGUMENT url, NSDataWritingOptions writingOptions, NSError **_Nullable errorPtr) {
    assert((writingOptions & NSDataWritingAtomic) == 0);

    NSString *path = url.path;
    char cpath[1026];

    if (![path getFileSystemRepresentation:cpath maxLength:1024]) {
        if (errorPtr) *errorPtr = _NSErrorWithFilePath(NSFileWriteInvalidFileNameError, path);
        return NO;
    }

    int protectionClass = 0;
#if TARGET_OS_IPHONE && !TARGET_OS_SIMULATOR
    protectionClass = __NSFileProtectionClassForOptions(writingOptions);
#endif

    int flags = O_WRONLY|O_CREAT|O_TRUNC;
    if (writingOptions & NSDataWritingWithoutOverwriting) {
        flags |= O_EXCL;
    }
    int32_t fd = _NSOpenFileDescriptor(cpath, flags, protectionClass, 0666);
    if (fd < 0) {
        if (errorPtr) *errorPtr = _NSErrorWithFilePathAndErrno(errno, path, NO);
        [url release];
        [data release];
        return NO;
    }

    __block BOOL writingFailed = NO;
    __block int32_t saveerr = 0;
    NSUInteger dataLength = [data length];
    [data enumerateByteRangesUsingBlock:^(const void *bytes, NSRange byteRange, BOOL *stop) {
        NSUInteger length = byteRange.length;
        BOOL success = NO;
        if (length > 0) {
            NSInteger writtenLength = _NSWriteToFileDescriptor(fd, bytes, length);
            success = writtenLength > 0 && (NSUInteger)writtenLength == length;
        } else {
            success = YES; // Writing nothing always succeeds.
        }
        if (!success) {
            saveerr = errno;
            writingFailed = YES;
            *stop = YES;
        }
    }];
    if (dataLength && !writingFailed) {
        if (fsync(fd) < 0) {
            writingFailed = YES;
            saveerr = errno;
        }
    }
    if (writingFailed) {
        close(fd);
        errno = (saveerr);
        if (errorPtr) {
            *errorPtr = _NSErrorWithFilePathAndErrno(errno, path, NO);
        }
        [url release];
        [data release];
        return NO;
    }
    close(fd);
    [url release];
    [data release];
    return YES;
}
