// FIXME: Only defining POSIXErrorCode for Darwin at the moment.

#if os(OSX) || os(iOS) || os(tvOS) || os(watchOS)
/// Enumeration describing POSIX error codes.
@objc public enum POSIXErrorCode : Int32 {
  // FIXME: These are the values for Darwin. We need to get the Linux
  // values as well.
  /// Operation not permitted.
  case EPERM           = 1
  /// No such file or directory.
  case ENOENT          = 2
  /// No such process.
  case ESRCH           = 3
  /// Interrupted system call.
  case EINTR           = 4
  /// Input/output error.
  case EIO             = 5
  /// Device not configured.
  case ENXIO           = 6
  /// Argument list too long.
  case E2BIG           = 7
  /// Exec format error.
  case ENOEXEC         = 8
  /// Bad file descriptor.
  case EBADF           = 9
  /// No child processes.
  case ECHILD          = 10
  /// Resource deadlock avoided.
  case EDEADLK         = 11
                         /// 11 was EAGAIN.
  /// Cannot allocate memory.
  case ENOMEM          = 12
  /// Permission denied.
  case EACCES          = 13
  /// Bad address.
  case EFAULT          = 14
  /// Block device required.
  case ENOTBLK         = 15
  /// Device / Resource busy.
  case EBUSY           = 16
  /// File exists.
  case EEXIST          = 17
  /// Cross-device link.
  case EXDEV           = 18
  /// Operation not supported by device.
  case ENODEV          = 19
  /// Not a directory.
  case ENOTDIR         = 20
  /// Is a directory.
  case EISDIR          = 21
  /// Invalid argument.
  case EINVAL          = 22
  /// Too many open files in system.
  case ENFILE          = 23
  /// Too many open files.
  case EMFILE          = 24
  /// Inappropriate ioctl for device.
  case ENOTTY          = 25
  /// Text file busy.
  case ETXTBSY         = 26
  /// File too large.
  case EFBIG           = 27
  /// No space left on device.
  case ENOSPC          = 28
  /// Illegal seek.
  case ESPIPE          = 29
  /// Read-only file system.
  case EROFS           = 30
  /// Too many links.
  case EMLINK          = 31
  /// Broken pipe.
  case EPIPE           = 32

/// math software.
  /// Numerical argument out of domain.
  case EDOM            = 33
  /// Result too large.
  case ERANGE          = 34

/// non-blocking and interrupt i/o.
  /// Resource temporarily unavailable.
  case EAGAIN          = 35
  /// Operation would block.
  public static var EWOULDBLOCK: POSIXErrorCode { return EAGAIN }
  /// Operation now in progress.
  case EINPROGRESS     = 36
  /// Operation already in progress.
  case EALREADY        = 37

/// ipc/network software -- argument errors.
  /// Socket operation on non-socket.
  case ENOTSOCK        = 38
  /// Destination address required.
  case EDESTADDRREQ    = 39
  /// Message too long.
  case EMSGSIZE        = 40
  /// Protocol wrong type for socket.
  case EPROTOTYPE      = 41
  /// Protocol not available.
  case ENOPROTOOPT     = 42
  /// Protocol not supported.
  case EPROTONOSUPPORT = 43
  /// Socket type not supported.
  case ESOCKTNOSUPPORT = 44
  /// Operation not supported.
  case ENOTSUP         = 45
  /// Protocol family not supported.
  case EPFNOSUPPORT    = 46
  /// Address family not supported by protocol family.
  case EAFNOSUPPORT    = 47
  /// Address already in use.
  case EADDRINUSE      = 48
  /// Can't assign requested address.
  case EADDRNOTAVAIL   = 49

/// ipc/network software -- operational errors
  /// Network is down.
  case ENETDOWN        = 50
  /// Network is unreachable.
  case ENETUNREACH     = 51
  /// Network dropped connection on reset.
  case ENETRESET       = 52
  /// Software caused connection abort.
  case ECONNABORTED    = 53
  /// Connection reset by peer.
  case ECONNRESET      = 54
  /// No buffer space available.
  case ENOBUFS         = 55
  /// Socket is already connected.
  case EISCONN         = 56
  /// Socket is not connected.
  case ENOTCONN        = 57
  /// Can't send after socket shutdown.
  case ESHUTDOWN       = 58
  /// Too many references: can't splice.
  case ETOOMANYREFS    = 59
  /// Operation timed out.
  case ETIMEDOUT       = 60
  /// Connection refused.
  case ECONNREFUSED    = 61

  /// Too many levels of symbolic links.
  case ELOOP           = 62
  /// File name too long.
  case ENAMETOOLONG    = 63

  /// Host is down.
  case EHOSTDOWN       = 64
  /// No route to host.
  case EHOSTUNREACH    = 65
  /// Directory not empty.
  case ENOTEMPTY       = 66

/// quotas & mush.
  /// Too many processes.
  case EPROCLIM        = 67
  /// Too many users.
  case EUSERS          = 68
  /// Disc quota exceeded.
  case EDQUOT          = 69

/// Network File System.
  /// Stale NFS file handle.
  case ESTALE          = 70
  /// Too many levels of remote in path.
  case EREMOTE         = 71
  /// RPC struct is bad.
  case EBADRPC         = 72
  /// RPC version wrong.
  case ERPCMISMATCH    = 73
  /// RPC prog. not avail.
  case EPROGUNAVAIL    = 74
  /// Program version wrong.
  case EPROGMISMATCH   = 75
  /// Bad procedure for program.
  case EPROCUNAVAIL    = 76

  /// No locks available.
  case ENOLCK          = 77
  /// Function not implemented.
  case ENOSYS          = 78

  /// Inappropriate file type or format.
  case EFTYPE          = 79
  /// Authentication error.
  case EAUTH           = 80
  /// Need authenticator.
  case ENEEDAUTH       = 81

/// Intelligent device errors.
  /// Device power is off.
  case EPWROFF         = 82
  /// Device error, e.g. paper out.
  case EDEVERR         = 83

  /// Value too large to be stored in data type.
  case EOVERFLOW       = 84

/// Program loading errors.
  /// Bad executable.
  case EBADEXEC        = 85
  /// Bad CPU type in executable.
  case EBADARCH        = 86
  /// Shared library version mismatch.
  case ESHLIBVERS      = 87
  /// Malformed Macho file.
  case EBADMACHO       = 88

  /// Operation canceled.
  case ECANCELED       = 89

  /// Identifier removed.
  case EIDRM           = 90
  /// No message of desired type.
  case ENOMSG          = 91
  /// Illegal byte sequence.
  case EILSEQ          = 92
  /// Attribute not found.
  case ENOATTR         = 93

  /// Bad message.
  case EBADMSG         = 94
  /// Reserved.
  case EMULTIHOP       = 95
  /// No message available on STREAM.
  case ENODATA         = 96
  /// Reserved.
  case ENOLINK         = 97
  /// No STREAM resources.
  case ENOSR           = 98
  /// Not a STREAM.
  case ENOSTR          = 99
  /// Protocol error.
  case EPROTO          = 100
  /// STREAM ioctl timeout.
  case ETIME           = 101

  /// No such policy registered.
  case ENOPOLICY       = 103

  /// State not recoverable.
  case ENOTRECOVERABLE = 104
  /// Previous owner died.
  case EOWNERDEAD      = 105

  /// Interface output queue is full.
  case EQFULL          = 106
  /// Must be equal largest errno.
  public static var ELAST: POSIXErrorCode { return EQFULL }

  // FIXME: EOPNOTSUPP has different values depending on __DARWIN_UNIX03 and
  // KERNEL.
}
#endif // os(OSX) || os(iOS) || os(tvOS) || os(watchOS)
