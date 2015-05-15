// FIXME: Only defining _POSIXError for Darwin at the moment.

#if os(OSX) || os(iOS)
/// Enumeration describing POSIX error codes.
@objc public enum _POSIXError : CInt {
  // FIXME: These are the values for Darwin. We need to get the Linux
  // values as well.
  case EPERM           = 1               /* Operation not permitted */
  case ENOENT          = 2               /* No such file or directory */
  case ESRCH           = 3               /* No such process */
  case EINTR           = 4               /* Interrupted system call */
  case EIO             = 5               /* Input/output error */
  case ENXIO           = 6               /* Device not configured */
  case E2BIG           = 7               /* Argument list too long */
  case ENOEXEC         = 8               /* Exec format error */
  case EBADF           = 9               /* Bad file descriptor */
  case ECHILD          = 10              /* No child processes */
  case EDEADLK         = 11              /* Resource deadlock avoided */
                                          /* 11 was EAGAIN */
  case ENOMEM          = 12              /* Cannot allocate memory */
  case EACCES          = 13              /* Permission denied */
  case EFAULT          = 14              /* Bad address */
  case ENOTBLK         = 15              /* Block device required */
  case EBUSY           = 16              /* Device / Resource busy */
  case EEXIST          = 17              /* File exists */
  case EXDEV           = 18              /* Cross-device link */
  case ENODEV          = 19              /* Operation not supported by device */
  case ENOTDIR         = 20              /* Not a directory */
  case EISDIR          = 21              /* Is a directory */
  case EINVAL          = 22              /* Invalid argument */
  case ENFILE          = 23              /* Too many open files in system */
  case EMFILE          = 24              /* Too many open files */
  case ENOTTY          = 25              /* Inappropriate ioctl for device */
  case ETXTBSY         = 26              /* Text file busy */
  case EFBIG           = 27              /* File too large */
  case ENOSPC          = 28              /* No space left on device */
  case ESPIPE          = 29              /* Illegal seek */
  case EROFS           = 30              /* Read-only file system */
  case EMLINK          = 31              /* Too many links */
  case EPIPE           = 32              /* Broken pipe */

/* math software */
  case EDOM            = 33              /* Numerical argument out of domain */
  case ERANGE          = 34              /* Result too large */

/* non-blocking and interrupt i/o */
  case EAGAIN          = 35              /* Resource temporarily unavailable */
  static let EWOULDBLOCK = EAGAIN        /* Operation would block */
  case EINPROGRESS     = 36              /* Operation now in progress */
  case EALREADY        = 37              /* Operation already in progress */

/* ipc/network software -- argument errors */
  case ENOTSOCK        = 38              /* Socket operation on non-socket */
  case EDESTADDRREQ    = 39              /* Destination address required */
  case EMSGSIZE        = 40              /* Message too long */
  case EPROTOTYPE      = 41              /* Protocol wrong type for socket */
  case ENOPROTOOPT     = 42              /* Protocol not available */
  case EPROTONOSUPPORT = 43              /* Protocol not supported */
  case ESOCKTNOSUPPORT = 44              /* Socket type not supported */
  case ENOTSUP         = 45              /* Operation not supported */
  case EPFNOSUPPORT    = 46              /* Protocol family not supported */
  case EAFNOSUPPORT    = 47              /* Address family not supported by protocol family */
  case EADDRINUSE      = 48              /* Address already in use */
  case EADDRNOTAVAIL   = 49              /* Can't assign requested address */

/* ipc/network software -- operational errors */
  case ENETDOWN        = 50              /* Network is down */
  case ENETUNREACH     = 51              /* Network is unreachable */
  case ENETRESET       = 52              /* Network dropped connection on reset */
  case ECONNABORTED    = 53              /* Software caused connection abort */
  case ECONNRESET      = 54              /* Connection reset by peer */
  case ENOBUFS         = 55              /* No buffer space available */
  case EISCONN         = 56              /* Socket is already connected */
  case ENOTCONN        = 57              /* Socket is not connected */
  case ESHUTDOWN       = 58              /* Can't send after socket shutdown */
  case ETOOMANYREFS    = 59              /* Too many references: can't splice */
  case ETIMEDOUT       = 60              /* Operation timed out */
  case ECONNREFUSED    = 61              /* Connection refused */

  case ELOOP           = 62              /* Too many levels of symbolic links */
  case ENAMETOOLONG    = 63              /* File name too long */

  case EHOSTDOWN       = 64              /* Host is down */
  case EHOSTUNREACH    = 65              /* No route to host */
  case ENOTEMPTY       = 66              /* Directory not empty */

/* quotas & mush */
  case EPROCLIM        = 67              /* Too many processes */
  case EUSERS          = 68              /* Too many users */
  case EDQUOT          = 69              /* Disc quota exceeded */

/* Network File System */
  case ESTALE          = 70              /* Stale NFS file handle */
  case EREMOTE         = 71              /* Too many levels of remote in path */
  case EBADRPC         = 72              /* RPC struct is bad */
  case ERPCMISMATCH    = 73              /* RPC version wrong */
  case EPROGUNAVAIL    = 74              /* RPC prog. not avail */
  case EPROGMISMATCH   = 75              /* Program version wrong */
  case EPROCUNAVAIL    = 76              /* Bad procedure for program */

  case ENOLCK          = 77              /* No locks available */
  case ENOSYS          = 78              /* Function not implemented */

  case EFTYPE          = 79              /* Inappropriate file type or format */
  case EAUTH           = 80              /* Authentication error */
  case ENEEDAUTH       = 81              /* Need authenticator */

/* Intelligent device errors */
  case EPWROFF         = 82      /* Device power is off */
  case EDEVERR         = 83      /* Device error, e.g. paper out */

  case EOVERFLOW       = 84              /* Value too large to be stored in data type */

/* Program loading errors */
  case EBADEXEC        = 85      /* Bad executable */
  case EBADARCH        = 86      /* Bad CPU type in executable */
  case ESHLIBVERS      = 87      /* Shared library version mismatch */
  case EBADMACHO       = 88      /* Malformed Macho file */

  case ECANCELED       = 89              /* Operation canceled */

  case EIDRM           = 90              /* Identifier removed */
  case ENOMSG          = 91              /* No message of desired type */   
  case EILSEQ          = 92              /* Illegal byte sequence */
  case ENOATTR         = 93              /* Attribute not found */
  case EBADMSG         = 94              /* Bad message */
  case EMULTIHOP       = 95              /* Reserved */
  case ENODATA         = 96              /* No message available on STREAM */
  case ENOLINK         = 97              /* Reserved */
  case ENOSR           = 98              /* No STREAM resources */
  case ENOSTR          = 99              /* Not a STREAM */
  case EPROTO          = 100             /* Protocol error */
  case ETIME           = 101             /* STREAM ioctl timeout */

  case ENOPOLICY       = 103             /* No such policy registered */

  case ENOTRECOVERABLE = 104             /* State not recoverable */
  case EOWNERDEAD      = 105             /* Previous owner died */

  case EQFULL          = 106             /* Interface output queue is full */
  static let ELAST     = EQFULL         /* Must be equal largest errno */

  // FIXME: EOPNOTSUPP has different values depending on __DARWIN_UNIX03 and
  // KERNEL.
}
#endif
