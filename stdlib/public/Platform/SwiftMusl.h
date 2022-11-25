//===--- SwiftMusl.h ------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include  <stdc-predef.h>
#include  <features.h>

// C standard library
#include  <complex.h>
#include  <ctype.h>
#include  <errno.h>
#include  <fenv.h>
#include  <float.h>
#include  <inttypes.h>
#include  <iso646.h>
#include  <limits.h>
#include  <locale.h>
#include  <math.h>
#include  <pty.h>
#include  <setjmp.h>
#include  <signal.h>
#include  <stdarg.h>
#include  <stdbool.h>
#include  <stddef.h>
#include  <stdint.h>
#include  <stdio.h>
#include  <stdlib.h>
#include  <string.h>
#include  <tgmath.h>
#include  <time.h>
#include  <utmp.h>

// POSIX
#include  <aio.h>
#include  <arpa/inet.h>
#include  <cpio.h>
#include  <dirent.h>
#include  <dlfcn.h>
#include  <fcntl.h>
#include  <fmtmsg.h>
#include  <fnmatch.h>
#include  <ftw.h>
#include  <glob.h>
#include  <grp.h>
#include  <iconv.h>
#include  <ifaddrs.h>
#include  <langinfo.h>
#include  <libgen.h>
#include  <link.h>
#include  <monetary.h>
#include  <net/if.h>
#include  <netdb.h>
#include  <netinet/in.h>
#include  <netinet/tcp.h>
#include  <nl_types.h>
#include  <poll.h>
#include  <pthread.h>
#include  <pwd.h>
#include  <regex.h>
#include  <sched.h>
#include  <search.h>
#include  <semaphore.h>
#include  <spawn.h>
#include  <strings.h>
#include  <sys/file.h>
#include  <sys/inotify.h>
#include  <sys/ioctl.h>
#include  <sys/ipc.h>
#include  <sys/mman.h>
#include  <sys/mount.h>
#include  <sys/msg.h>
#include  <sys/resource.h>
#include  <sys/select.h>
#include  <sys/sem.h>
#include  <sys/sendfile.h>
#include  <sys/shm.h>
#include  <sys/socket.h>
#include  <sys/stat.h>
#include  <sys/statvfs.h>
#include  <sys/time.h>
#include  <sys/times.h>
#include  <sys/types.h>
#include  <sys/uio.h>
#include  <sys/un.h>
#include  <sys/user.h>
#include  <sys/utsname.h>
#include  <sys/wait.h>
#include  <sysexits.h>
#include  <syslog.h>
#include  <tar.h>
#include  <termios.h>
#include  <ulimit.h>
#include  <unistd.h>
#include  <utime.h>
#include  <utmpx.h>
#include  <wordexp.h>
