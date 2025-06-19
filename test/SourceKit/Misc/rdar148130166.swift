// RUN: env DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib %sourcekitd-test -req=active-regions %s -- %s -module-name B -module-alias A=B
// Make sure we don't crash.

// guardmalloc is incompatible with ASAN
// REQUIRES: no_asan
