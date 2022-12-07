// RUN: %empty-directory(%t)
// RUN: %target-clang %s -std=c++11 -I %swift_src_root/include -I %swift_src_root/stdlib/public/SwiftShims -I %clang-include-dir -isysroot %sdk %platform-dylib-dir/%target-library-name(swiftCore) -o %t/paths-test
// RUN: %target-codesign %t/paths-test
// RUN: %target-run %t/paths-test | %FileCheck %s

// REQUIRES: executable_test
// UNSUPPORTED: remote_run

// This can't be done in unittests, because that statically links the runtime
// so we get the wrong paths.  We explicitly want to test that we get the
// path we expect (that is, the path to the runtime, and paths relative to
// that).

#include "swift/Runtime/Paths.h"

#include <sys/types.h>
#include <sys/stat.h>

#include <stdio.h>
#include <string.h>

#if defined(_WIN32) && !defined(__CYGWIN)
#define stat _stat
#define S_IFDIR _S_IFDIR
#endif

static bool
exists(const char *path) {
  struct stat st;

  return stat(path, &st) == 0;
}

static bool
isdir(const char *path) {
  struct stat st;

  return stat(path, &st) == 0 && (st.st_mode & S_IFDIR);
}

static bool
isfile(const char *path) {
  struct stat st;

  return stat(path, &st) == 0 && !(st.st_mode & S_IFDIR);
}

int main(void) {
  const char *runtimePath = swift_getRuntimePath();

  // Runtime path must point to libswiftCore and must be a file.

  // CHECK: runtime path: {{.*[\\/]}}libswiftCore.{{so|dylib|dll}}
  // CHECK-NEXT: runtime is a file: yes
  printf("runtime path: %s\n", runtimePath ? runtimePath: "<NULL>");
  printf("runtime is a file: %s\n", isfile(runtimePath) ? "yes" : "no");

  const char *rootPath = swift_getRootPath();

  // Root path must end in a separator and must be a directory

  // CHECK: root path: {{.*[\\/]$}}
  // CHECK-NEXT: root is a directory: yes
  printf("root path: %s\n", rootPath ? rootPath : "<NULL>");
  printf("root is a directory: %s\n", isdir(rootPath) ? "yes" : "no");

  // Auxiliary executable path must be in swift-root/libexec.
#define UNLIKELY_NAME "anUnlikelyExecutableName"

  const size_t unlikelyLen = strlen(UNLIKELY_NAME);
  const char *auxPath = swift_getAuxiliaryExecutablePath(UNLIKELY_NAME);

  // CHECK: aux path: {{.*[\\/](libexec[\\/])?}}anUnlikelyExecutableName{{(\.exe)?}}
  printf("aux path: %s\n", auxPath ? auxPath : "<NULL>");

  return 0;
}
