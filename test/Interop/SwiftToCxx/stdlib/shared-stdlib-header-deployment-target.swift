// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %empty-directory(%t/include/swift)
// RUN: %target-swift-frontend -parse-as-library %platform-module-dir/Swift.swiftmodule/%module-target-triple.swiftmodule -module-name Swift -parse-stdlib -typecheck -cxx-interoperability-mode=default -clang-header-expose-decls=has-expose-attr-or-stdlib -target %target-cpu-apple-macosx12 -emit-clang-header-path %t/include/swift/Swift.h

// The same header selects primitive metadata traits for the C++ deployment target.
// RUN: %target-interop-build-clangxx -std=c++17 -target %target-cpu-apple-macosx12 -DDEBUG=1 -c %t/client.cpp -I %t/include -o %t/old.o
// RUN: %llvm-nm --undefined-only %t/old.o | %FileCheck %s --check-prefix=OLD --implicit-check-not='$ss6Int128VN' --implicit-check-not='$ss7UInt128VN'
// RUN: %target-interop-build-clangxx -std=c++17 -target %target-cpu-apple-macosx15 -DDEBUG=1 -DNEW_RUNTIME -c %t/client.cpp -I %t/include -o %t/new.o
// RUN: %llvm-nm --undefined-only %t/new.o | %FileCheck %s --check-prefix=NEW

// REQUIRES: OS=macosx
// REQUIRES: PTRSIZE=64

// OLD: $sSiN
// NEW: $ss6Int128VN
// NEW: $ss7UInt128VN

//--- client.cpp
#include <swift/Swift.h>

static_assert(swift::isUsableInGenericContext<swift::Int>, "Int metadata is available");

#ifdef NEW_RUNTIME
static_assert(swift::isUsableInGenericContext<__int128>, "Int128 metadata is available");
static_assert(swift::isUsableInGenericContext<unsigned __int128>, "UInt128 metadata is available");
void *int128Metadata() {
  return swift::TypeMetadataTrait<__int128>::getTypeMetadata();
}
void *uint128Metadata() {
  return swift::TypeMetadataTrait<unsigned __int128>::getTypeMetadata();
}
#else
static_assert(!swift::isUsableInGenericContext<__int128>, "Int128 metadata is unavailable");
static_assert(!swift::isUsableInGenericContext<unsigned __int128>, "UInt128 metadata is unavailable");
#endif

void *intMetadata() {
  return swift::TypeMetadataTrait<swift::Int>::getTypeMetadata();
}
