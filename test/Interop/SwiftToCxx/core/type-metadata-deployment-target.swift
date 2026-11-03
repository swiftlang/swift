// Type metadata for standard library types that were introduced after the
// deployment target must not be referenced through the shared header, as that
// would prevent the C++ client from launching on an older runtime.

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %s -module-name Core -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/core-old.h -target %target-cpu-apple-macosx12
// RUN: %target-interop-build-clangxx -std=c++17 -target %target-cpu-apple-macosx12 -x c++ -E -P %t/core-old.h | %FileCheck --check-prefix=OLD %s

// RUN: %target-swift-frontend %s -module-name Core -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/core-new.h -target %target-cpu-apple-macosx15
// RUN: %target-interop-build-clangxx -std=c++17 -target %target-cpu-apple-macosx15 -x c++ -E -P %t/core-new.h | %FileCheck --check-prefix=NEW %s

// RUN: %target-interop-build-clangxx -std=c++17 -target %target-cpu-apple-macosx12 -x c++ %t/core-old.h -c -o %t/core-old.o -DDEBUG=1
// RUN: %target-interop-build-clangxx -std=c++17 -target %target-cpu-apple-macosx15 -x c++ %t/core-new.h -c -o %t/core-new.o -DDEBUG=1

// REQUIRES: OS=macosx
// REQUIRES: PTRSIZE=64

public func takesInt(_ x: Int) {}

// OLD-NOT: $ss6Int128VN
// OLD-NOT: $ss7UInt128VN
// OLD-NOT: isUsableInGenericContext<__int128>
// OLD: struct TypeMetadataTrait<char32_t> {
// OLD-NOT: $ss6Int128VN
// OLD-NOT: $ss7UInt128VN
// OLD-NOT: isUsableInGenericContext<__int128>

// NEW: extern size_t $ss6Int128VN;
// NEW-NEXT: extern size_t $ss7UInt128VN;
// NEW: struct TypeMetadataTrait<__int128> {
