// Type metadata for standard library types that were introduced after the
// deployment target must not be referenced from the generated header, as that
// would prevent the C++ client from launching on an older runtime.

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %s -module-name Core -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/core-old.h -target %target-cpu-apple-macosx12
// RUN: %FileCheck --check-prefix=OLD %s < %t/core-old.h

// RUN: %target-swift-frontend %s -module-name Core -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/core-new.h -target %target-cpu-apple-macosx15
// RUN: %FileCheck --check-prefix=NEW %s < %t/core-new.h

// RUN: %target-interop-build-clangxx -x c++ %t/core-new.h -c -o %t/core-new.o -DDEBUG=1

// REQUIRES: OS=macosx
// REQUIRES: PTRSIZE=64

public func takesInt(_ x: Int) {}

// OLD: // type metadata address for CChar32.
// OLD: struct TypeMetadataTrait<char32_t> {

// NEW: // type metadata address for CChar32.
// NEW: struct TypeMetadataTrait<char32_t> {
