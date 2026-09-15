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

// OLD-NOT: $ss6Int128VN
// OLD-NOT: $ss7UInt128VN
// OLD-NOT: isUsableInGenericContext<__int128>
// OLD: // type metadata address for CChar32.
// OLD: struct TypeMetadataTrait<char32_t> {
// OLD-NOT: $ss6Int128VN
// OLD-NOT: $ss7UInt128VN
// OLD-NOT: isUsableInGenericContext<__int128>

// NEW: // type metadata address for Int128.
// NEW-NEXT: SWIFT_IMPORT_STDLIB_SYMBOL extern size_t $ss6Int128VN;
// NEW-NEXT: // type metadata address for UInt128.
// NEW-NEXT: SWIFT_IMPORT_STDLIB_SYMBOL extern size_t $ss7UInt128VN;
// NEW: struct TypeMetadataTrait<__int128> {
