// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

/// Generate cdecl.h for an app
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) \
// RUN:   %t/Lib.swift -emit-module -verify -o %t -emit-module-doc \
// RUN:   -emit-clang-header-path %t/cdecl.h -package-name pkg \
// RUN:   -enable-experimental-feature CDecl
// RUN: %FileCheck %s --input-file %t/cdecl.h --check-prefixes PUBLIC-AND-INTERNAL,INTERNAL-ONLY
// RUN: %check-in-clang-c %t/cdecl.h

/// Generate cdecl.h for a library
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library \
// RUN:   %t/Lib.swift -emit-module -verify -o %t -emit-module-doc \
// RUN:   -emit-clang-header-path %t/cdecl.h -package-name pkg \
// RUN:   -enable-experimental-feature CDecl
// RUN: %FileCheck %s --input-file %t/cdecl.h --check-prefixes PUBLIC-AND-INTERNAL
// RUN: %FileCheck %s --input-file %t/cdecl.h --implicit-check-not INTERNAL-ONLY
// RUN: %check-in-clang-c %t/cdecl.h

// REQUIRES: swift_feature_CDecl

//--- Lib.swift

@cdecl private enum PrivateEnum: CInt { case A, B }
// PUBLIC-AND-INTERNAL-NOT: PrivateEnum

@cdecl internal enum InternalEnum: CInt { case A, B }
// INTERNAL-ONLY: typedef SWIFT_ENUM(int, InternalEnum, closed) {
// INTERNAL-ONLY:   InternalEnumA = 0,
// INTERNAL-ONLY:   InternalEnumB = 1,
// INTERNAL-ONLY: };

@cdecl package enum PackageEnum: CInt { case A, B }
// INTERNAL-ONLY: typedef SWIFT_ENUM(int, PackageEnum, closed) {
// INTERNAL-ONLY:   PackageEnumA = 0,
// INTERNAL-ONLY:   PackageEnumB = 1,
// INTERNAL-ONLY: };

@cdecl public enum PublicEnum: CInt { case A, B }
// PUBLIC-AND-INTERNAL: typedef SWIFT_ENUM(int, PublicEnum, closed) {
// PUBLIC-AND-INTERNAL:   PublicEnumA = 0,
// PUBLIC-AND-INTERNAL:   PublicEnumB = 1,
// PUBLIC-AND-INTERNAL: };

/// Private documentation
@cdecl private func a_private() {}
// PUBLIC-AND-INTERNAL-NOT: // Private documentation
// PUBLIC-AND-INTERNAL-NOT: a_private

/// Internal documentation
@cdecl internal func b_internal() {}
// INTERNAL-ONLY: // Internal documentation
// INTERNAL-ONLY: b_internal

/// Package documentation
@cdecl package func c_package() {}
// INTERNAL-ONLY: // Package documentation
// INTERNAL-ONLY: c_package

/// Public documentation
@cdecl public func d_public() {}
// PUBLIC-AND-INTERNAL: // Public documentation
// PUBLIC-AND-INTERNAL: d_public
