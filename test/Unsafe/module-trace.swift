// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/unsafe_swift_decls.swiftmodule %S/Inputs/unsafe_swift_decls.swift -enable-experimental-feature AllowUnsafeAttribute
// RUN: %target-swift-frontend -emit-module-path %t/safe_swift_decls.swiftmodule %S/Inputs/safe_swift_decls.swift -enable-experimental-feature AllowUnsafeAttribute -enable-experimental-feature WarnUnsafe

// RUN: %target-typecheck-verify-swift -enable-experimental-feature AllowUnsafeAttribute -enable-experimental-feature WarnUnsafe -I %S/Inputs -I %t -emit-loaded-module-trace-path %t/unsafe.trace

// RUN: %FileCheck -check-prefix TRACE %s < %t/unsafe.trace

// REQUIRES: swift_feature_AllowUnsafeAttribute
// REQUIRES: swift_feature_WarnUnsafe

import unsafe_decls
import unsafe_swift_decls
import safe_swift_decls

// Module-level indication
// TRACE: "strictMemorySafety":true

// Dependencies
// TRACE: "safe_swift_decls"{{.*}}"strictMemorySafety":true
// TRACE: "unsafe_swift_decls"{{.*}}"strictMemorySafety":false
