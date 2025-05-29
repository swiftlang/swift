// RUN: %target-swift-frontend -Xcc -femulated-tls %s -S -import-objc-header %S/Inputs/tls.h | %FileCheck %s --check-prefix=EMUTLS --check-prefix=EMUTLS-%target-os
// RUN: %target-swift-frontend -Xcc -fno-emulated-tls %s -S -import-objc-header %S/Inputs/tls.h | %FileCheck %s --check-prefix=NOEMUTLS

_swift_stdlib_gettid()

// EMUTLS: __emutls_v._swift_stdlib_gettid.tid
// EMUTLS-linux-android: __emutls_get_address
// EMUTLS-linux-gnu: __emutls_get_address
// EMUTLS-macosx: __emutls_get_address
// EMUTLS-openbsd: __emutls_get_address
// EMUTLS-windows-msvc: __emutls_get_address
// EMUTLS-wasi-NOT: __emutls_get_address

// NOEMUTLS-NOT: __emutls_v._swift_stdlib_gettid.tid
// NOEMUTLS-NOT: __emutls_get_address
