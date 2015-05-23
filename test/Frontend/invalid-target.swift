// RUN: not %swift %s -target 6502-apple-ios9.0 2>&1 | FileCheck %s -check-prefix=CHECKARCH
// RUN: not %swift %s -target x86_64-apple-eyeOS 2>&1 | FileCheck %s -check-prefix=CHECKOS

// CHECKARCH: <unknown>:0: error: unsupported target architecture: '6502'
// CHECKOS: <unknown>:0: error: unsupported target OS: 'eyeOS'
