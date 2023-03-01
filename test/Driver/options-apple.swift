// REQUIRES: swift_interpreter
// REQUIRES: OS=macosx

// RUN: %swift_driver -link-objc-runtime %s 2>&1 | %FileCheck -check-prefix LINK_OBJC_RUNTIME_WARNING %s
// RUN: %swift_driver -no-link-objc-runtime %s 2>&1 | %FileCheck -check-prefix LINK_OBJC_RUNTIME_WARNING %s
// LINK_OBJC_RUNTIME_WARNING: warning: -link-objc-runtime is no longer supported on Apple platforms
