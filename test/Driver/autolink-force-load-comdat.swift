// RUN: %swiftc_driver -incremental -autolink-force-load %s 2>&1 | %FileCheck -check-prefix=AUTOLINK_FORCE_LOAD %s
// RUN: %swiftc_driver -autolink-force-load -incremental %s 2>&1 | %FileCheck -check-prefix=AUTOLINK_FORCE_LOAD %s

// MACHO targets do not support COMDAT
// UNSUPPORTED: OS=macosx
// UNSUPPORTED: OS=tvos
// UNSUPPORTED: OS=watchos
// UNSUPPORTED: OS=ios

// AUTOLINK_FORCE_LOAD-NOT: error: '-autolink-force-load' is not supported with '-incremental'

