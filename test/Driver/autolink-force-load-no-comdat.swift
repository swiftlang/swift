// RUN: not %swiftc_driver -incremental -autolink-force-load %s 2>&1 | %FileCheck -check-prefix=AUTOLINK_FORCE_LOAD %s
// RUN: not %swiftc_driver -autolink-force-load -incremental %s 2>&1 | %FileCheck -check-prefix=AUTOLINK_FORCE_LOAD %s

// MACHO targets do not support COMDAT
// REQUIRES-ANY: OS=macosx, OS=tvos, OS=watchos, OS=ios

// AUTOLINK_FORCE_LOAD: error: '-autolink-force-load' is not supported with '-incremental'

