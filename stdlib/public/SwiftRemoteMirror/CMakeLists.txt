add_swift_library(swiftRemoteMirror SHARED IS_STDLIB
  SwiftRemoteMirror.cpp
  LINK_LIBRARIES swiftReflection
  INSTALL_IN_COMPONENT stdlib)

