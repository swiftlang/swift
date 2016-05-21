add_swift_library(swiftRemoteMirror SHARED TARGET_LIBRARY
  SwiftRemoteMirror.cpp
  LINK_LIBRARIES swiftReflection
  INSTALL_IN_COMPONENT stdlib)

