add_swift_library(swiftRemoteMirror SHARED TARGET_LIBRARY DONT_EMBED_BITCODE
  SwiftRemoteMirror.cpp
  LINK_LIBRARIES swiftReflection
  INSTALL_IN_COMPONENT stdlib)

