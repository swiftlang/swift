# libswiftRemoteMirror.dylib should not have runtime dependencies; it's
# always built as a shared library.
if(SWIFT_BUILD_DYNAMIC_STDLIB OR SWIFT_BUILD_REMOTE_MIRROR)
  swift_add_target_library(swiftRemoteMirror
                           SHARED DONT_EMBED_BITCODE NOSWIFTRT
                           SwiftRemoteMirror.cpp
                           LINK_LIBRARIES
                             swiftReflection
                           C_COMPILE_FLAGS
                             ${SWIFT_RUNTIME_CXX_FLAGS} -DswiftRemoteMirror_EXPORTS
                           LINK_FLAGS
                             ${SWIFT_RUNTIME_LINK_FLAGS}
                           INSTALL_IN_COMPONENT
                             swift-remote-mirror)
endif()
