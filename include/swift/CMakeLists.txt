if(SWIFT_BUILD_STDLIB OR SWIFT_BUILD_SDK_OVERLAY OR SWIFT_BUILD_STANDALONE_OVERLAY)
  add_subdirectory(Runtime)
endif()

if(SWIFT_BUILD_REMOTE_MIRROR)
  add_subdirectory(SwiftRemoteMirror)
endif()

if(SWIFT_INCLUDE_TOOLS)
  configure_file(Config.h.in ${CMAKE_CURRENT_BINARY_DIR}/Config.h
                ESCAPE_QUOTES @ONLY)
  add_subdirectory(Option)
  add_subdirectory(Syntax)
endif()

