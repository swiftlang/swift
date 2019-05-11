add_subdirectory(Runtime)

if(SWIFT_BUILD_REMOTE_MIRROR)
  add_subdirectory(SwiftRemoteMirror)
endif()

if(SWIFT_INCLUDE_TOOLS)
  configure_file(Config.h.in ${CMAKE_CURRENT_BINARY_DIR}/Config.h
                ESCAPE_QUOTES @ONLY)
  add_subdirectory(Option)
  add_subdirectory(Parse)
  add_subdirectory(Syntax)
endif()

