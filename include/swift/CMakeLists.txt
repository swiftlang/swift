configure_file(Config.h.in ${CMAKE_CURRENT_BINARY_DIR}/Config.h
               ESCAPE_QUOTES @ONLY)

add_subdirectory(Option)
add_subdirectory(Runtime)
add_subdirectory(SwiftRemoteMirror)
add_subdirectory(Syntax)
