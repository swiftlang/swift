include(CheckSymbolExists)

check_symbol_exists(el_wgets "histedit.h" HAVE_UNICODE_LIBEDIT)

configure_file(Config.h.in ${CMAKE_CURRENT_BINARY_DIR}/Config.h
               ESCAPE_QUOTES @ONLY)

add_subdirectory(Option)
add_subdirectory(SwiftRemoteMirror)
