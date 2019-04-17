add_subdirectory(vim)
add_subdirectory(lldb)
add_subdirectory(api_checker)

install(FILES swift-mode.el
        DESTINATION "share/emacs/site-lisp"
        COMPONENT editor-integration)

install(FILES swift-api-dump.py
        DESTINATION bin
        COMPONENT tools)

# We install LLVM's FileCheck, if requested.
file(TO_CMAKE_PATH "${LLVM_BUILD_BINARY_DIR}/bin/FileCheck${CMAKE_EXECUTABLE_SUFFIX}" _SWIFT_UTILS_FILECHECK)
install(FILES "${_SWIFT_UTILS_FILECHECK}"
        DESTINATION bin
        COMPONENT toolchain-dev-tools)
