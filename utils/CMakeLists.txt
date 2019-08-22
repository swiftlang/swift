add_subdirectory(vim)
add_subdirectory(lldb)
add_subdirectory(api_checker)

swift_install_in_component(FILES swift-mode.el
                           DESTINATION "share/emacs/site-lisp"
                           COMPONENT editor-integration)

swift_install_in_component(PROGRAMS swift-api-dump.py
                           DESTINATION bin
                           COMPONENT tools)

# We install LLVM's FileCheck, if requested.
file(TO_CMAKE_PATH "${LLVM_BUILD_BINARY_DIR}/bin/FileCheck${CMAKE_EXECUTABLE_SUFFIX}" _SWIFT_UTILS_FILECHECK)
swift_install_in_component(PROGRAMS "${_SWIFT_UTILS_FILECHECK}"
                           DESTINATION bin
                           COMPONENT toolchain-dev-tools)
