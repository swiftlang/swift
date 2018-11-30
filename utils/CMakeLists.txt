add_subdirectory(vim)
add_subdirectory(lldb)

swift_install_in_component(editor-integration swift-mode-el
    FILES swift-mode.el
    DESTINATION "share/emacs/site-lisp")

swift_install_in_component(tools swift-api-dump-py
    FILES swift-api-dump.py
    DESTINATION bin)

# We install LLVM's FileCheck, if requested.
swift_install_in_component(toolchain-dev-tools swift-FileCheck
    FILES "${SWIFT_PATH_TO_LLVM_BUILD}/bin/FileCheck"
    DESTINATION bin)
