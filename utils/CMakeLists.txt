add_subdirectory(vim)

swift_install_in_component(editor-integration
    FILES swift-mode.el
    DESTINATION "share/emacs/site-lisp")

swift_install_in_component(tools
    FILES swift-api-dump.py
    DESTINATION bin)

# We install LLVM's FileCheck, if requested.
swift_install_in_component(toolchain-dev-tools
    FILES "${SWIFT_PATH_TO_LLVM_BUILD}/bin/FileCheck"
    DESTINATION bin)
