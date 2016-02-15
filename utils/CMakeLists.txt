add_subdirectory(vim)

if(SWIFT_INSTALL_TOOLS)
  # FIXME: install gyb.
endif()

swift_install_in_component(editor-integration
    FILES swift-mode.el
    DESTINATION "share/emacs/site-lisp")

swift_install_in_component(tools
    FILES swift-api-dump.py
    RUNTIME DESTINATION bin)

# Create a symlink for swift-api-dump.py in the bin directory
add_custom_target(swift-api-dump ALL
    COMMAND
      "${CMAKE_COMMAND}" "-E" "create_symlink"
      "${CMAKE_CURRENT_SOURCE_DIR}/swift-api-dump.py"
      "${SWIFT_RUNTIME_OUTPUT_INTDIR}/swift-api-dump.py"
    COMMENT "Creating development symlink for swift-api-dump.py")
