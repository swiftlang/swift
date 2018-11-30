swift_install_in_component(editor-integration vim-editor-integration
    DIRECTORY
      ftdetect
      syntax
    DESTINATION "share/vim/vim73"
    PATTERN ".*" EXCLUDE)

