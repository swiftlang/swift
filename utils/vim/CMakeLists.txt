install(DIRECTORY
          ftdetect
          syntax
        DESTINATION "share/vim/vim73"
        COMPONENT editor-integration
        PATTERN ".*" EXCLUDE)

