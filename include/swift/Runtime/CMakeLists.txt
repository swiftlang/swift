configure_file(CMakeConfig.h.in ${CMAKE_CURRENT_BINARY_DIR}/CMakeConfig.h
               ESCAPE_QUOTES @ONLY)
swift_install_in_component(FILES ${CMAKE_CURRENT_BINARY_DIR}/CMakeConfig.h
                           DESTINATION "include/swift/Runtime"
                           COMPONENT testsuite-tools)
