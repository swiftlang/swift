
# Install the generated swift interface files for the target.
function(install_swift_interface target)
  # Install binary swift modules
  install(FILES "${CMAKE_CURRENT_BINARY_DIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule"
    RENAME "${SwiftCore_MODULE_TRIPLE}.swiftmodule"
    DESTINATION "${SwiftCore_INSTALL_SWIFTMODULEDIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule"
    COMPONENT SwiftCore_development)
  if(SwiftCore_VARIANT_MODULE_TRIPLE)
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${target}-${SwiftCore_VARIANT_MODULE_TRIPLE}/${target}.swiftmodule"
      RENAME "${SwiftCore_VARIANT_MODULE_TRIPLE}.swiftmodule"
      DESTINATION "${SwiftCore_INSTALL_SWIFTMODULEDIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule"
      COMPONENT SwiftCore_development)
  endif()

  # Install Swift interfaces if library-evolution is enabled
  if(SwiftCore_ENABLE_LIBRARY_EVOLUTION)
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftinterface"
      RENAME "${SwiftCore_MODULE_TRIPLE}.swiftinterface"
      DESTINATION "${SwiftCore_INSTALL_SWIFTMODULEDIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule"
      COMPONENT SwiftCore_development)
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.private.swiftinterface"
      RENAME "${SwiftCore_MODULE_TRIPLE}.private.swiftinterface"
      DESTINATION "${SwiftCore_INSTALL_SWIFTMODULEDIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule"
      COMPONENT SwiftCore_development)

    # Install catalyst interface files
    if(SwiftCore_VARIANT_MODULE_TRIPLE)
      install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${target}-${SwiftCore_VARIANT_MODULE_TRIPLE}/${target}.swiftinterface"
        RENAME "${SwiftCore_VARIANT_MODULE_TRIPLE}.swiftinterface"
        DESTINATION "${SwiftCore_INSTALL_SWIFTMODULEDIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule"
        COMPONENT SwiftCore_development)
      install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${target}-${SwiftCore_VARIANT_MODULE_TRIPLE}/${target}.private.swiftinterface"
        RENAME "${SwiftCore_VARIANT_MODULE_TRIPLE}.private.swiftinterface"
        DESTINATION "${SwiftCore_INSTALL_SWIFTMODULEDIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule"
        COMPONENT SwiftCore_development)
    endif()
  endif()

  # Install Swift documentation interface files.
  install(FILES "${CMAKE_CURRENT_BINARY_DIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftdoc"
    RENAME "${SwiftCore_MODULE_TRIPLE}.swiftdoc"
    DESTINATION "${SwiftCore_INSTALL_SWIFTMODULEDIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule"
    COMPONENT SwiftCore_development)
  if(SwiftCore_VARIANT_MODULE_TRIPLE)
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${target}-${SwiftCore_VARIANT_MODULE_TRIPLE}/${target}.swiftdoc"
      RENAME "${SwiftCore_VARIANT_MODULE_TRIPLE}.swiftdoc"
      DESTINATION "${SwiftCore_INSTALL_SWIFTMODULEDIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule"
      COMPONENT SwiftCore_development)
  endif()
endfunction()
