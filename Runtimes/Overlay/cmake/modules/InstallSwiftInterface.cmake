
# Install the generated swift interface files for the target.
function(install_swift_interface target)
  # Install binary swift modules
  install(FILES "${CMAKE_CURRENT_BINARY_DIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule"
    RENAME "${SwiftOverlay_MODULE_TRIPLE}.swiftmodule"
    DESTINATION "${SwiftOverlay_INSTALL_SWIFTMODULEDIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule")
  if(SwiftOverlay_VARIANT_MODULE_TRIPLE)
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${target}-${SwiftOverlay_VARIANT_MODULE_TRIPLE}/${target}.swiftmodule"
      RENAME "${SwiftOverlay_VARIANT_MODULE_TRIPLE}.swiftmodule"
      DESTINATION "${SwiftOverlay_INSTALL_SWIFTMODULEDIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule")
  endif()

  # Install Swift interfaces if library-evolution is enabled
  if(SwiftOverlay_ENABLE_LIBRARY_EVOLUTION)
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftinterface"
      RENAME "${SwiftOverlay_MODULE_TRIPLE}.swiftinterface"
      DESTINATION "${SwiftOverlay_INSTALL_SWIFTMODULEDIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule")

    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.private.swiftinterface"
      RENAME "${SwiftOverlay_MODULE_TRIPLE}.private.swiftinterface"
      DESTINATION "${SwiftOverlay_INSTALL_SWIFTMODULEDIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule")

    # Install catalyst interface files
    if(SwiftOverlay_VARIANT_MODULE_TRIPLE)
      install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${target}-${SwiftOverlay_VARIANT_MODULE_TRIPLE}/${target}.swiftinterface"
        RENAME "${SwiftOverlay_VARIANT_MODULE_TRIPLE}.swiftinterface"
        DESTINATION "${SwiftOverlay_INSTALL_SWIFTMODULEDIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule")
      install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${target}-${SwiftOverlay_VARIANT_MODULE_TRIPLE}/${target}.private.swiftinterface"
        RENAME "${SwiftOverlay_VARIANT_MODULE_TRIPLE}.private.swiftinterface"
        DESTINATION "${SwiftOverlay_INSTALL_SWIFTMODULEDIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule")
    endif()
  endif()

  # Install Swift documentation interface files.
  install(FILES "${CMAKE_CURRENT_BINARY_DIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftdoc"
    RENAME "${SwiftOverlay_MODULE_TRIPLE}.swiftdoc"
    DESTINATION "${SwiftOverlay_INSTALL_SWIFTMODULEDIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule"
    COMPONENT SwiftOverlay_development)
  if(SwiftOverlay_VARIANT_MODULE_TRIPLE)
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${target}-${SwiftOverlay_VARIANT_MODULE_TRIPLE}/${target}.swiftdoc"
      RENAME "${SwiftOverlay_VARIANT_MODULE_TRIPLE}.swiftdoc"
      DESTINATION "${SwiftOverlay_INSTALL_SWIFTMODULEDIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule"
      COMPONENT SwiftOverlay_development)
  endif()
endfunction()
