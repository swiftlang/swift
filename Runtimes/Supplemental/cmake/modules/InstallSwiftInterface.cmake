
# Install the generated swift interface files for the target.
function(install_swift_interface target)
  # Install binary swift modules
  install(FILES "${CMAKE_CURRENT_BINARY_DIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule"
    RENAME "${${PROJECT_NAME}_MODULE_TRIPLE}.swiftmodule"
    DESTINATION "${${PROJECT_NAME}_INSTALL_SWIFTMODULEDIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule"
    COMPONENT ${PROJECT_NAME}_development)
  if(${PROJECT_NAME}_VARIANT_MODULE_TRIPLE)
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${target}-${${PROJECT_NAME}_VARIANT_MODULE_TRIPLE}/${target}.swiftmodule"
      RENAME "${${PROJECT_NAME}_VARIANT_MODULE_TRIPLE}.swiftmodule"
      DESTINATION "${${PROJECT_NAME}_INSTALL_SWIFTMODULEDIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule"
      COMPONENT ${PROJECT_NAME}_development)
  endif()

  # Install Swift interfaces if library-evolution is enabled
  if(${PROJECT_NAME}_ENABLE_LIBRARY_EVOLUTION)
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftinterface"
      RENAME "${${PROJECT_NAME}_MODULE_TRIPLE}.swiftinterface"
      DESTINATION "${${PROJECT_NAME}_INSTALL_SWIFTMODULEDIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule"
      COMPONENT ${PROJECT_NAME}_development)
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.private.swiftinterface"
      RENAME "${${PROJECT_NAME}_MODULE_TRIPLE}.private.swiftinterface"
      DESTINATION "${${PROJECT_NAME}_INSTALL_SWIFTMODULEDIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule"
      COMPONENT ${PROJECT_NAME}_development)

    # Install catalyst interface files
    if(${PROJECT_NAME}_VARIANT_MODULE_TRIPLE)
      install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${target}-${${PROJECT_NAME}_VARIANT_MODULE_TRIPLE}/${target}.swiftinterface"
        RENAME "${${PROJECT_NAME}_VARIANT_MODULE_TRIPLE}.swiftinterface"
        DESTINATION "${${PROJECT_NAME}_INSTALL_SWIFTMODULEDIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule"
        COMPONENT ${PROJECT_NAME}_development)
      install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${target}-${${PROJECT_NAME}_VARIANT_MODULE_TRIPLE}/${target}.private.swiftinterface"
        RENAME "${${PROJECT_NAME}_VARIANT_MODULE_TRIPLE}.private.swiftinterface"
        DESTINATION "${${PROJECT_NAME}_INSTALL_SWIFTMODULEDIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule"
        COMPONENT ${PROJECT_NAME}_development)
    endif()
  endif()

  # Install Swift documentation interface files.
  install(FILES "${CMAKE_CURRENT_BINARY_DIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftdoc"
    RENAME "${${PROJECT_NAME}_MODULE_TRIPLE}.swiftdoc"
    DESTINATION "${${PROJECT_NAME}_INSTALL_SWIFTMODULEDIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule"
    COMPONENT ${PROJECT_NAME}_development)
  if(SwiftCore_VARIANT_MODULE_TRIPLE)
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${target}-${${PROJECT_NAME}_VARIANT_MODULE_TRIPLE}/${target}.swiftdoc"
      RENAME "${${PROJECT_NAME}_VARIANT_MODULE_TRIPLE}.swiftdoc"
      DESTINATION "${${PROJECT_NAME}_INSTALL_SWIFTMODULEDIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule"
      COMPONENT ${PROJECT_NAME}_development)
  endif()
endfunction()
