
function(install_swift_doc target)
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
