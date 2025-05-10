
function(install_swift_doc target)
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
