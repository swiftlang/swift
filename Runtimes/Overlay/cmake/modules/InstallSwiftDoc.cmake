
function(install_swift_doc target)
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
