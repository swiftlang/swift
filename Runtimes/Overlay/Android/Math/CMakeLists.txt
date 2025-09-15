
add_library(swift_math
  Math.swift)
set_target_properties(swift_math PROPERTIES
  Swift_MODULE_NAME math)
target_link_libraries(swift_math PRIVATE
  SwiftAndroid
  swiftCore)

install(TARGETS swift_math
  EXPORT SwiftOverlayTargets
  ARCHIVE DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swift_math)
install_swift_interface(swift_math)

embed_manifest(swift_math)
