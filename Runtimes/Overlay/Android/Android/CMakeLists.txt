
gyb_expand(tgmath.swift.gyb tgmath.swift)

add_library(swiftAndroid
  tgmath.swift
  Android.swift
  Platform.swift
  POSIXError.swift
  TiocConstants.swift)
set_target_properties(swiftAndroid PROPERTIES
  Swift_MODULE_NAME Android)
target_compile_definitions(swiftAndroid PRIVATE
  $<$<BOOL:${SwiftOverlay_ENABLE_REFLECTION}>:SWIFT_ENABLE_REFLECTION>)
target_link_libraries(swiftAndroid PRIVATE
  SwiftAndroid
  swiftCore)

install(TARGETS swiftAndroid
  EXPORT SwiftOverlayTargets
  ARCHIVE DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swiftAndroid)
install_swift_interface(swiftAndroid)

embed_manifest(swiftAndroid)
