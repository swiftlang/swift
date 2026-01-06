
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
target_link_libraries(swiftAndroid PUBLIC
  SwiftAndroid)
target_link_libraries(swiftAndroid PRIVATE
  swiftCore)

# FIXME: Why is this not implicitly in the interface flags?
target_include_directories(swiftAndroid INTERFACE
  "$<$<COMPILE_LANGUAGE:Swift>:$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/${SwiftCore_INSTALL_SWIFTMODULEDIR}>>")

install(TARGETS swiftAndroid
  EXPORT SwiftOverlayTargets
  ARCHIVE DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swiftAndroid)
install_swift_interface(swiftAndroid)

embed_manifest(swiftAndroid)
