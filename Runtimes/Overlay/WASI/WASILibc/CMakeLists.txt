
gyb_expand(tgmath.swift.gyb tgmath.swift)
gyb_expand(WASILibc.swift.gyb WASILibc.swift)

add_library(swiftWASILibc
  Platform.swift
  POSIXError.swift
  tgmath.swift
  TiocConstants.swift
  WASILibc.swift)
set_target_properties(swiftWASILibc PROPERTIES
  Swift_MODULE_NAME WASILibc)
target_compile_definitions(swiftWASILibc PRIVATE
  $<$<BOOL:${SwiftOverlay_ENABLE_REFLECTION}>:SWIFT_ENABLE_REFLECTION>)
target_link_libraries(swiftWASILibc PUBLIC
  SwiftWASILibc)
target_link_libraries(swiftWASILibc PRIVATE
  swiftCore)

# FIXME: Why is this not implicitly in the interface flags?
target_include_directories(swiftWASILibc INTERFACE
  "$<$<COMPILE_LANGUAGE:Swift>:$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/${SwiftCore_INSTALL_SWIFTMODULEDIR}>>")

install(TARGETS swiftWASILibc
  EXPORT SwiftOverlayTargets
  ARCHIVE DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swiftWASILibc)
install_swift_interface(swiftWASILibc)
