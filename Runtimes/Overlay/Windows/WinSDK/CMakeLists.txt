
add_library(swiftWinSDK
  WinSDK.swift)
set_target_properties(swiftWinSDK PROPERTIES
  Swift_MODULE_NAME WinSDK)
target_compile_definitions(swiftCRT PRIVATE
  $<$<BOOL:${SwiftOverlay_ENABLE_REFLECTION}>:SWIFT_ENABLE_REFLECTION>)
target_link_libraries(swiftWinSDK PRIVATE
  ClangModules
  swiftCore)

install(TARGETS swiftWinSDK
  EXPORT SwiftOverlayTargets
  ARCHIVE DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swiftWinSDK)
install_swift_interface(swiftWinSDK)

embed_manifest(swiftWinSDK)
