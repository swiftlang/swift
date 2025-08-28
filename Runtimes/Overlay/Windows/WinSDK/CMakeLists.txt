
add_library(swiftWinSDK
  WinSDK.swift)
set_target_properties(swiftWinSDK PROPERTIES
  Swift_MODULE_NAME WinSDK)
target_compile_definitions(swiftWinSDK PRIVATE
  $<$<BOOL:${SwiftOverlay_ENABLE_REFLECTION}>:SWIFT_ENABLE_REFLECTION>)
target_compile_options(swiftWinSDK PRIVATE
  "SHELL:-Xfrontend -disable-force-load-symbols")
target_link_libraries(swiftWinSDK PUBLIC
  ClangModules)
target_link_libraries(swiftWinSDK PRIVATE
  swiftCore)

# FIXME: Why is this not implicitly in the interface flags?
target_include_directories(swiftWinSDK INTERFACE
  "$<$<COMPILE_LANGUAGE:Swift>:$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/${SwiftCore_INSTALL_SWIFTMODULEDIR}>>")

install(TARGETS swiftWinSDK
  EXPORT SwiftOverlayTargets
  ARCHIVE DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swiftWinSDK)
install_swift_interface(swiftWinSDK)

embed_manifest(swiftWinSDK)
