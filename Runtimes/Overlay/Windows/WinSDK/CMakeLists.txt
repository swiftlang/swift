
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

emit_swift_interface(swiftWinSDK)
install(TARGETS swiftWinSDK
  EXPORT SwiftOverlayTargets
  ARCHIVE DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
  FILE_SET swift_modules
    COMPONENT SwiftOverlay_development
    DESTINATION "${SwiftOverlay_INSTALL_SWIFTMODULEDIR}")

embed_manifest(swiftWinSDK)
