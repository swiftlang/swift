
gyb_expand(tgmath.swift.gyb tgmath.swift)

add_library(swiftCRT
  tgmath.swift
  ucrt.swift
  Platform.swift
  POSIXError.swift
  TiocConstants.swift)
set_target_properties(swiftCRT PROPERTIES
  Swift_MODULE_NAME CRT)
target_compile_definitions(swiftCRT PRIVATE
  $<$<BOOL:${SwiftOverlay_ENABLE_REFLECTION}>:SWIFT_ENABLE_REFLECTION>)
target_compile_options(swiftCRT PRIVATE
  "SHELL:-Xcc -D_USE_MATH_DEFINES")
target_link_libraries(swiftCRT PRIVATE
  ClangModules
  swiftCore)

install(TARGETS swiftCRT
  ARCHIVE DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swiftCRT)
install_swift_interface(swiftCRT)

embed_manifest(swiftCRT)
