add_subdirectory(clang)

gyb_expand(Glibc.swift.gyb Glibc.swift)
gyb_expand(tgmath.swift.gyb tgmath.swift)

add_library(swiftGlibc
  Glibc.swift
  POSIXError.swift
  Platform.swift
  TiocConstants.swift
  tgmath.swift)

target_link_libraries(swiftGlibc
  PRIVATE
    swiftCore
    SwiftGlibcClangOverlay)

set_target_properties(swiftGlibc PROPERTIES
  Swift_MODULE_NAME Glibc)

target_compile_options(swiftGlibc PRIVATE
  "SHELL:-Xcc -fno-omit-frame-pointer"
  "SHELL:-Xfrontend -empty-abi-descriptor"
  "SHELL:-Xfrontend -prespecialize-generic-metadata"
  "SHELL:-Xfrontend -require-explicit-availability=ignore"
  "SHELL:-library-level api")

install(TARGETS swiftGlibc
  EXPORT SwiftOverlayTargets
  ARCHIVE DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swiftGlibc)
install_swift_interface(swiftGlibc)
embed_manifest(swiftGlibc)
