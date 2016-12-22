add_swift_library(swiftDemangle SHARED
  SwiftDemangle.cpp
  MangleHack.cpp
  LINK_LIBRARIES swiftBasic)

swift_install_in_component(compiler
    TARGETS swiftDemangle
    LIBRARY DESTINATION "lib${LLVM_LIBDIR_SUFFIX}"
    ARCHIVE DESTINATION "lib${LLVM_LIBDIR_SUFFIX}")

# Create a compatibility symlink.

swift_create_post_build_symlink(sourcekitd
  IS_DIRECTORY
  SOURCE "${SWIFTLIB_DIR}"
  DESTINATION "${SOURCEKIT_LIBRARY_OUTPUT_INTDIR}/swift"
  COMMENT "Creating compatibility symlink for functionNameDemangle.dylib")

swift_install_in_component(compiler
    FILES "${SWIFT_LIBRARY_OUTPUT_INTDIR}/libfunctionNameDemangle.dylib"
    DESTINATION "lib")

