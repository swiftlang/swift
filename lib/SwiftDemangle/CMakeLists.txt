add_swift_library(swiftDemangle SHARED
  SwiftDemangle.cpp
  MangleHack.cpp
  LINK_LIBRARIES swiftBasic)

swift_install_in_component(compiler
    TARGETS swiftDemangle
    LIBRARY DESTINATION "lib${LLVM_LIBDIR_SUFFIX}"
    ARCHIVE DESTINATION "lib${LLVM_LIBDIR_SUFFIX}")

# Create a compatibility symlink.

add_custom_command_target(unused_var
    COMMAND
      "${CMAKE_COMMAND}" "-E" "make_directory"
      "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib"
    COMMAND
      "${CMAKE_COMMAND}" "-E" "create_symlink"
      "libswiftDemangle.dylib"
      "libfunctionNameDemangle.dylib"

    WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib"
    CUSTOM_TARGET_NAME "functionNameDemangle_compat_symlink"
    OUTPUT "${SWIFTLIB_DIR}/lib/libfunctionNameDemangle.dylib"
    COMMENT "Creating compatibility symlink for functionNameDemangle.dylib")
add_dependencies(swiftDemangle functionNameDemangle_compat_symlink)

swift_install_in_component(compiler
    FILES "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib/libfunctionNameDemangle.dylib"
    DESTINATION "lib")

