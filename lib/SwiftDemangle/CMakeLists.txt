add_swift_library(swiftDemangle SHARED
  SwiftDemangle.cpp
  MangleHack.cpp
  LINK_LIBRARIES swiftDemangling)

# We don't need to link against libbsd on MacOS and FreeBSD.
if (NOT APPLE AND (NOT SWIFT_HOST_VARIANT STREQUAL "freebsd"))
  target_link_libraries(swiftDemangle PRIVATE bsd)
endif()

swift_install_in_component(compiler
    TARGETS swiftDemangle
    LIBRARY DESTINATION "lib${LLVM_LIBDIR_SUFFIX}"
    ARCHIVE DESTINATION "lib${LLVM_LIBDIR_SUFFIX}")
