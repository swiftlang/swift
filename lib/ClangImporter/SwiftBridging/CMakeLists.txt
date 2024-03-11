# Mark - copy "bridging" (support header) into the local include directory and
# install it into the compiler toolchain.
set(SWIFTINC_DIR
    "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/include/swift")

add_custom_command(
    OUTPUT "${SWIFTINC_DIR}/bridging"
    DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/swift/bridging"
    COMMAND "${CMAKE_COMMAND}" "-E" "copy" "${CMAKE_CURRENT_SOURCE_DIR}/swift/bridging" "${SWIFTINC_DIR}")

add_custom_target("copy_cxxInterop_support_header"
    DEPENDS "${SWIFTINC_DIR}/bridging"
    COMMENT "Copying C++ interop support header to ${SWIFTINC_DIR}")

swift_install_in_component(FILES
    "${CMAKE_CURRENT_SOURCE_DIR}/swift/bridging"
    "${CMAKE_CURRENT_SOURCE_DIR}/swift/bridging.modulemap"
    DESTINATION "include/swift"
    COMPONENT compiler)
swift_install_in_component(FILES
    "${CMAKE_CURRENT_SOURCE_DIR}/module.modulemap"
    DESTINATION "include"
    COMPONENT compiler)

add_dependencies(swiftClangImporter
    "copy_cxxInterop_support_header")