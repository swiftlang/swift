# Mark - copy "bridging" (support header) into the local include directory and
# install it into the compiler toolchain.

add_custom_command(
    OUTPUT "${SWIFT_INCLUDE_DIR}/module.modulemap"
    DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/module.modulemap"
    COMMAND "${CMAKE_COMMAND}" "-E" "copy" "${CMAKE_CURRENT_SOURCE_DIR}/module.modulemap" "${SWIFT_INCLUDE_DIR}")
add_custom_command(
    OUTPUT "${SWIFT_INCLUDE_DIR}/swift/bridging"
    DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/swift/bridging"
    COMMAND "${CMAKE_COMMAND}" "-E" "copy" "${CMAKE_CURRENT_SOURCE_DIR}/swift/bridging" "${SWIFT_INCLUDE_DIR}/swift")
add_custom_command(
    OUTPUT "${SWIFT_INCLUDE_DIR}/swift/bridging.modulemap"
    DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/swift/bridging.modulemap"
    COMMAND "${CMAKE_COMMAND}" "-E" "copy" "${CMAKE_CURRENT_SOURCE_DIR}/swift/bridging.modulemap" "${SWIFT_INCLUDE_DIR}/swift")

add_custom_target("copy_cxxInterop_support_header"
    DEPENDS "${SWIFT_INCLUDE_DIR}/module.modulemap"
    DEPENDS "${SWIFT_INCLUDE_DIR}/swift/bridging"
    DEPENDS "${SWIFT_INCLUDE_DIR}/swift/bridging.modulemap"
    COMMENT "Copying C++ interop support header to ${SWIFT_INCLUDE_DIR}/swift")

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