#
# API Notes for the C++ Standard Library
#
set(output_dir "${SWIFTLIB_DIR}/apinotes")
add_custom_command_target(unused_var
    CUSTOM_TARGET_NAME CxxStdlib-apinotes
    COMMAND ${CMAKE_COMMAND} "-E" "make_directory" "${output_dir}"
    COMMAND ${CMAKE_COMMAND} "-E" "copy_if_different" "${CMAKE_CURRENT_SOURCE_DIR}/std.apinotes" "${output_dir}"
    COMMENT "Copying CxxStdlib API Notes to ${output_dir}"
    OUTPUT "${output_dir}/std.apinotes"
    DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/std.apinotes")

swift_install_in_component(FILES std.apinotes
    DESTINATION "lib/swift/apinotes"
    COMPONENT compiler)

set_property(TARGET CxxStdlib-apinotes PROPERTY FOLDER "Miscellaneous")
add_dependencies(sdk-overlay CxxStdlib-apinotes)
add_dependencies(compiler CxxStdlib-apinotes)


#
# C++ Standard Library Overlay.
#
# The overlay is fragile (i.e. it does not use library evolution)
# as it's not ABI stable.
add_swift_target_library(swiftCxxStdlib STATIC NO_LINK_NAME IS_STDLIB IS_SWIFT_ONLY IS_FRAGILE
    std.swift
    Chrono.swift
    String.swift

    SWIFT_MODULE_DEPENDS Cxx
    SWIFT_MODULE_DEPENDS_ANDROID Android

    SWIFT_COMPILE_FLAGS ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS} ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    -cxx-interoperability-mode=default
    -Xfrontend -module-interface-preserve-types-as-written

    SWIFT_COMPILE_FLAGS_LINUX
    ${SWIFT_SDK_LINUX_CXX_OVERLAY_SWIFT_COMPILE_FLAGS}

    DEPLOYMENT_VERSION_OSX ${COMPATIBILITY_MINIMUM_DEPLOYMENT_VERSION_OSX}

    LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
    TARGET_SDKS ALL_APPLE_PLATFORMS LINUX WINDOWS ANDROID
    MACCATALYST_BUILD_FLAVOR zippered
    INSTALL_IN_COMPONENT compiler
    INSTALL_WITH_SHARED
    DEPENDS libstdcxx-modulemap libcxxshim_modulemap CxxStdlib-apinotes)
