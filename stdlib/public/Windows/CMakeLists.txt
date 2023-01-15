set(winsdk_modulemap_target_list)
foreach(arch ${SWIFT_SDK_WINDOWS_ARCHITECTURES})
set(arch_suffix "${SWIFT_SDK_WINDOWS_LIB_SUBDIR}-${arch}")
set(arch_subdir "${SWIFT_SDK_WINDOWS_LIB_SUBDIR}/${arch}")

set(module_dir "${SWIFTLIB_DIR}/${arch_subdir}")
set(module_dir_static "${SWIFTSTATICLIB_DIR}/${arch_subdir}")

set(winsdk_modulemap "winsdk.modulemap")
set(winsdk_modulemap_out "${module_dir}/WinSDK.modulemap")
set(winsdk_modulemap_out_static "${module_dir_static}/WinSDK.modulemap")

add_custom_command_target(
    copy_winsdk_modulemap
    COMMAND
    "${CMAKE_COMMAND}" "-E" "make_directory" ${module_dir}
    COMMAND
    "${CMAKE_COMMAND}" "-E" "copy_if_different" "${CMAKE_CURRENT_SOURCE_DIR}/${winsdk_modulemap}" "${winsdk_modulemap_out}"
    OUTPUT ${winsdk_modulemap_out}
    DEPENDS ${winsdk_modulemap}
    COMMENT "Copying WinSDK modulemap to resources")
list(APPEND winsdk_modulemap_target_list ${copy_winsdk_modulemap})
add_dependencies(swift-stdlib-${arch_suffix} ${copy_winsdk_modulemap})

if(SWIFT_BUILD_STATIC_STDLIB)
    add_custom_command_target(
        copy_winsdk_modulemap_static
        COMMAND
        "${CMAKE_COMMAND}" "-E" "make_directory" ${module_dir_static}
        COMMAND
        "${CMAKE_COMMAND}" "-E" "copy_if_different"
        "${winsdk_modulemap_out}" "${winsdk_modulemap_out_static}"
        OUTPUT ${winsdk_modulemap_out_static}
        DEPENDS ${copy_winsdk_modulemap}
        COMMENT "Copying WinSDK modulemap to static resources")
    list(APPEND winsdk_modulemap_target_list ${copy_winsdk_modulemap_static})
    add_dependencies(swift-stdlib-${arch_suffix} ${copy_winsdk_modulemap_static})
endif()

swift_install_in_component(FILES "${winsdk_modulemap_out}"
    DESTINATION "lib/swift/${arch_subdir}"
    COMPONENT sdk-overlay)

if(SWIFT_BUILD_STATIC_STDLIB)
    swift_install_in_component(FILES "${winsdk_modulemap_out_static}"
        DESTINATION "lib/swift_static/${arch_subdir}"
        COMPONENT sdk-overlay)
endif()

if(${BOOTSTRAPPING_MODE} MATCHES "BOOTSTRAPPING.*")
    foreach(bootstrapping "0" "1")
    get_bootstrapping_path(bootstrapping_dir ${module_dir} ${bootstrapping})
    set(winsdk_modulemap_out_bootstrapping "${bootstrapping_dir}/WinSDK.modulemap")

    add_custom_command_target(unused_var
        COMMAND
        "${CMAKE_COMMAND}" "-E" "make_directory" "${bootstrapping_dir}"
        COMMAND
        "${CMAKE_COMMAND}" "-E" "copy_if_different"
        "${CMAKE_CURRENT_SOURCE_DIR}/${winsdk_modulemap}" "${winsdk_modulemap_out_bootstrapping}"

        CUSTOM_TARGET_NAME "copy-winsdk-modulemap-bootstrapping${bootstrapping}"
        OUTPUT "${winsdk_modulemap_out_bootstrapping}"
        DEPENDS ${winsdk_modulemap}
        COMMENT "Copying WinSDK modulemap to resources for bootstrapping${bootstrapping}")
    endforeach()
endif()
endforeach()
add_custom_target(winsdk-modulemap DEPENDS ${winsdk_modulemap_target_list})
set_property(TARGET winsdk-modulemap PROPERTY FOLDER "Miscellaneous")
add_dependencies(sdk-overlay winsdk-modulemap)

add_swift_target_library(swiftWinSDK ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
    WinSDK.swift

    SWIFT_COMPILE_FLAGS ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS} ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
    TARGET_SDKS WINDOWS
    INSTALL_IN_COMPONENT sdk-overlay
    DEPENDS winsdk-modulemap)
