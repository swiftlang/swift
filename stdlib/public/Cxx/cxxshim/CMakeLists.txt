set(libcxxshim_modulemap_target_list)
foreach(sdk ${SWIFT_SDKS})
  foreach(arch ${SWIFT_SDK_${sdk}_ARCHITECTURES})
    set(arch_suffix "${SWIFT_SDK_${sdk}_LIB_SUBDIR}-${arch}")
    set(arch_subdir "${SWIFT_SDK_${sdk}_LIB_SUBDIR}/${arch}")

    set(module_dir "${SWIFTLIB_DIR}/${arch_subdir}")
    set(module_dir_static "${SWIFTSTATICLIB_DIR}/${arch_subdir}")

    set(libcxxshim_header "libcxxshim.h")
    set(libcxxshim_header_out "${module_dir}/libcxxshim.h")
    set(libcxxshim_header_out_static "${module_dir_static}/libcxxshim.h")
    set(libcxxshim_modulemap "libcxxshim.modulemap")
    set(libcxxshim_modulemap_out "${module_dir}/libcxxshim.modulemap")
    set(libcxxshim_modulemap_out_static "${module_dir_static}/libcxxshim.modulemap")

    add_custom_command_target(
            copy_libcxxshim_modulemap
            COMMAND
            "${CMAKE_COMMAND}" "-E" "make_directory" ${module_dir}
            COMMAND
            "${CMAKE_COMMAND}" "-E" "copy_if_different" "${CMAKE_CURRENT_SOURCE_DIR}/${libcxxshim_modulemap}" "${libcxxshim_modulemap_out}"
            OUTPUT ${libcxxshim_modulemap_out}
            DEPENDS ${libcxxshim_modulemap}
            COMMENT "Copying libcxxshim modulemap to resources")
    list(APPEND libcxxshim_modulemap_target_list ${copy_libcxxshim_modulemap})
    add_dependencies(swift-stdlib-${arch_suffix} ${copy_libcxxshim_modulemap})

    add_custom_command_target(
            copy_libcxxshim_header
            COMMAND
            "${CMAKE_COMMAND}" "-E" "make_directory" ${module_dir}
            COMMAND
            "${CMAKE_COMMAND}" "-E" "copy_if_different" "${CMAKE_CURRENT_SOURCE_DIR}/${libcxxshim_header}" "${libcxxshim_header_out}"
            OUTPUT ${libcxxshim_header_out}
            DEPENDS ${libcxxshim_header}
            COMMENT "Copying libcxxshim header to resources")
    list(APPEND libcxxshim_modulemap_target_list ${copy_libcxxshim_header})
    add_dependencies(swift-stdlib-${arch_suffix} ${copy_libcxxshim_header})

    if(SWIFT_BUILD_STATIC_STDLIB)
      add_custom_command_target(
              copy_libcxxshim_modulemap_static
              COMMAND
              "${CMAKE_COMMAND}" "-E" "make_directory" ${module_dir_static}
              COMMAND
              "${CMAKE_COMMAND}" "-E" "copy_if_different"
              "${libcxxshim_modulemap_out}" "${libcxxshim_modulemap_out_static}"
              OUTPUT ${libcxxshim_modulemap_out_static}
              DEPENDS ${copy_libcxxshim_modulemap}
              COMMENT "Copying libcxxshim modulemap to static resources")
      list(APPEND libcxxshim_modulemap_target_list ${copy_libcxxshim_modulemap_static})
      add_dependencies(swift-stdlib-${arch_suffix} ${copy_libcxxshim_modulemap_static})

      add_custom_command_target(
              copy_libcxxshim_header_static
              COMMAND
              "${CMAKE_COMMAND}" "-E" "make_directory" ${module_dir_static}
              COMMAND
              "${CMAKE_COMMAND}" "-E" "copy_if_different"
              "${libcxxshim_header_out}" "${libcxxshim_header_out_static}"
              OUTPUT ${libcxxshim_header_out_static}
              DEPENDS ${copy_libcxxshim_header}
              COMMENT "Copying libcxxshim header to static resources")
      list(APPEND libcxxshim_modulemap_target_list ${copy_libcxxshim_header_static})
      add_dependencies(swift-stdlib-${arch_suffix} ${copy_libcxxshim_header_static})
    endif()

    swift_install_in_component(FILES "${libcxxshim_modulemap_out}"
            DESTINATION "lib/swift/${arch_subdir}"
            COMPONENT sdk-overlay)
    swift_install_in_component(FILES "${libcxxshim_header_out}"
            DESTINATION "lib/swift/${arch_subdir}"
            COMPONENT sdk-overlay)

    if(SWIFT_BUILD_STATIC_STDLIB)
      swift_install_in_component(FILES "${libcxxshim_modulemap_out_static}"
              DESTINATION "lib/swift_static/${arch_subdir}"
              COMPONENT sdk-overlay)
      swift_install_in_component(FILES "${libcxxshim_header_out_static}"
              DESTINATION "lib/swift_static/${arch_subdir}"
              COMPONENT sdk-overlay)
    endif()
  endforeach()
endforeach()

add_custom_target(libcxxshim-modulemap DEPENDS ${libcxxshim_modulemap_target_list})
set_property(TARGET libcxxshim-modulemap PROPERTY FOLDER "Miscellaneous")
add_dependencies(sdk-overlay libcxxshim-modulemap)
