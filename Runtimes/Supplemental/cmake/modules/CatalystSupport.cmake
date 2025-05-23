include(CheckCompilerFlag)

# Add flags for generating the zippered target variant in the build

# Initialize `${PROJECT_NAME}_VARIANT_MODULE_TRIPLE` if the driver is able to emit
# modules for the target variant.

if(${PROJECT_NAME}_COMPILER_VARIANT_TARGET)
  add_compile_options(
    "$<$<COMPILE_LANGUAGE:C,CXX>:SHELL:-darwin-target-variant ${${PROJECT_NAME}_COMPILER_VARIANT_TARGET}>"
    "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-target-variant ${${PROJECT_NAME}_COMPILER_VARIANT_TARGET}>"

    # TODO: Remove me once we have a driver with
    #   https://github.com/swiftlang/swift-driver/pull/1803
    "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xclang-linker -darwin-target-variant -Xclang-linker ${${PROJECT_NAME}_COMPILER_VARIANT_TARGET}>")

  add_link_options(
    "$<$<LINK_LANGUAGE:C,CXX>:SHELL:-darwin-target-variant ${${PROJECT_NAME}_COMPILER_VARIANT_TARGET}>"
    "$<$<LINK_LANGUAGE:Swift>:SHELL:-target-variant ${${PROJECT_NAME}_COMPILER_VARIANT_TARGET}>"

    # TODO: Remove me once we have a driver with
    #   https://github.com/swiftlang/swift-driver/pull/1803
    "$<$<LINK_LANGUAGE:Swift>:SHELL:-Xclang-linker -darwin-target-variant -Xclang-linker ${${PROJECT_NAME}_COMPILER_VARIANT_TARGET}>")

  # TODO: Once we are guaranteed to have a driver with the variant module path
  #       support everywhere, we should integrate this into PlatformInfo.cmake
  check_compiler_flag(Swift "-emit-variant-module-path ${CMAKE_CURRENT_BINARY_DIR}/CompilerID/variant.swiftmodule" HAVE_Swift_VARIANT_MODULE_PATH_FLAG)
  if(HAVE_Swift_VARIANT_MODULE_PATH_FLAG)
    # Get variant module triple
    set(module_triple_command "${CMAKE_Swift_COMPILER}" -print-target-info -target ${${PROJECT_NAME}_COMPILER_VARIANT_TARGET})
    execute_process(COMMAND ${module_triple_command} OUTPUT_VARIABLE target_info_json)
    message(CONFIGURE_LOG "Swift target variant info: ${target_info_json}")


    string(JSON module_triple GET "${target_info_json}" "target" "moduleTriple")
    set(${PROJECT_NAME}_VARIANT_MODULE_TRIPLE "${module_triple}" CACHE STRING "Triple used for installed swift{module,interface} files for the target variant")
    mark_as_advanced(${PROJECT_NAME}_VARIANT_MODULE_TRIPLE)
    message(CONFIGURE_LOG "Swift target variant module triple: ${module_triple}")
  endif()
endif()
