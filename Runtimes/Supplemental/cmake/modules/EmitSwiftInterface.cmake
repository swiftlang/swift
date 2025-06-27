# Generate and install swift interface files

# TODO: CMake should learn how to model library evolution and generate this
#       stuff automatically.


# Generate a swift interface file for the target if library evolution is enabled
function(emit_swift_interface target)
  # Generate the target-variant binary swift module when performing zippered
  # build
  # Clean this up once CMake has nested swiftmodules in the build directory:
  # https://gitlab.kitware.com/cmake/cmake/-/merge_requests/10664
  # https://cmake.org/cmake/help/git-stage/policy/CMP0195.html

  # We can't expand the Swift_MODULE_NAME target property in a generator
  # expression or it will fail saying that the target doesn't exist.
  get_target_property(module_name ${target} Swift_MODULE_NAME)
  if(NOT module_name)
    set(module_name ${target})
  endif()
  file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/${module_name}.swiftmodule")
  target_compile_options(${target} PRIVATE
    "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-emit-module-path ${CMAKE_CURRENT_BINARY_DIR}/${module_name}.swiftmodule/${${PROJECT_NAME}_MODULE_TRIPLE}.swiftmodule>")
  if(${PROJECT_NAME}_VARIANT_MODULE_TRIPLE)
    target_compile_options(${target} PRIVATE
      "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-emit-variant-module-path ${CMAKE_CURRENT_BINARY_DIR}/${module_name}.swiftmodule/${${PROJECT_NAME}_VARIANT_MODULE_TRIPLE}.swiftmodule>")
  endif()
  add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${module_name}.swiftmodule/${${PROJECT_NAME}_MODULE_TRIPLE}.swiftmodule"
    DEPENDS ${target})
  target_sources(${target}
    INTERFACE
      $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/${module_name}.swiftmodule/${${PROJECT_NAME}_MODULE_TRIPLE}.swiftmodule>)

  # Generate textual swift interfaces is library-evolution is enabled
  if(${PROJECT_NAME}_ENABLE_LIBRARY_EVOLUTION)
    target_compile_options(${target} PRIVATE
      $<$<COMPILE_LANGUAGE:Swift>:-emit-module-interface-path$<SEMICOLON>${CMAKE_CURRENT_BINARY_DIR}/${module_name}.swiftmodule/${${PROJECT_NAME}_MODULE_TRIPLE}.swiftinterface>
      $<$<COMPILE_LANGUAGE:Swift>:-emit-private-module-interface-path$<SEMICOLON>${CMAKE_CURRENT_BINARY_DIR}/${module_name}.swiftmodule/${${PROJECT_NAME}_MODULE_TRIPLE}.private.swiftinterface>)
    if(${PROJECT_NAME}_VARIANT_MODULE_TRIPLE)
      target_compile_options(${target} PRIVATE
        "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-emit-variant-module-interface-path ${CMAKE_CURRENT_BINARY_DIR}/${module_name}.swiftmodule/${${PROJECT_NAME}_VARIANT_MODULE_TRIPLE}.swiftinterface>"
        "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-emit-variant-private-module-interface-path ${CMAKE_CURRENT_BINARY_DIR}/${module_name}.swiftmodule/${${PROJECT_NAME}_VARIANT_MODULE_TRIPLE}.private.swiftinterface>")
    endif()
    target_compile_options(${target} PRIVATE
      $<$<COMPILE_LANGUAGE:Swift>:-library-level$<SEMICOLON>api>
      $<$<COMPILE_LANGUAGE:Swift>:-Xfrontend$<SEMICOLON>-require-explicit-availability=ignore>)
  endif()
endfunction()
