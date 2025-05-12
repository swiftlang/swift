# Generate and install swift interface files

# TODO: CMake should learn how to model library evolution and generate this
#       stuff automatically.


# Generate a swift interface file for the target if library evolution is enabled
function(emit_swift_interface target)
  # Generate the target-variant binary swift module when performing zippered
  # build
  if(SwiftCore_VARIANT_MODULE_TRIPLE)
    # Clean this up once CMake has nested swiftmodules in the build directory:
    # https://gitlab.kitware.com/cmake/cmake/-/merge_requests/10664

    # We can't expand the Swift_MODULE_NAME target property in a generator
    # expression or it will fail saying that the target doesn't exist.
    get_target_property(module_name ${target} Swift_MODULE_NAME)
    if(NOT module_name)
      set(module_name ${target})
    endif()
    target_compile_options(${target} PRIVATE
      "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-emit-module-path ${CMAKE_CURRENT_BINARY_DIR}/${module_name}.swiftmodule/${SwiftCore_MODULE_TRIPLE}.swiftmodule>"
      "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-emit-variant-module-path ${CMAKE_CURRENT_BINARY_DIR}/${module_name}.swiftmodule/${SwiftCore_VARIANT_MODULE_TRIPLE}.swiftmodule>")
    add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${module_name}.swiftmodule/${SwiftCore_MODULE_TRIPLE}.swiftmodule"
      DEPENDS ${target})
    target_sources(${target}
      INTERFACE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/${module_name}.swiftmodule/${SwiftCore_MODULE_TRIPLE}.swiftmodule>)
  endif()

  # Generate textual swift interfaces is library-evolution is enabled
  if(SwiftCore_ENABLE_LIBRARY_EVOLUTION)
    # Emit catalyst swiftmodules and interfaces
    if(SwiftCore_VARIANT_MODULE_TRIPLE)
      target_compile_options(${target} PRIVATE
        $<$<COMPILE_LANGUAGE:Swift>:-emit-module-interface-path$<SEMICOLON>${CMAKE_CURRENT_BINARY_DIR}/${module_name}.swiftmodule/${SwiftCore_MODULE_TRIPLE}.swiftinterface>
        $<$<COMPILE_LANGUAGE:Swift>:-emit-private-module-interface-path$<SEMICOLON>${CMAKE_CURRENT_BINARY_DIR}/${module_name}.swiftmodule/${SwiftCore_MODULE_TRIPLE}.private.swiftinterface>
        "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-emit-variant-module-interface-path ${CMAKE_CURRENT_BINARY_DIR}/${module_name}.swiftmodule/${SwiftCore_VARIANT_MODULE_TRIPLE}.swiftinterface>"
        "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-emit-variant-private-module-interface-path ${CMAKE_CURRENT_BINARY_DIR}/${module_name}.swiftmodule/${SwiftCore_VARIANT_MODULE_TRIPLE}.private.swiftinterface>")
    else()
      target_compile_options(${target} PRIVATE
        $<$<COMPILE_LANGUAGE:Swift>:-emit-module-interface-path$<SEMICOLON>${CMAKE_CURRENT_BINARY_DIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftinterface>
        $<$<COMPILE_LANGUAGE:Swift>:-emit-private-module-interface-path$<SEMICOLON>${CMAKE_CURRENT_BINARY_DIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.private.swiftinterface>)
    endif()
    target_compile_options(${target} PRIVATE
      $<$<COMPILE_LANGUAGE:Swift>:-library-level$<SEMICOLON>api>
      $<$<COMPILE_LANGUAGE:Swift>:-Xfrontend$<SEMICOLON>-require-explicit-availability=ignore>)
  endif()
endfunction()

# Install the generated swift interface file for the target if library evolution
# is enabled.
function(install_swift_interface target)
  if(SwiftCore_VARIANT_MODULE_TRIPLE)
    # Swiftmodules are already in the directory structure if we are doing a
    # catalyst build. Just copy the entire directory.
    get_target_property(module_name ${target} Swift_MODULE_NAME)
    if(NOT module_name)
      set(module_name ${target})
    endif()

    install(DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/${module_name}.swiftmodule"
      DESTINATION "${SwiftCore_INSTALL_SWIFTMODULEDIR}"
      COMPONENT SwiftCore_development)
  else()
    # Install binary swift modules
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule"
      RENAME "${SwiftCore_MODULE_TRIPLE}.swiftmodule"
      DESTINATION "${SwiftCore_INSTALL_SWIFTMODULEDIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule"
      COMPONENT SwiftCore_development)

    # Install Swift interfaces if library-evolution is enabled
    if(SwiftCore_ENABLE_LIBRARY_EVOLUTION)
      install(FILES "${CMAKE_CURRENT_BINARY_DIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftinterface"
        RENAME "${SwiftCore_MODULE_TRIPLE}.swiftinterface"
        DESTINATION "${SwiftCore_INSTALL_SWIFTMODULEDIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule"
        COMPONENT SwiftCore_development)
      install(FILES "${CMAKE_CURRENT_BINARY_DIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.private.swiftinterface"
        RENAME "${SwiftCore_MODULE_TRIPLE}.private.swiftinterface"
        DESTINATION "${SwiftCore_INSTALL_SWIFTMODULEDIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule"
        COMPONENT SwiftCore_development)
    endif()
  endif()
endfunction()
