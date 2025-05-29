# Generate and install swift interface files

# TODO: CMake should learn how to model library evolution and generate this
#       stuff automatically.


# Generate a swift interface file for the target if library evolution is enabled
function(emit_swift_interface target)
  # Generate the target-variant binary swift module when performing zippered
  # build
  if(SwiftCore_VARIANT_MODULE_TRIPLE)
    set(variant_module_tmp_dir "${CMAKE_CURRENT_BINARY_DIR}/${target}-${SwiftCore_VARIANT_MODULE_TRIPLE}")
    file(MAKE_DIRECTORY "${variant_module_tmp_dir}")
    target_compile_options(${target} PRIVATE
      "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-emit-variant-module-path ${variant_module_tmp_dir}/${target}.swiftmodule>")
  endif()

  # Generate textual swift interfaces is library-evolution is enabled
  if(SwiftCore_ENABLE_LIBRARY_EVOLUTION)
    target_compile_options(${target} PRIVATE
      $<$<COMPILE_LANGUAGE:Swift>:-emit-module-interface-path$<SEMICOLON>${CMAKE_CURRENT_BINARY_DIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftinterface>
      $<$<COMPILE_LANGUAGE:Swift>:-emit-private-module-interface-path$<SEMICOLON>${CMAKE_CURRENT_BINARY_DIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.private.swiftinterface>
      $<$<COMPILE_LANGUAGE:Swift>:-library-level$<SEMICOLON>api>
      $<$<COMPILE_LANGUAGE:Swift>:-Xfrontend$<SEMICOLON>-require-explicit-availability=ignore>)

      # Emit catalyst swiftmodules and interfaces
      if(SwiftCore_VARIANT_MODULE_TRIPLE)
        target_compile_options(${target} PRIVATE
          "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-emit-variant-module-interface-path ${variant_module_tmp_dir}/${target}.swiftinterface>"
          "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-emit-variant-private-module-interface-path ${variant_module_tmp_dir}/${target}.private.swiftinterface>")
      endif()
  endif()
endfunction()
