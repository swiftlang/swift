# Generate and install swift interface files

# TODO: CMake should learn how to model library evolution and generate this
#       stuff automatically.


# Generate a swift interface file for the target if library evolution is enabled
function(emit_swift_interface target)
  if(SwiftCore_ENABLE_LIBRARY_EVOLUTION)
    target_compile_options(${target} PRIVATE
      $<$<COMPILE_LANGUAGE:Swift>:-emit-module-interface-path$<SEMICOLON>${CMAKE_CURRENT_BINARY_DIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftinterface>
      $<$<COMPILE_LANGUAGE:Swift>:-emit-private-module-interface-path$<SEMICOLON>${CMAKE_CURRENT_BINARY_DIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.private.swiftinterface>
      $<$<COMPILE_LANGUAGE:Swift>:-library-level$<SEMICOLON>api>
      $<$<COMPILE_LANGUAGE:Swift>:-Xfrontend$<SEMICOLON>-require-explicit-availability=ignore>)
  endif()
endfunction()

# Install the generated swift interface file for the target if library evolution
# is enabled.
function(install_swift_interface target)
  if(SwiftCore_ENABLE_LIBRARY_EVOLUTION)
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftinterface"
      RENAME "${SwiftCore_MODULE_TRIPLE}.swiftinterface"
      DESTINATION "${CMAKE_INSTALL_LIBDIR}/swift/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule")

    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.private.swiftinterface"
      RENAME "${SwiftCore_MODULE_TRIPLE}.private.swiftinterface"
      DESTINATION "${CMAKE_INSTALL_LIBDIR}/swift/$<TARGET_PROPERTY:${target},Swift_MODULE_NAME>.swiftmodule")
  endif()
endfunction()
