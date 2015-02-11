swift_get_configuration_types(configs)
foreach(config ${configs})
  set(OPT "${LLVM_TOOLS_BINARY_DIR}/opt")
  set(SWIFT_LLVM_DYLIB
      "${CMAKE_BINARY_DIR}/${config}/lib/SwiftLLVMPasses${CMAKE_SHARED_LIBRARY_SUFFIX}")
  configure_file(
      "${CMAKE_CURRENT_SOURCE_DIR}/llvm-opt.in"
      "${CMAKE_CURRENT_BINARY_DIR}/${config}/llvm-opt"
      @ONLY
      NEWLINE_STYLE UNIX)
  file(COPY "${CMAKE_CURRENT_BINARY_DIR}/${config}/llvm-opt"
      DESTINATION "${CMAKE_BINARY_DIR}/${config}/bin"
      FILE_PERMISSIONS
        OWNER_READ OWNER_WRITE OWNER_EXECUTE
        GROUP_READ GROUP_EXECUTE
        WORLD_READ WORLD_EXECUTE)
endforeach()

swift_install_in_component(testsuite-tools
    PROGRAMS "${SWIFT_RUNTIME_OUTPUT_INTDIR}/llvm-opt"
    DESTINATION "bin")

