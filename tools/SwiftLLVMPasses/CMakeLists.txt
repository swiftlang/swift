if(NOT SWIFT_INSTALL_TESTSUITE_TOOLS)
  # Prevent SwiftLLVMPasses from being installed.
  set(LLVM_INSTALL_TOOLCHAIN_ONLY TRUE)
endif()

add_swift_llvm_loadable_module(SwiftLLVMPasses
  SwiftLLVMPasses.cpp
  LINK_LIBS swiftLLVMPasses
)

set_target_properties(SwiftLLVMPasses PROPERTIES
  LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib")

foreach(config ${CMAKE_CONFIGURATION_TYPES})
  string(TOUPPER ${config} config_upper)
  apply_xcode_substitutions(
      "${config}"
      "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib"
      config_lib_dir)
  set_target_properties(SwiftLLVMPasses PROPERTIES
      LIBRARY_OUTPUT_DIRECTORY_${config_upper} ${config_lib_dir}
      ARCHIVE_OUTPUT_DIRECTORY_${config_upper} ${config_lib_dir})
endforeach()
