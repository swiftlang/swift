
add_llvm_loadable_module(SwiftARCPasses
  SwiftARCPasses.cpp
  LINK_LIBS swiftOptimizeARC
)

set_target_properties(SwiftARCPasses PROPERTIES
  LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib")

foreach(config ${CMAKE_CONFIGURATION_TYPES})
  string(TOUPPER ${config} config_upper)
  # Hack to deal with the fact that ${SWIFTLIB_DIR} contains the build-time
  # variable $(CONFIGURATION). Note that this fix is Xcode-specific.
  # You might think you could just replace ${CMAKE_CFG_INTDIR} here, but
  # that can actually have other things mangled into it besides just the
  # configuration. This at least doesn't pretend to do the right thing
  # outside of Xcode.
  string(REPLACE "$(CONFIGURATION)" ${config} config_lib_dir
        "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib")
  set_target_properties(SwiftARCPasses PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY_${config_upper} ${config_lib_dir}
    ARCHIVE_OUTPUT_DIRECTORY_${config_upper} ${config_lib_dir})
endforeach()
