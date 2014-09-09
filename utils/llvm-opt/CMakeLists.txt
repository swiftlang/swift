set(OPT ${PATH_TO_LLVM_BUILD}/bin/opt)

if(CMAKE_CONFIGURATION_TYPES)
  foreach(config ${CMAKE_CONFIGURATION_TYPES})
    # Xcode support. See the CMakeLists.txt for the SwiftLLVMPasses library.

    string(TOUPPER ${config} config_upper)
    get_property(SWIFT_LLVM_DYLIB
      TARGET SwiftLLVMPasses
      PROPERTY "LOCATION_${config_upper}")

    # Note: this won't work if the EFFECTIVE_PLATFORM_NAME isn't empty.
    # Currently it always is on OS X.
    string(REPLACE "$(EFFECTIVE_PLATFORM_NAME)" "" SWIFT_LLVM_DYLIB
      "${SWIFT_LLVM_DYLIB}")

    configure_file(
      ${CMAKE_CURRENT_SOURCE_DIR}/llvm-opt.in
      ${CMAKE_BINARY_DIR}/${config}/bin/llvm-opt
      @ONLY)
  endforeach()
else()
  get_property(SWIFT_LLVM_DYLIB
    TARGET SwiftLLVMPasses
    PROPERTY LOCATION)
  configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/llvm-opt.in
    ${CMAKE_BINARY_DIR}/${config}/bin/llvm-opt
    @ONLY)
endif()
