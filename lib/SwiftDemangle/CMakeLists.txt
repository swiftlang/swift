add_swift_host_library(swiftDemangle
                  SHARED
                    SwiftDemangle.cpp
                    MangleHack.cpp
                  C_COMPILE_FLAGS
                    -DLLVM_DISABLE_ABI_BREAKING_CHECKS_ENFORCING=1)
target_link_libraries(swiftDemangle PRIVATE
  swiftDemangling)

swift_install_in_component(compiler
    TARGETS swiftDemangle
    LIBRARY DESTINATION "lib${LLVM_LIBDIR_SUFFIX}"
    ARCHIVE DESTINATION "lib${LLVM_LIBDIR_SUFFIX}")
