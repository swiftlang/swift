add_swift_host_library(swiftDemangle
                  SHARED
                    SwiftDemangle.cpp
                    MangleHack.cpp
                  LINK_LIBRARIES
                    swiftDemangling
                  C_COMPILE_FLAGS
                    -DLLVM_DISABLE_ABI_BREAKING_CHECKS_ENFORCING=1)

swift_install_in_component(compiler swift-demangle-host
    TARGETS swiftDemangle
    LIBRARY DESTINATION "lib${LLVM_LIBDIR_SUFFIX}"
    ARCHIVE DESTINATION "lib${LLVM_LIBDIR_SUFFIX}")
