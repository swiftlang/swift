add_swift_host_library(swiftDemangle
                  SHARED
                    SwiftDemangle.cpp
                    MangleHack.cpp
                  LINK_LIBS
                    swiftDemangling)
target_compile_definitions(swiftDemangle
                           PRIVATE
                             LLVM_DISABLE_ABI_BREAKING_CHECKS_ENFORCING=1)

# Do not enforce checks for LLVM's ABI-breaking build settings.
# The demangler uses some header-only code from LLVM's ADT classes,
# but we do not want to link libSupport into the demangler. These checks rely
# on the presence of symbols in libSupport to identify how the code was
# built and cause link failures for mismatches. Without linking that library,
# we get link failures regardless, so instead, this just disables the checks.
append("-DLLVM_DISABLE_ABI_BREAKING_CHECKS_ENFORCING=1" CMAKE_C_FLAGS CMAKE_CXX_FLAGS)

swift_install_in_component(compiler
    TARGETS swiftDemangle
    LIBRARY DESTINATION "lib${LLVM_LIBDIR_SUFFIX}"
    ARCHIVE DESTINATION "lib${LLVM_LIBDIR_SUFFIX}")
