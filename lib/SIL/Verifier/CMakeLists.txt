target_sources(swiftSIL PRIVATE
  LoadBorrowImmutabilityChecker.cpp
  LinearLifetimeChecker.cpp
  MemoryLifetime.cpp
  ReborrowVerifier.cpp
  SILOwnershipVerifier.cpp
  SILVerifier.cpp)

if (NOT LLVM_ENABLE_ASSERTIONS)
  add_swift_host_library(swiftExternalSILVerifier SHARED
    SILVerifierCAPI.cpp)
  target_compile_definitions(swiftExternalSILVerifier PRIVATE
    LLVM_DISABLE_ABI_BREAKING_CHECKS_ENFORCING=1)
  target_link_libraries(swiftExternalSILVerifier PRIVATE
    swiftSIL)
  target_link_options(swiftExternalSILVerifier PRIVATE
    "SHELL:-Xlinker -exported_symbols_list -Xlinker ${CMAKE_CURRENT_SOURCE_DIR}/SILVerifierCAPI.exports")
  add_dependencies(compiler swiftExternalSILVerifier)
  swift_install_in_component(TARGETS swiftExternalSILVerifier
    RUNTIME
      DESTINATION "bin"
      COMPONENT compiler
    LIBRARY
      DESTINATION "lib${LLVM_LIBDIR_SUFFIX}"
      COMPONENT compiler
    ARCHIVE
      DESTINATION "lib${LLVM_LIBDIR_SUFFIX}"
      COMPONENT compiler)
endif()
