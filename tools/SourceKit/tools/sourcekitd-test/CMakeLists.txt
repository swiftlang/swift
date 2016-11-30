set(LLVM_TARGET_DEFINITIONS Options.td)
swift_tablegen(Options.inc -gen-opt-parser-defs)
swift_add_public_tablegen_target(sourcekitdTestOptionsTableGen)

if(SWIFT_SOURCEKIT_USE_INPROC_LIBRARY)
  set(SOURCEKITD_TEST_DEPEND sourcekitdInProc)
else()
  set(SOURCEKITD_TEST_DEPEND sourcekitd)
endif()

if(SOURCEKIT_NEED_EXPLICIT_LIBDISPATCH)
  set(SOURCEKITD_TEST_DEPEND ${SOURCEKITD_TEST_DEPEND} dispatch swiftCore)
endif()

add_sourcekit_executable(sourcekitd-test
  sourcekitd-test.cpp
  TestOptions.cpp
  DEPENDS ${SOURCEKITD_TEST_DEPEND} SourceKitSupport
    clangRewrite clangLex clangBasic
  LLVM_COMPONENT_DEPENDS core support option coverage lto
)

add_dependencies(sourcekitd-test sourcekitdTestOptionsTableGen)

if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  set_target_properties(sourcekitd-test
    PROPERTIES
    LINK_FLAGS "-Wl,-rpath -Wl,@executable_path/../lib")
endif()

if(SWIFT_ANALYZE_CODE_COVERAGE)
  set_property(TARGET sourcekitd-test APPEND_STRING PROPERTY
    LINK_FLAGS " -fprofile-instr-generate -fcoverage-mapping")
endif()

swift_install_in_component(tools
    TARGETS sourcekitd-test
    RUNTIME DESTINATION bin)
