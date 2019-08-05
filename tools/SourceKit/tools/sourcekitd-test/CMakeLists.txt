set(LLVM_TARGET_DEFINITIONS Options.td)
swift_tablegen(Options.inc -gen-opt-parser-defs)
swift_add_public_tablegen_target(sourcekitdTestOptionsTableGen)

if(SWIFT_SOURCEKIT_USE_INPROC_LIBRARY)
  set(SOURCEKITD_TEST_LINK_LIBS sourcekitdInProc)
  set(SOURCEKITD_TEST_CFLAGS "-DSWIFT_SOURCEKIT_USE_INPROC_LIBRARY")
else()
  set(SOURCEKITD_TEST_LINK_LIBS sourcekitd)
  set(SOURCEKITD_TEST_CFLAGS)
endif()

add_sourcekit_executable(sourcekitd-test
  sourcekitd-test.cpp
  TestOptions.cpp
  C_COMPILE_FLAGS ${SOURCEKITD_TEST_CFLAGS}
  LINK_LIBS ${SOURCEKITD_TEST_LINK_LIBS} SourceKitSupport
    clangRewrite clangLex clangBasic
  LLVM_LINK_COMPONENTS core support option coverage lto
)
if(SWIFT_NEED_EXPLICIT_LIBDISPATCH)
  target_link_libraries(sourcekitd-test PRIVATE dispatch BlocksRuntime)
endif()

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

swift_install_in_component(TARGETS sourcekitd-test
                           RUNTIME
                             DESTINATION bin
                             COMPONENT tools)
