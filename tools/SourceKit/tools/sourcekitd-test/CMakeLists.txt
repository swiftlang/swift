set(LLVM_TARGET_DEFINITIONS Options.td)
swift_tablegen(Options.inc -gen-opt-parser-defs)
swift_add_public_tablegen_target(sourcekitdTestOptionsTableGen)

# sourcekitd-test must only depend on DLL-exported APIs (sourcekitd,
# libdispatch, LLVM ...). Of particular note, bringing in swiftBasic directly or
# indirectly will cause the tests to crash at runtime with an LLVM dylib build.
# This is because swiftBasic has static cl::opt initializers in
# lib/Basic/Assertions.cpp. swiftBasic is already embedded in sourcekitdInProc,
# which this test loads, and in an LLVM dylib build, the static initializers
# will register twice against the single shared LLVM dylib cl::opt registry,
# causing a crash at startup.
add_sourcekit_executable(sourcekitd-test
  sourcekitd-test.cpp
  TestOptions.cpp
  LLVM_LINK_COMPONENTS option coverage lto
)
if(SWIFT_SOURCEKIT_USE_INPROC_LIBRARY)
  target_link_libraries(sourcekitd-test PRIVATE sourcekitdInProc)
else()
  target_link_libraries(sourcekitd-test PRIVATE sourcekitd)
endif()
if(NOT CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  target_link_libraries(sourcekitd-test PRIVATE
    dispatch
    BlocksRuntime)
endif()

add_dependencies(sourcekitd-test sourcekitdTestOptionsTableGen)

if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  set_target_properties(sourcekitd-test PROPERTIES
    INSTALL_RPATH "@executable_path/../lib")
  target_link_options(sourcekitd-test PRIVATE
    "LINKER:-exported_symbol,_main")
endif()
if(SWIFT_ANALYZE_CODE_COVERAGE)
  set_property(TARGET sourcekitd-test APPEND_STRING PROPERTY
    LINK_FLAGS " -fprofile-instr-generate -fcoverage-mapping")
endif()

add_dependencies(tools sourcekitd-test)
swift_install_in_component(TARGETS sourcekitd-test
  RUNTIME DESTINATION bin COMPONENT tools)
