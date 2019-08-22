if(SWIFT_SOURCEKIT_USE_INPROC_LIBRARY)
  set(SOURCEKITD_TEST_LINK_LIBS sourcekitdInProc)
else()
  set(SOURCEKITD_TEST_LINK_LIBS sourcekitd)
endif()

add_sourcekit_executable(complete-test
  complete-test.cpp
  LINK_LIBS ${SOURCEKITD_TEST_LINK_LIBS}
  LLVM_LINK_COMPONENTS support option coverage lto
)
if(SWIFT_NEED_EXPLICIT_LIBDISPATCH)
  target_link_libraries(complete-test PRIVATE dispatch BlocksRuntime)
endif()

if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  set_target_properties(complete-test
    PROPERTIES
    LINK_FLAGS "-Wl,-rpath -Wl,@executable_path/../lib")
endif()

if(SWIFT_ANALYZE_CODE_COVERAGE)
  set_property(TARGET complete-test APPEND_STRING PROPERTY
    LINK_FLAGS " -fprofile-instr-generate -fcoverage-mapping")
endif()

add_dependencies(tools complete-test)
swift_install_in_component(TARGETS complete-test
                           RUNTIME
                             DESTINATION bin
                             COMPONENT tools)
