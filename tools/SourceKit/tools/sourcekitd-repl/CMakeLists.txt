add_sourcekit_executable(sourcekitd-repl
  sourcekitd-repl.cpp
  DEPENDS sourcekitd edit
  COMPONENT_DEPENDS support
)

if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  set_target_properties(sourcekitd-repl
    PROPERTIES
    LINK_FLAGS "-Wl,-rpath -Wl,@executable_path/../lib")

if(NOT SWIFT_ANALYZE_CODE_COVERAGE STREQUAL "NONE")
    set_property(TARGET sourcekitd-repl APPEND_STRING PROPERTY
        LINK_FLAGS " -fprofile-instr-generate -fcoverage-mapping")
  endif()
endif()

swift_install_in_component(tools
    TARGETS sourcekitd-repl
    RUNTIME DESTINATION bin)
