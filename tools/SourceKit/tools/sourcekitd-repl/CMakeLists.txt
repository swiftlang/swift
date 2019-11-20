set(CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} edit)
check_symbol_exists(el_wgets "histedit.h" HAVE_UNICODE_LIBEDIT)

if(HAVE_UNICODE_LIBEDIT)
  add_sourcekit_executable(sourcekitd-repl
    sourcekitd-repl.cpp
    LLVM_LINK_COMPONENTS coverage lto
  )
  target_link_libraries(sourcekitd-repl PRIVATE edit)
  if(SWIFT_SOURCEKIT_USE_INPROC_LIBRARY)
    target_link_libraries(sourcekitd-repl PRIVATE sourcekitdInProc)
  else()
    target_link_libraries(sourcekitd-repl PRIVATE sourcekitd)
  endif()
  if(NOT CMAKE_SYSTEM_NAME STREQUAL Darwin)
    target_link_libraries(sourcekitd-repl PRIVATE
      dispatch
      BlocksRuntime)
  endif()

  if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set_target_properties(sourcekitd-repl
      PROPERTIES
      LINK_FLAGS "-Wl,-rpath -Wl,@executable_path/../lib")
  endif()
  if(SWIFT_ANALYZE_CODE_COVERAGE)
    set_property(TARGET sourcekitd-repl APPEND_STRING PROPERTY
      LINK_FLAGS " -fprofile-instr-generate -fcoverage-mapping")
  endif()

  add_dependencies(tools sourcekitd-repl)
  swift_install_in_component(TARGETS sourcekitd-repl
                             RUNTIME
                               DESTINATION bin
                               COMPONENT tools)
endif()
