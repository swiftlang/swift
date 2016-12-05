set(CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} edit)
check_symbol_exists(el_wgets "histedit.h" HAVE_UNICODE_LIBEDIT)

if(HAVE_UNICODE_LIBEDIT)
  if(SWIFT_SOURCEKIT_USE_INPROC_LIBRARY)
    set(SOURCEKITD_REPL_DEPEND sourcekitdInProc)
  else()
    set(SOURCEKITD_REPL_DEPEND sourcekitd)
  endif()

  if(SOURCEKIT_NEED_EXPLICIT_LIBDISPATCH)
    set(SOURCEKITD_REPL_DEPEND ${SOURCEKITD_REPL_DEPEND} dispatch swiftCore)
  endif()

  add_sourcekit_executable(sourcekitd-repl
    sourcekitd-repl.cpp
    DEPENDS ${SOURCEKITD_REPL_DEPEND} edit
    LLVM_COMPONENT_DEPENDS support coverage lto
  )

  if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set_target_properties(sourcekitd-repl
      PROPERTIES
      LINK_FLAGS "-Wl,-rpath -Wl,@executable_path/../lib")
  endif()
  if(SWIFT_ANALYZE_CODE_COVERAGE)
    set_property(TARGET sourcekitd-repl APPEND_STRING PROPERTY
      LINK_FLAGS " -fprofile-instr-generate -fcoverage-mapping")
  endif()

  swift_install_in_component(tools
      TARGETS sourcekitd-repl
      RUNTIME DESTINATION bin)
endif()
