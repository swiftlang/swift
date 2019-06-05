if(libedit_FOUND)
  if(SWIFT_SOURCEKIT_USE_INPROC_LIBRARY)
    set(SOURCEKITD_REPL_LINK_LIBS sourcekitdInProc)
  else()
    set(SOURCEKITD_REPL_LINK_LIBS sourcekitd)
  endif()

  add_sourcekit_executable(sourcekitd-repl
    sourcekitd-repl.cpp
    LINK_LIBS ${SOURCEKITD_REPL_LINK_LIBS}
    LLVM_LINK_COMPONENTS support coverage lto
  )
  target_include_directories(sourcekitd-repl PRIVATE
    ${libedit_INCLUDE_DIRS})
  target_link_libraries(sourcekitd-repl PRIVATE
    ${libedit_LIBRARIES})
  if(SWIFT_NEED_EXPLICIT_LIBDISPATCH)
    target_link_libraries(sourcekitd-repl PRIVATE dispatch BlocksRuntime)
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

  swift_install_in_component(TARGETS sourcekitd-repl
                             RUNTIME DESTINATION bin
                             COMPONENT tools)
endif()
