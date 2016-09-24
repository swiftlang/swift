if (WIN32 AND NOT CYGWIN)
  # llvm-lit needs suffix.py for multiprocess to find a main module.
  set(suffix .py)
endif ()
set(llvm_lit_path ${LLVM_RUNTIME_OUTPUT_INTDIR}/llvm-lit${suffix})

if(NOT "${CMAKE_CFG_INTDIR}" STREQUAL ".")
  foreach(BUILD_MODE ${CMAKE_CONFIGURATION_TYPES})
    string(REPLACE ${CMAKE_CFG_INTDIR} ${BUILD_MODE} bi ${llvm_lit_path})
    configure_file(
      llvm-lit.in
      ${bi}
      )
  endforeach()
else()
  set(BUILD_MODE .)
  configure_file(
    llvm-lit.in
    ${llvm_lit_path}
    )
endif()
