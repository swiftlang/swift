
# clang-cl interprets paths starting with /U as macro undefines, so we need to
# put a -- before the input file path to force it to be treated as a path.
if(NOT MSVC AND "${CMAKE_SIMULATE_ID}" STREQUAL "MSVC")
  string(REPLACE "-c <SOURCE>" "-c -- <SOURCE>" CMAKE_C_COMPILE_OBJECT "${CMAKE_C_COMPILE_OBJECT}")
  string(REPLACE "-c <SOURCE>" "-c -- <SOURCE>" CMAKE_CXX_COMPILE_OBJECT "${CMAKE_CXX_COMPILE_OBJECT}")
endif()

# NOTE(compnerd) incremental linking is known to cause corruption in the
# protocol conformance tables.  Avoid using incremental links with Visual
# Studio.
foreach(TYPE EXE SHARED MODULE)
  foreach(BUILD DEBUG RELWITHDEBINFO)
    string(REPLACE "/INCREMENTAL:YES" "" CMAKE_${TYPE}_LINKER_FLAGS_${BUILD} "${CMAKE_${TYPE}_LINKER_FLAGS_${BUILD}}")
    string(REPLACE "/INCREMENTAL" "" CMAKE_${TYPE}_LINKER_FLAGS_${BUILD} "${CMAKE_${TYPE}_LINKER_FLAGS_${BUILD}}")
    set(CMAKE_${TYPE}_LINKER_FLAGS_${BUILD} ${CMAKE_${TYPE}_LINKER_FLAGS_${BUILD}} CACHE STRING "Flags used by the linker during builds." FORCE)
  endforeach()
endforeach()

