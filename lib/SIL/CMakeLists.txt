
set(SIL_SOURCES)

function(_list_transform newvar)
  set(sources ${ARGN})
  set(dir ${CMAKE_CURRENT_SOURCE_DIR})
  set(tmp)
  foreach (s ${sources})
    list(APPEND tmp "${dir}/${s}")
  endforeach()
  set(${newvar} "${tmp}" PARENT_SCOPE)
endfunction()

macro(sil_register_sources)
  precondition(new_transformed_sources
    NEGATE
    MESSAGE "Expected this to be empty since we clear after each run")
  _list_transform(new_transformed_sources ${ARGN})
  list_union("${SIL_SOURCES}" "${new_transformed_sources}" out)
  set(SIL_SOURCES "${out}" PARENT_SCOPE)
  set(new_transformed_sources)
endmacro()

add_subdirectory(IR)
add_subdirectory(Utils)
add_subdirectory(Verifier)

add_swift_host_library(swiftSIL STATIC
  ${SIL_SOURCES})
target_link_libraries(swiftSIL PRIVATE
  swiftSema
  swiftSerialization)

# intrinsics_gen is the LLVM tablegen target that generates the include files
# where intrinsics and attributes are declared. swiftSIL depends on these
# headers.
# For more information see the comment at the top of lib/CMakeLists.txt.
add_dependencies(swiftSIL intrinsics_gen clang-tablegen-targets)
