
set(SILOPTIMIZER_SOURCES)

function(_list_transform newvar)
  set(sources ${ARGN})
  set(dir ${CMAKE_CURRENT_SOURCE_DIR})
  set(tmp)
  foreach (s ${sources})
    list(APPEND tmp "${dir}/${s}")
  endforeach()
  set(${newvar} "${tmp}" PARENT_SCOPE)
endfunction()

macro(silopt_register_sources)
  precondition(new_transformed_sources
    NEGATE
    MESSAGE "Expected this to be empty since we clear after each run")
  _list_transform(new_transformed_sources ${ARGN})
  list_union("${SILOPTIMIZER_SOURCES}" "${new_transformed_sources}" out)
  set(SILOPTIMIZER_SOURCES "${out}" PARENT_SCOPE)
  set(new_transformed_sources)
endmacro()

add_subdirectory(ARC)
add_subdirectory(Analysis)
add_subdirectory(FunctionSignatureTransforms)
add_subdirectory(IPO)
add_subdirectory(LoopTransforms)
add_subdirectory(Mandatory)
add_subdirectory(PassManager)
add_subdirectory(SILCombiner)
add_subdirectory(Transforms)
add_subdirectory(UtilityPasses)
add_subdirectory(Utils)

add_swift_host_library(swiftSILOptimizer STATIC
  ${SILOPTIMIZER_SOURCES}
  LINK_LIBRARIES swiftSIL)
