
include(CMakeParseArguments)
include(LLVMExternalProjectUtils)
include(SwiftUtils)

# This is the name of the target on the parent cmake side that is associated
# with an external project target.
#
# If LLVMExternalProjectUtils refactors its external target code, so we can
# easily add individual forwarded targets with different dependencies, this can
# be removed.
function(compute_external_target_name target_name_out target)
  string(REPLACE ":" ";" target_list ${target})
  list(GET target_list 0 target)
  list(LENGTH target_list target_list_len)
  if(${target_list_len} GREATER 1)
    list(GET target_list 1 target_name)
  else()
    set(target_name "${target}")
  endif()

  set(${target_name_out} "${target_name}" PARENT_SCOPE)
endfunction()

function(compute_stdlib_dependencies stdlib_dependencies_out platform)
  foreach(stdlib_dependency ${UNIVERSAL_LIBRARY_NAMES_${platform}})
    string(FIND "${stdlib_dependency}" "Unittest" find_output)
    if("${find_output}" STREQUAL "-1")
      list(APPEND stdlib_dependencies "${stdlib_dependency}")
    endif()
  endforeach()
  set(${stdlib_dependencies_out} "${stdlib_dependencies}" PARENT_SCOPE)
endfunction()

set(ONLY_PLATFORMS "macosx" "iphoneos" "appletvos" "watchos")

function (get_platform_from_target target_platform_out target)
  foreach (platform ${ONLY_PLATFORMS})
    string(FIND "${target}" "${platform}" FOUND_TARGET_PLATFORM)
    if (NOT FOUND_TARGET_PLATFORM)
      continue()
    endif()

    set(${target_platform_out} "${platform}" PARENT_SCOPE)
    break()
  endforeach()
endfunction()

function (compute_target_stdlib_dependencies dependencies_out target)
  get_platform_from_target(target_platform ${target})
  precondition(target_platform
    MESSAGE "Failed to find a platform for ${target_platform}")
  compute_stdlib_dependencies(stdlib_dependencies ${target_platform})
  set(${dependencies_out} ${stdlib_dependencies} PARENT_SCOPE)
endfunction()

function (add_external_benchmark_suite)
  set(name swift-benchmark)
  set(src_dir ${SWIFT_SOURCE_DIR}/benchmark)
  set(bin_dir ${SWIFT_BINARY_DIR}/external-benchmark/binary)
  set(stamp_dir ${SWIFT_BINARY_DIR}/external-benchmark/stamps)
  set(prefix_dir ${SWIFT_BINARY_DIR}/external-benchmark/prefix)

  set(bench_targets Benchmark_O Benchmark_Onone Benchmark_Osize)
  set(library_targets swift-benchmark-macosx-x86_64-external)

  set(all_stdlib_dependencies)
  foreach (target ${library_targets})
    compute_target_stdlib_dependencies(stdlib_dependencies ${target})
    precondition(stdlib_dependencies)
    # Add dependencies from all of our stdlib dependencies to
    # swift-bench-configure. This will ensure the stdlib is ready to be poked at
    # in the configure script if we ever want to do so.
    list(APPEND all_stdlib_dependencies ${stdlib_dependencies})
  endforeach()

  llvm_ExternalProject_add(swift-bench ${src_dir}
    SOURCE_DIR ${src_dir}
    EXCLUDE_FROM_ALL
    DEPENDS swift ${all_stdlib_dependencies}
    EXTRA_TARGETS ${bench_targets} ${library_targets}
    CMAKE_ARGS
      -DSWIFT_EXEC=${SWIFT_BINARY_DIR}/bin/swiftc
      -DSWIFT_LIBRARY_PATH=${SWIFT_BINARY_DIR}/lib/swift
      -DCMAKE_C_COMPILER=${PATH_TO_CLANG_BUILD}/bin/clang
      -DCMAKE_CXX_COMPILER=${PATH_TO_CLANG_BUILD}/bin/clang++
      -DCLANG_EXEC=${PATH_TO_CLANG_BUILD}/bin/clang
      -DSWIFT_BENCHMARK_SUBCMAKE_BUILD=TRUE
    PASSTHROUGH_PREFIXES SWIFT_BENCHMARK
    )
endfunction()
