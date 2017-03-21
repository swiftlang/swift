
include(CMakeParseArguments)


# Run a shell command and assign output to a variable or fail with an error.
# Example usage:
#   runcmd(COMMAND "xcode-select" "-p"
#          VARIABLE xcodepath
#          ERROR "Unable to find current Xcode path")
function(runcmd)
  cmake_parse_arguments(RUNCMD "" "VARIABLE;ERROR" "COMMAND" ${ARGN})
  execute_process(
      COMMAND ${RUNCMD_COMMAND}
      OUTPUT_VARIABLE ${RUNCMD_VARIABLE}
      RESULT_VARIABLE result
      ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)
  if(NOT "${result}" MATCHES "0")
    message(FATAL_ERROR "${RUNCMD_ERROR}")
  endif()
  set(${RUNCMD_VARIABLE} ${${RUNCMD_VARIABLE}} PARENT_SCOPE)
endfunction(runcmd)

function (swift_benchmark_compile_archopts)
  cmake_parse_arguments(BENCH_COMPILE_ARCHOPTS "" "PLATFORM;ARCH;OPT" "" ${ARGN})
  set(sdk ${${BENCH_COMPILE_ARCHOPTS_PLATFORM}_sdk})
  set(ver ${${BENCH_COMPILE_ARCHOPTS_PLATFORM}_ver})
  set(triple_platform ${${BENCH_COMPILE_ARCHOPTS_PLATFORM}_triple_platform})

  set(target "${BENCH_COMPILE_ARCHOPTS_ARCH}-apple-${triple_platform}${ver}")

  set(objdir "${CMAKE_CURRENT_BINARY_DIR}/${BENCH_COMPILE_ARCHOPTS_OPT}-${target}")
  file(MAKE_DIRECTORY "${objdir}")

  string(REGEX REPLACE "_.*" "" optflag "${BENCH_COMPILE_ARCHOPTS_OPT}")
  string(REGEX REPLACE "^[^_]+" "" opt_suffix "${BENCH_COMPILE_ARCHOPTS_OPT}")

  set(benchvar "BENCHOPTS${opt_suffix}")
  if (NOT DEFINED ${benchvar})
    message(FATAL_ERROR "Invalid benchmark configuration ${BENCH_COMPILE_ARCHOPTS_OPT}")
  endif()

  set(bench_flags "${${benchvar}}")

  set(common_options
      "-c"
      "-sdk" "${sdk}"
      "-target" "${target}"
      "-F" "${sdk}/../../../Developer/Library/Frameworks"
      "-${BENCH_COMPILE_ARCHOPTS_OPT}"
      "-D" "INTERNAL_CHECKS_ENABLED"
      "-no-link-objc-runtime"
      "-I" "${srcdir}/utils/ObjectiveCTests")

  # Always optimize the driver modules.
  # Note that we compile the driver for Ounchecked also with -Ounchecked
  # (and not with -O), because of <rdar://problem/19614516>.
  string(REPLACE "Onone" "O" driver_opt "${optflag}")

  set(common_options_driver
      "-c"
      "-sdk" "${sdk}"
      "-target" "${target}"
      "-F" "${sdk}/../../../Developer/Library/Frameworks"
      "-${driver_opt}"
      "-D" "INTERNAL_CHECKS_ENABLED"
      "-no-link-objc-runtime")

  set(bench_library_objects)
  set(bench_library_sibfiles)
  foreach(module_name_path ${BENCH_DRIVER_LIBRARY_MODULES})
    get_filename_component(module_name "${module_name_path}" NAME)

    if("${module_name}" STREQUAL "DriverUtils")
      set(extra_sources "${srcdir}/utils/ArgParse.swift")
    endif()

    set(objfile "${objdir}/${module_name}.o")
    set(swiftmodule "${objdir}/${module_name}.swiftmodule")
    list(APPEND bench_library_objects "${objfile}")
    set(source "${srcdir}/${module_name_path}.swift")
    add_custom_command(
        OUTPUT "${objfile}"
        DEPENDS ${stdlib_dependencies} "${source}" ${extra_sources}
        COMMAND "${SWIFT_EXEC}"
        ${common_options_driver}
        ${BENCH_DRIVER_LIBRARY_FLAGS}
        "-force-single-frontend-invocation"
        "-parse-as-library"
        "-module-name" "${module_name}"
        "-emit-module" "-emit-module-path" "${swiftmodule}"
        "-o" "${objfile}"
        "${source}" ${extra_sources})
    if(SWIFT_BENCHMARK_EMIT_SIB)
      set(sibfile "${objdir}/${module_name}.sib")
      list(APPEND bench_library_sibfiles "${sibfile}")
      add_custom_command(
          OUTPUT "${sibfile}"
          DEPENDS
            ${stdlib_dependencies} "${srcdir}/${module_name_path}.swift"
            ${extra_sources}
          COMMAND "${SWIFT_EXEC}"
          ${common_options_driver}
          ${BENCH_DRIVER_LIBRARY_FLAGS}
          "-force-single-frontend-invocation"
          "-parse-as-library"
          "-module-name" "${module_name}"
          "-emit-sib"
          "-o" "${sibfile}"
          "${source}" ${extra_sources})
    endif()
  endforeach()

  foreach(module_name_path ${BENCH_LIBRARY_MODULES})
    get_filename_component(module_name "${module_name_path}" NAME)

    set(objfile "${objdir}/${module_name}.o")
    set(swiftmodule "${objdir}/${module_name}.swiftmodule")
    set(source "${srcdir}/${module_name_path}.swift")
    list(APPEND bench_library_objects "${objfile}")
    add_custom_command(
        OUTPUT "${objfile}"
        DEPENDS
          ${stdlib_dependencies} "${srcdir}/${module_name_path}.swift"
          ${extra_sources}
        COMMAND "${SWIFT_EXEC}"
        ${common_options}
        "-force-single-frontend-invocation"
        "-parse-as-library"
        "-module-name" "${module_name}"
        "-emit-module" "-emit-module-path" "${swiftmodule}"
        "-o" "${objfile}"
        "${source}" ${extra_sources})
    if (SWIFT_BENCHMARK_EMIT_SIB)
      set(sibfile "${objdir}/${module_name}.sib")
      list(APPEND bench_library_sibfiles "${sibfile}")
      add_custom_command(
          OUTPUT "${sibfile}"
          DEPENDS
            ${stdlib_dependencies} "${srcdir}/${module_name_path}.swift"
            ${extra_sources}
          COMMAND "${SWIFT_EXEC}"
          ${common_options}
          "-force-single-frontend-invocation"
          "-parse-as-library"
          "-module-name" "${module_name}"
          "-emit-sib"
          "-o" "${sibfile}"
          "${source}" ${extra_sources})
    endif()
  endforeach()

  set(SWIFT_BENCH_OBJFILES)
  set(SWIFT_BENCH_SIBFILES)
  foreach(module_name_path ${SWIFT_BENCH_MODULES})
    get_filename_component(module_name "${module_name_path}" NAME)

    if(module_name)
      set(extra_options "")
      # For this file we disable automatic bridging between Foundation and swift.
      if("${module_name}" STREQUAL "ObjectiveCNoBridgingStubs")
        set(extra_options "-Xfrontend"
                          "-disable-swift-bridge-attr")
      endif()
      set(objfile "${objdir}/${module_name}.o")
      set(swiftmodule "${objdir}/${module_name}.swiftmodule")
      set(source "${srcdir}/${module_name_path}.swift")
      list(APPEND SWIFT_BENCH_OBJFILES "${objfile}")

      if ("${bench_flags}" MATCHES "-whole-module.*")
        set(output_option "-o" "${objfile}")
      else()
        set(json "{\n  \"${source}\": { \"object\": \"${objfile}\" },\n}")
        file(WRITE "${objdir}/${module_name}-outputmap.json" ${json})
        set(output_option "-output-file-map"
                          "${objdir}/${module_name}-outputmap.json")
      endif()

      add_custom_command(
          OUTPUT "${objfile}"
          DEPENDS
            ${stdlib_dependencies} ${bench_library_objects}
            "${srcdir}/${module_name_path}.swift"
          COMMAND "${SWIFT_EXEC}"
          ${common_options}
          ${extra_options}
          "-parse-as-library"
          ${bench_flags}
          "-module-name" "${module_name}"
          "-emit-module-path" "${swiftmodule}"
          "-I" "${objdir}"
          ${output_option}
          "${source}")
      if (SWIFT_BENCHMARK_EMIT_SIB)
        set(sibfile "${objdir}/${module_name}.sib")
        list(APPEND SWIFT_BENCH_SIBFILES "${sibfile}")
        add_custom_command(
            OUTPUT "${sibfile}"
            DEPENDS
              ${stdlib_dependencies} ${bench_library_sibfiles}
              "${srcdir}/${module_name_path}.swift"
            COMMAND "${SWIFT_EXEC}"
            ${common_options}
            "-parse-as-library"
            ${bench_flags}
            "-module-name" "${module_name}"
            "-I" "${objdir}"
            "-emit-sib"
            "-o" "${sibfile}"
            "${source}")
      endif()
    endif()
  endforeach()

  foreach(module_name_path ${SWIFT_MULTISOURCE_BENCHES})
    get_filename_component(module_name "${module_name_path}" NAME)

    if ("${bench_flags}" MATCHES "-whole-module.*" AND
        NOT "${bench_flags}" MATCHES "-num-threads.*")
      # Regular whole-module-compilation: only a single object file is
      # generated.
      set(objfile "${objdir}/${module_name}.o")
      list(APPEND SWIFT_BENCH_OBJFILES "${objfile}")
      set(sources)
      foreach(source ${${module_name}_sources})
        list(APPEND sources "${srcdir}/${source}")
      endforeach()
      add_custom_command(
          OUTPUT "${objfile}"
          DEPENDS
            ${stdlib_dependencies} ${bench_library_objects} ${sources}
          COMMAND "${SWIFT_EXEC}"
          ${common_options}
          ${bench_flags}
          "-parse-as-library"
          "-emit-module" "-module-name" "${module_name}"
          "-I" "${objdir}"
          "-o" "${objfile}"
          ${sources})
    else()

      # No whole-module-compilation or multi-threaded compilation.
      # There is an output object file for each input file. We have to write
      # an output-map-file to specify the output object file names.
      set(sources)
      set(objfiles)
      set(json "{\n")
      foreach(source ${${module_name}_sources})
          list(APPEND sources "${srcdir}/${source}")

          get_filename_component(basename "${source}" NAME_WE)
          set(objfile "${objdir}/${module_name}/${basename}.o")

              string(CONCAT json "${json}"
    "  \"${srcdir}/${source}\": { \"object\": \"${objfile}\" },\n")

          list(APPEND objfiles "${objfile}")
          list(APPEND SWIFT_BENCH_OBJFILES "${objfile}")
      endforeach()
      string(CONCAT json "${json}" "}")
      file(WRITE "${objdir}/${module_name}/outputmap.json" ${json})

      add_custom_command(
          OUTPUT ${objfiles}
          DEPENDS
            ${stdlib_dependencies} ${bench_library_objects} ${sources}
          COMMAND "${SWIFT_EXEC}"
          ${common_options}
          ${bench_flags}
          "-parse-as-library"
          "-emit-module-path" "${objdir}/${module_name}.swiftmodule"
          "-module-name" "${module_name}"
          "-I" "${objdir}"
          "-output-file-map" "${objdir}/${module_name}/outputmap.json"
          ${sources})
    endif()
  endforeach()

  set(module_name "main")
  set(source "${srcdir}/utils/${module_name}.swift")
  add_custom_command(
      OUTPUT "${objdir}/${module_name}.o"
      DEPENDS
        ${stdlib_dependencies}
        ${bench_library_objects} ${SWIFT_BENCH_OBJFILES}
        ${bench_library_sibfiles} ${SWIFT_BENCH_SIBFILES} "${source}"
      COMMAND "${SWIFT_EXEC}"
      ${common_options}
      "-force-single-frontend-invocation"
      "-emit-module" "-module-name" "${module_name}"
      "-I" "${objdir}"
      "-o" "${objdir}/${module_name}.o"
      "${source}")
  list(APPEND SWIFT_BENCH_OBJFILES "${objdir}/${module_name}.o")

  if("${BENCH_COMPILE_ARCHOPTS_PLATFORM}" STREQUAL "macosx")
    set(OUTPUT_EXEC "${benchmark-bin-dir}/Benchmark_${BENCH_COMPILE_ARCHOPTS_OPT}")
  else()
    set(OUTPUT_EXEC "${benchmark-bin-dir}/Benchmark_${BENCH_COMPILE_ARCHOPTS_OPT}-${target}")
  endif()

  set(objcfile "${objdir}/ObjectiveCTests.o")
  add_custom_command(
      OUTPUT "${objcfile}"
      DEPENDS "${srcdir}/utils/ObjectiveCTests/ObjectiveCTests.m"
        "${srcdir}/utils/ObjectiveCTests/ObjectiveCTests.h"
      COMMAND
        "${CLANG_EXEC}"
        "-fno-stack-protector"
        "-fPIC"
        "-Werror=date-time"
        "-fcolor-diagnostics"
        "-O3"
        "-target" "${target}"
        "-isysroot" "${sdk}"
        "-fobjc-arc"
        "-arch" "${BENCH_COMPILE_ARCHOPTS_ARCH}"
        "-F" "${sdk}/../../../Developer/Library/Frameworks"
        "-m${triple_platform}-version-min=${ver}"
        "-I" "${srcdir}/utils/ObjectiveCTests"
        "${srcdir}/utils/ObjectiveCTests/ObjectiveCTests.m"
        "-c"
        "-o" "${objcfile}")

  add_custom_command(
      OUTPUT "${OUTPUT_EXEC}"
      DEPENDS
        ${bench_library_objects} ${SWIFT_BENCH_OBJFILES}
        "${objcfile}"
      COMMAND
        "${CLANG_EXEC}"
        "-fno-stack-protector"
        "-fPIC"
        "-Werror=date-time"
        "-fcolor-diagnostics"
        "-O3"
        "-Wl,-search_paths_first"
        "-Wl,-headerpad_max_install_names"
        "-target" "${target}"
        "-isysroot" "${sdk}"
        "-arch" "${BENCH_COMPILE_ARCHOPTS_ARCH}"
        "-F" "${sdk}/../../../Developer/Library/Frameworks"
        "-m${triple_platform}-version-min=${ver}"
        "-lobjc"
        "-L${SWIFT_LIBRARY_PATH}/${BENCH_COMPILE_ARCHOPTS_PLATFORM}"
        "-Xlinker" "-rpath"
        "-Xlinker" "@executable_path/../lib/swift/${BENCH_COMPILE_ARCHOPTS_PLATFORM}"
        ${bench_library_objects}
        ${SWIFT_BENCH_OBJFILES}
        ${objcfile}
        "-o" "${OUTPUT_EXEC}"
      COMMAND
        ${CMAKE_CODESIGN} "-f" "-s" "-" "${OUTPUT_EXEC}")
  set(new_output_exec "${OUTPUT_EXEC}" PARENT_SCOPE)
endfunction()

function(swift_benchmark_compile)
  cmake_parse_arguments(SWIFT_BENCHMARK_COMPILE "" "PLATFORM" "" ${ARGN})

  if(IS_SWIFT_BUILD)
    set(stdlib_dependencies "swift")
    foreach(stdlib_dependency ${UNIVERSAL_LIBRARY_NAMES_${SWIFT_BENCHMARK_COMPILE_PLATFORM}})
      string(FIND "${stdlib_dependency}" "Unittest" find_output)
      if("${find_output}" STREQUAL "-1")
        list(APPEND stdlib_dependencies "${stdlib_dependency}")
      endif()
    endforeach()
  endif()

  set(platform_executables)
  foreach(arch ${${SWIFT_BENCHMARK_COMPILE_PLATFORM}_arch})
    set(platform_executables)
    foreach(optset ${SWIFT_OPTIMIZATION_LEVELS})
      swift_benchmark_compile_archopts(
        PLATFORM "${platform}"
        ARCH "${arch}"
        OPT "${optset}")
      list(APPEND platform_executables ${new_output_exec})
    endforeach()

    set(executable_target "swift-benchmark-${SWIFT_BENCHMARK_COMPILE_PLATFORM}-${arch}")

    add_custom_target("${executable_target}"
        DEPENDS ${platform_executables})

    if(IS_SWIFT_BUILD AND "${SWIFT_BENCHMARK_COMPILE_PLATFORM}" STREQUAL "macosx")
      add_custom_command(
          TARGET "${executable_target}"
          POST_BUILD
          COMMAND
            "mv" ${platform_executables} "${swift-bin-dir}")

      add_custom_target("check-${executable_target}"
          COMMAND "${swift-bin-dir}/Benchmark_Driver" "run"
                  "-o" "O" "--output-dir" "${CMAKE_CURRENT_BINARY_DIR}/logs"
                  "--swift-repo" "${SWIFT_SOURCE_DIR}"
                  "--iterations" "${SWIFT_BENCHMARK_NUM_O_ITERATIONS}"
          COMMAND "${swift-bin-dir}/Benchmark_Driver" "run"
                  "-o" "Onone" "--output-dir" "${CMAKE_CURRENT_BINARY_DIR}/logs"
                  "--swift-repo" "${SWIFT_SOURCE_DIR}"
                  "--iterations" "${SWIFT_BENCHMARK_NUM_ONONE_ITERATIONS}"
          COMMAND "${swift-bin-dir}/Benchmark_Driver" "compare"
                  "--log-dir" "${CMAKE_CURRENT_BINARY_DIR}/logs"
                  "--swift-repo" "${SWIFT_SOURCE_DIR}"
                  "--compare-script"
                  "${SWIFT_SOURCE_DIR}/benchmark/scripts/compare_perf_tests.py")
    endif()
  endforeach()
endfunction()
