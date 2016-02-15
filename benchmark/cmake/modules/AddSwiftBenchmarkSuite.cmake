
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

function (_swift_benchmark_compile_file)
  cmake_parse_arguments(BENCH_COMPILE_FILE "IS_SIB;IS_SINGLE_FRONTEND_INVOCATION;IS_LIBRARY" "OBJDIR;MODULE_NAME;OUT_VAR" "SOURCES;EXTRA_DEPENDS;FLAGS" ${ARGN})
  set(outfile "${BENCH_COMPILE_FILE_OBJDIR}/${BENCH_COMPILE_MODULE_NAME}")

  if (BENCH_COMPILE_FILE_IS_SIB)
    set(outfile "${outfile}.sib")
  else()
    set(outfile "${outfile}.o")
  endif()

  set(extra_flags)
  if (BENCH_COMPILE_FILE_IS_SINGLE_FRONTEND_INVOCATION)
    list(APPEND extra_flags "-force-single-frontend-invocation")
  endif()
  if (BENCH_COMPILE_FILE_IS_LIBRARY)
    list(APPEND extra_flags "-parse-as-library")
  endif()


  add_custom_command(
    OUTPUT "${BENCH_COMPILE_FILE_OUTFILE}"
    DEPENDS ${BENCH_COMPILE_FILE_EXTRA_DEPENDS} ${BENCH_COMPILE_FILE_SOURCES}
    COMMAND ${SWIFT_EXEC}
    ${extra_flags}
    "-module-name" "${BENCH_COMPILE_MODULE_NAME}"
    "-I" "${objdir}"
    ${BENCH_COMPILE_FILE_FLAGS}
    ${BENCH_COMPILE_FILE_SOURCES}
    "-o" "${outfile}")
  set(${OUT_VAR} outfile PARENT_SCOPE)
endfunction(_swift_benchmark_compile_file)

function (swift_benchmark_compile_file)
  make_parse_arguments(BENCH_COMPILE_FILE "IS_SINGLE_FRONTEND_INVOCATION;IS_LIBRARY" "OBJDIR;MODULE_NAME;OBJ_OUT_VAR;SIB_OUT_VAR" "SOURCES;EXTRA_DEPENDS;FLAGS" ${ARGN})
  set(swiftmodule "${BENCH_COMPILE_FILE_OBJDIR}/${BENCH_COMPILE_FILE_MODULE_NAME}.swiftmodule")
  set(extra_boolean_args)
  if (BENCH_COMPILE_FILE_IS_LIBRARY)
    list(APPEND extra_boolean_args "IS_LIBRARY")
  endif()
  if (BENCH_COMPILE_FILE_IS_SINGLE_FRONTEND_INVOCATION)
    list(APPEND extra_boolean_args "IS_SINGLE_FRONTEND_INVOCATION")
  endif()

  _swift_benchmark_compile_file(
    ${extra_boolean_args}
    OBJDIR "${BENCH_COMPILE_FILE_OBJDIR}"
    MODULE_NAME "${BENCH_COMPILE_FILE_MODULE_NAME}"
    SOURCES ${BENCH_COMPILE_FILE_SOURCES}
    FLAGS ${BENCH_COMPILE_FILE_FLAGS} "-emit-module" "-emit-module-path" "${swiftmodule}"
    EXTRA_DEPENDS ${BENCH_COMPILE_FILE_EXTRA_DEPENDS}
    OUT_VAR new_obj_file)
  set(${BENCH_COMPILE_FILE_OBJ_OUT_VAR} ${new_obj_file} PARENT_SCOPE)

  if(SWIFT_BENCHMARK_EMIT_SIB)
    _swift_benchmark_compile_file(
      IS_SIB
      ${extra_boolean_args}
      OBJDIR "${BENCH_COMPILE_FILE_OBJDIR}"
      MODULE_NAME "${module_name}"
      SOURCES ${BENCH_COMPILE_FILE_SOURCES}
      FLAGS ${BENCH_COMPILE_FILE_FLAGS}
      EXTRA_DEPENDS ${BENCH_COMPILE_FILE_EXTRA_DEPENDS}
      OUT_VAR new_sib_file)
    set(${BENCH_COMPILE_FILE_SIB_OUT_VAR} ${new_sib_file} PARENT_SCOPE)
  endif()
endfunction()

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
      "-D" "SWIFT_ENABLE_OBJECT_LITERALS"
      "-no-link-objc-runtime")

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
      "-D" "SWIFT_ENABLE_OBJECT_LITERALS"
      "-no-link-objc-runtime")

  set(bench_library_objects)
  set(bench_library_sibfiles)
  foreach(module_name_path ${BENCH_DRIVER_LIBRARY_MODULES})
    get_filename_component(module_name "${module_name_path}" NAME)

    set(source "${srcdir}/${module_name_path}.swift")
    if("${module_name}" STREQUAL "DriverUtils")
      set(extra_sources "${srcdir}/utils/ArgParse.swift")
    endif()

    set(new_obj_file)
    set(new_sib_file)
    swift_benchmark_compile_file(
      IS_LIBRARY
      IS_SINGLE_FRONTEND_INVOCATION
      OBJDIR "${objdir}"
      MODULE_NAME "${module_name}"
      SOURCES ${source} ${extra_sources}
      FLAGS ${common_options_driver} ${BENCH_DRIVER_LIBRARY_FLAGS}
      EXTRA_DEPENDS ${stdlib_dependencies}
      OBJ_OUT_VAR new_obj_file
      SIB_OUT_VAR new_sib_file)
    if (new_obj_file)
      list(APPEND bench_library_objects "${new_obj_file}")
    endif()
    if (new_sib_file)
      list(APPEND bench_library_sibfiles "${new_sib_file}")
    endif()
  endforeach()

  foreach(module_name_path ${BENCH_LIBRARY_MODULES})
    get_filename_component(module_name "${module_name_path}" NAME)

    set(source "${srcdir}/${module_name_path}.swift")

    set(new_obj_file)
    set(new_sib_file)
    swift_benchmark_compile_file(
      IS_LIBRARY
      IS_SINGLE_FRONTEND_INVOCATION
      OBJDIR "${objdir}"
      MODULE_NAME "${module_name}"
      SOURCES ${source}
      FLAGS ${common_options}
      EXTRA_DEPENDS ${stdlib_dependencies}
      OBJ_OUT_VAR new_obj_file
      SIB_OUT_VAR new_sib_file)
    if (new_obj_file)
      list(APPEND bench_library_objects "${new_obj_file}")
    endif()
    if (new_sib_file)
      list(APPEND bench_library_sibfiles "${new_sib_file}")
    endif()
  endforeach()

  set(SWIFT_BENCH_OBJFILES)
  set(SWIFT_BENCH_SIBFILES)
  foreach(module_name_path ${SWIFT_BENCH_MODULES})
    get_filename_component(module_name "${module_name_path}" NAME)

    set(source "${srcdir}/${module_name_path}.swift")

    if(module_name)
      set(new_obj_file)
      set(new_sib_file)
      swift_benchmark_compile_file(
        IS_LIBRARY
        OBJDIR "${objdir}"
        MODULE_NAME "${module_name}"
        SOURCES "${source}"
        FLAGS ${common_options} ${bench_flags}
        EXTRA_DEPENDS ${stdlib_dependencies} ${bench_library_objects}
        OBJ_OUT_VAR new_obj_file
        SIB_OUT_VAR new_sib_file)
      if (new_obj_file)
        list(APPEND SWIFT_BENCH_OBJFILES "${new_obj_file}")
      endif()
      if (new_sib_file)
        list(APPEND SWIFT_BENCH_SIBFILES "${new_sib_file}")
      endif()
    endif()
  endforeach()

  foreach(module_name_path ${SWIFT_MULTISOURCE_BENCHES})
    get_filename_component(module_name "${module_name_path}" NAME)

    if ("${bench_flags}" MATCHES "-whole-module.*" AND
        NOT "${bench_flags}" MATCHES "-num-threads.*")
      # Regular whole-module-compilation: only a single object file is
      # generated.
      set(sources)
      foreach(source ${${module_name}_sources})
        list(APPEND sources "${srcdir}/${source}")
      endforeach()

      set(new_obj_file)
      set(new_sib_file)
      swift_benchmark_compile_file(
        IS_LIBRARY
        OBJDIR "${objdir}"
        MODULE_NAME "${module_name}"
        SOURCES "${sources}"
        FLAGS ${common_options} ${bench_flags}
        EXTRA_DEPENDS ${stdlib_dependencies} ${bench_library_objects}
        OBJ_OUT_VAR new_obj_file
        SIB_OUT_VAR new_sib_file)
      if (new_obj_file)
        list(APPEND SWIFT_BENCH_OBJFILES "${new_obj_file}")
      endif()
      if (new_sib_file)
        list(APPEND SWIFT_BENCH_SIBFILES "${new_sib_file}")
      endif()

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

  add_custom_command(
      OUTPUT "${OUTPUT_EXEC}"
      DEPENDS
        ${bench_library_objects} ${SWIFT_BENCH_OBJFILES}
        "adhoc-sign-swift-stdlib-${BENCH_COMPILE_ARCHOPTS_PLATFORM}"
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
        "-o" "${OUTPUT_EXEC}"
      COMMAND
        "codesign" "-f" "-s" "-" "${OUTPUT_EXEC}")
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

  add_custom_target("copy-swift-stdlib-${SWIFT_BENCHMARK_COMPILE_PLATFORM}"
      DEPENDS ${stdlib_dependencies}
      COMMAND
        "${CMAKE_COMMAND}" "-E" "copy_directory"
        "${SWIFT_LIBRARY_PATH}/${SWIFT_BENCHMARK_COMPILE_PLATFORM}"
        "${benchmark-lib-swift-dir}/${SWIFT_BENCHMARK_COMPILE_PLATFORM}")

  add_custom_target("adhoc-sign-swift-stdlib-${SWIFT_BENCHMARK_COMPILE_PLATFORM}"
      DEPENDS "copy-swift-stdlib-${SWIFT_BENCHMARK_COMPILE_PLATFORM}"
      COMMAND
        "codesign" "-f" "-s" "-"
        "${benchmark-lib-swift-dir}/${SWIFT_BENCHMARK_COMPILE_PLATFORM}/*.dylib" "2>/dev/null")

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
                  "--iterations" "3"
          COMMAND "${swift-bin-dir}/Benchmark_Driver" "run"
                  "-o" "Onone" "--output-dir" "${CMAKE_CURRENT_BINARY_DIR}/logs"
                  "--swift-repo" "${SWIFT_SOURCE_DIR}"
                  "--iterations" "3"
          COMMAND "${swift-bin-dir}/Benchmark_Driver" "compare"
                  "--log-dir" "${CMAKE_CURRENT_BINARY_DIR}/logs"
                  "--swift-repo" "${SWIFT_SOURCE_DIR}"
                  "--compare-script"
                  "${SWIFT_SOURCE_DIR}/benchmark/scripts/compare_perf_tests.py")
    endif()
  endforeach()
endfunction()
