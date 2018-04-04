
include(CMakeParseArguments)
include(SwiftBenchmarkUtils)

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

function (add_swift_benchmark_library objfile_out sibfile_out)
  cmake_parse_arguments(BENCHLIB "" "MODULE_PATH;SOURCE_DIR;OBJECT_DIR" "SOURCES;LIBRARY_FLAGS;DEPENDS" ${ARGN})

  precondition(BENCHLIB_MODULE_PATH)
  precondition(BENCHLIB_SOURCE_DIR)
  precondition(BENCHLIB_OBJECT_DIR)
  precondition(BENCHLIB_SOURCES)

  set(module_name_path "${BENCHLIB_MODULE_PATH}")
  get_filename_component(module_name "${module_name_path}" NAME)
  set(srcdir "${BENCHLIB_SOURCE_DIR}")
  set(objdir "${BENCHLIB_OBJECT_DIR}")
  set(sources "${BENCHLIB_SOURCES}")

  set(objfile "${objdir}/${module_name}.o")
  set(swiftmodule "${objdir}/${module_name}.swiftmodule")

  precondition(objfile_out)
  add_custom_command(
    OUTPUT "${objfile}"
    DEPENDS ${stdlib_dependencies} ${sources} ${BENCHLIB_DEPENDS}
    COMMAND "${SWIFT_EXEC}"
      ${BENCHLIB_LIBRARY_FLAGS}
      "-force-single-frontend-invocation"
      "-parse-as-library"
      "-module-name" "${module_name}"
      "-emit-module" "-emit-module-path" "${swiftmodule}"
      "-I" "${objdir}"
      "-o" "${objfile}"
      ${sources})
  set(${objfile_out} "${objfile}" PARENT_SCOPE)

  if(SWIFT_BENCHMARK_EMIT_SIB)
    precondition(sibfile_out)
    set(sibfile "${objdir}/${module_name}.sib")

    add_custom_command(
      OUTPUT "${sibfile}"
      DEPENDS
      ${stdlib_dependencies} ${sources} ${BENCHLIB_DEPENDS}
      COMMAND "${SWIFT_EXEC}"
        ${BENCHLIB_LIBRARY_FLAGS}
        "-force-single-frontend-invocation"
        "-parse-as-library"
        "-module-name" "${module_name}"
        "-emit-sib"
        "-I" "${objdir}"
        "-o" "${sibfile}"
        ${sources})
    set(sibfile_out "${sibfile}" PARENT_SCOPE)
  endif()
endfunction()

function(_construct_sources_for_multibench sources_out objfile_out)
  cmake_parse_arguments(SOURCEJSONLIST "" "" "SOURCES" ${ARGN})

  set(sources)
  set(objfiles)

  foreach(source ${SOURCEJSONLIST_SOURCES})
    list(APPEND sources "${srcdir}/${source}")

    get_filename_component(basename "${source}" NAME_WE)
    set(objfile "${objdir}/${module_name}/${basename}.o")

    string(CONCAT json "${json}"
      "  \"${srcdir}/${source}\": { \"object\": \"${objfile}\" },\n")

    list(APPEND objfiles "${objfile}")
  endforeach()
  string(CONCAT json "${json}" "}")
  file(WRITE "${objdir}/${module_name}/outputmap.json" ${json})
  set(${sources_out} ${sources} PARENT_SCOPE)
  set(${objfile_out} ${objfiles} PARENT_SCOPE)
endfunction()

function(add_opt_view opt_view_main_dir, module_name, opt_view_dir_out)
  set(opt_view_dir "${opt_view_main_dir}/${module_name}")
  set(opt_record "${objdir}/${module_name}.opt.yaml")
  add_custom_command(
    OUTPUT ${opt_view_dir}
    DEPENDS "${objfile}"
    COMMAND ${SWIFT_BENCHMARK_OPT_VIEWER} ${opt_record} "-o" ${opt_view_dir})
  set(${opt_view_dir_out} ${opt_view_dir} PARENT_SCOPE)
endfunction()

# Regular whole-module-compilation: only a single object file is
# generated.
function (add_swift_multisource_wmo_benchmark_library objfile_out)
  cmake_parse_arguments(BENCHLIB "" "MODULE_PATH;SOURCE_DIR;OBJECT_DIR" "SOURCES;LIBRARY_FLAGS;DEPENDS" ${ARGN})

  precondition(BENCHLIB_MODULE_PATH)
  precondition(BENCHLIB_SOURCE_DIR)
  precondition(BENCHLIB_OBJECT_DIR)
  precondition(BENCHLIB_SOURCES)

  set(module_name_path "${BENCHLIB_MODULE_PATH}")
  get_filename_component(module_name "${module_name_path}" NAME)
  set(srcdir "${BENCHLIB_SOURCE_DIR}")
  set(objdir "${BENCHLIB_OBJECT_DIR}")

  set(objfile "${objdir}/${module_name}.o")

  set(sources)
  foreach(source ${BENCHLIB_SOURCES})
    list(APPEND sources "${srcdir}/${source}")
  endforeach()

  add_custom_command(
    OUTPUT "${objfile}"
    DEPENDS
      ${sources} ${BENCHLIB_DEPENDS}
    COMMAND "${SWIFT_EXEC}"
      ${BENCHLIB_LIBRARY_FLAGS}
      "-parse-as-library"
      "-emit-module" "-module-name" "${module_name}"
      "-I" "${objdir}"
      "-o" "${objfile}"
      ${sources})

  set(${objfile_out} "${objfile}" PARENT_SCOPE)
endfunction()

function(add_swift_multisource_nonwmo_benchmark_library objfiles_out)
  cmake_parse_arguments(BENCHLIB "" "MODULE_PATH;SOURCE_DIR;OBJECT_DIR" "SOURCES;LIBRARY_FLAGS;DEPENDS" ${ARGN})

  precondition(BENCHLIB_MODULE_PATH)
  precondition(BENCHLIB_SOURCE_DIR)
  precondition(BENCHLIB_OBJECT_DIR)
  precondition(BENCHLIB_SOURCES)

  set(module_name_path "${BENCHLIB_MODULE_PATH}")
  get_filename_component(module_name "${module_name_path}" NAME)
  set(srcdir "${BENCHLIB_SOURCE_DIR}")
  set(objdir "${BENCHLIB_OBJECT_DIR}")

  set(objfile "${objdir}/${module_name}.o")

  # No whole-module-compilation or multi-threaded compilation.
  # There is an output object file for each input file. We have to write
  # an output-map-file to specify the output object file names.
  set(sources)
  set(objfiles)
  _construct_sources_for_multibench(sources objfiles
    SOURCES ${BENCHLIB_SOURCES})

  add_custom_command(
    OUTPUT ${objfiles}
    DEPENDS ${sources} ${BENCHLIB_DEPENDS}
    COMMAND "${SWIFT_EXEC}"
      ${BENCHLIB_LIBRARY_FLAGS}
      "-parse-as-library"
      "-emit-module-path" "${objdir}/${module_name}.swiftmodule"
      "-module-name" "${module_name}"
      "-I" "${objdir}"
      "-output-file-map" "${objdir}/${module_name}/outputmap.json"
      ${sources})

  set(${objfiles_out} ${objfiles} PARENT_SCOPE)
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
      "-no-link-objc-runtime"
      "-I" "${srcdir}/utils/ObjectiveCTests")

  set(opt_view_main_dir)
  if(SWIFT_BENCHMARK_GENERATE_OPT_VIEW AND LLVM_HAVE_OPT_VIEWER_MODULES)
    if(NOT ${optflag} STREQUAL "Onone" AND "${bench_flags}" MATCHES "-whole-module.*")
      list(APPEND common_options "-save-optimization-record")
      set(opt_view_main_dir "${objdir}/opt-view")
    endif()
  endif()

  set(common_swift4_options ${common_options} "-swift-version" "4")

  # Always optimize the driver modules.
  # Note that we compile the driver for Osize also with -Osize
  # (and not with -O), because of <rdar://problem/19614516>.
  string(REPLACE "Onone" "O" driver_opt "${optflag}")

  set(common_options_driver
      "-c"
      "-sdk" "${sdk}"
      "-target" "${target}"
      "-F" "${sdk}/../../../Developer/Library/Frameworks"
      "-${driver_opt}"
      "-no-link-objc-runtime")

  set(bench_library_objects)
  set(bench_library_sibfiles)
  set(opt_view_dirs)
  # Build libraries used by the driver and benchmarks.
  foreach(module_name_path ${BENCH_LIBRARY_MODULES})
    set(sources "${srcdir}/${module_name_path}.swift")

    add_swift_benchmark_library(objfile_out sibfile_out
      MODULE_PATH "${module_name_path}"
      SOURCE_DIR "${srcdir}"
      OBJECT_DIR "${objdir}"
      SOURCES ${sources}
      LIBRARY_FLAGS ${common_swift4_options})
    precondition(objfile_out)
    list(APPEND bench_library_objects "${objfile_out}")
    if (SWIFT_BENCHMARK_EMIT_SIB)
      precondition(sibfile_out)
      list(APPEND bench_library_sibfiles "${sibfile_out}")
    endif()
  endforeach()
  precondition(bench_library_objects)

  set(bench_driver_objects)
  set(bench_driver_sibfiles)
  foreach(module_name_path ${BENCH_DRIVER_LIBRARY_MODULES})
    set(sources "${srcdir}/${module_name_path}.swift")

    get_filename_component(module_name "${module_name_path}" NAME)
    if("${module_name}" STREQUAL "DriverUtils")
      list(APPEND sources "${srcdir}/utils/ArgParse.swift")
    endif()

    set(objfile_out)
    set(sibfile_out)
    add_swift_benchmark_library(objfile_out sibfile_out
      MODULE_PATH "${module_name_path}"
      SOURCE_DIR "${srcdir}"
      OBJECT_DIR "${objdir}"
      SOURCES ${sources}
      LIBRARY_FLAGS ${common_options_driver} ${BENCH_DRIVER_LIBRARY_FLAGS}
      DEPENDS ${bench_library_objects})
    precondition(objfile_out)
    list(APPEND bench_driver_objects "${objfile_out}")
    if (SWIFT_BENCHMARK_EMIT_SIB)
      precondition(sibfile_out)
      list(APPEND bench_driver_sibfiles "${sibfile_out}")
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
          ${common_swift4_options}
          ${extra_options}
          "-parse-as-library"
          ${bench_flags}
          ${SWIFT_BENCHMARK_EXTRA_FLAGS}
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
            ${common_swift4_options}
            "-parse-as-library"
            ${bench_flags}
            ${SWIFT_BENCHMARK_EXTRA_FLAGS}
            "-module-name" "${module_name}"
            "-I" "${objdir}"
            "-emit-sib"
            "-o" "${sibfile}"
            "${source}")
      endif()

      if(opt_view_main_dir)
        set(opt_view_dir)
        add_opt_view(${opt_view_main_dir}, ${module_name}, opt_view_dir)
        precondition(opt_view_dir)
        list(APPEND opt_view_dirs ${opt_view_dir})
      endif()
    endif()
  endforeach()

  foreach(module_name_path ${SWIFT_MULTISOURCE_SWIFT3_BENCHES})
    get_filename_component(module_name "${module_name_path}" NAME)

    if ("${bench_flags}" MATCHES "-whole-module.*" AND
        NOT "${bench_flags}" MATCHES "-num-threads.*")
      set(objfile_out)
      add_swift_multisource_wmo_benchmark_library(objfile_out
        MODULE_PATH "${module_name_path}"
        SOURCE_DIR "${srcdir}"
        OBJECT_DIR "${objdir}"
        SOURCES ${${module_name}_sources}
        LIBRARY_FLAGS ${common_swift4_options} ${bench_flags} ${SWIFT_BENCHMARK_EXTRA_FLAGS}
        DEPENDS ${bench_library_objects} ${stdlib_dependencies})
      precondition(objfile_out)
      list(APPEND SWIFT_BENCH_OBJFILES "${objfile_out}")

      if(opt_view_main_dir)
        set(opt_view_dir)
        add_opt_view(${opt_view_main_dir}, ${module_name}, opt_view_dir)
        precondition(opt_view_dir)
        list(APPEND opt_view_dirs ${opt_view_dir})
      endif()
    else()
      set(objfiles_out)
      add_swift_multisource_nonwmo_benchmark_library(objfiles_out
        MODULE_PATH "${module_name_path}"
        SOURCE_DIR "${srcdir}"
        OBJECT_DIR "${objdir}"
        SOURCES ${${module_name}_sources}
        LIBRARY_FLAGS ${common_swift4_options} ${bench_flags} ${SWIFT_BENCHMARK_EXTRA_FLAGS}
        DEPENDS ${bench_library_objects} ${stdlib_dependencies})
      precondition(objfiles_out)
      list(APPEND SWIFT_BENCH_OBJFILES ${objfiles_out})
    endif()
  endforeach()

  foreach(module_name_path ${SWIFT_MULTISOURCE_SWIFT4_BENCHES})
    get_filename_component(module_name "${module_name_path}" NAME)

    if ("${bench_flags}" MATCHES "-whole-module.*" AND
        NOT "${bench_flags}" MATCHES "-num-threads.*")
      set(objfile_out)
      add_swift_multisource_wmo_benchmark_library(objfile_out
        MODULE_PATH "${module_name_path}"
        SOURCE_DIR "${srcdir}"
        OBJECT_DIR "${objdir}"
        SOURCES ${${module_name}_sources}
        LIBRARY_FLAGS ${common_swift4_options} ${bench_flags} ${SWIFT_BENCHMARK_EXTRA_FLAGS}
        DEPENDS ${bench_library_objects} ${stdlib_dependencies})
      precondition(objfile_out)
      list(APPEND SWIFT_BENCH_OBJFILES "${objfile_out}")

      if(opt_view_main_dir)
        set(opt_view_dir)
        add_opt_view(${opt_view_main_dir}, ${module_name}, opt_view_dir)
        precondition(opt_view_dir)
        list(APPEND opt_view_dirs ${opt_view_dir})
      endif()
    else()
      set(objfiles_out)
      add_swift_multisource_nonwmo_benchmark_library(objfiles_out
        MODULE_PATH "${module_name_path}"
        SOURCE_DIR "${srcdir}"
        OBJECT_DIR "${objdir}"
        SOURCES ${${module_name}_sources}
        LIBRARY_FLAGS ${common_swift4_options} ${bench_flags} ${SWIFT_BENCHMARK_EXTRA_FLAGS}
        DEPENDS ${bench_library_objects} ${stdlib_dependencies})
      precondition(objfiles_out)
      list(APPEND SWIFT_BENCH_OBJFILES ${objfiles_out})
    endif()
  endforeach()

  set(module_name "main")
  set(source "${srcdir}/utils/${module_name}.swift")
  add_custom_command(
      OUTPUT "${objdir}/${module_name}.o"
      DEPENDS
        ${stdlib_dependencies}
        ${bench_library_objects} ${bench_driver_objects} ${SWIFT_BENCH_OBJFILES}
        ${bench_library_sibfiles} ${bench_driver_sibfiles}
        ${SWIFT_BENCH_SIBFILES} "${source}"
      COMMAND "${SWIFT_EXEC}"
      ${common_swift4_options}
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
        ${bench_library_objects} ${bench_driver_objects} ${SWIFT_BENCH_OBJFILES}
        "${objcfile}" ${opt_view_dirs}
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
        ${bench_driver_objects}
        ${SWIFT_BENCH_OBJFILES}
        ${objcfile}
        "-o" "${OUTPUT_EXEC}"
      COMMAND
        "codesign" "-f" "-s" "-" "${OUTPUT_EXEC}")
  set(new_output_exec "${OUTPUT_EXEC}" PARENT_SCOPE)
endfunction()

function(swift_benchmark_compile)
  cmake_parse_arguments(SWIFT_BENCHMARK_COMPILE "" "PLATFORM" "" ${ARGN})

  if(NOT SWIFT_BENCHMARK_BUILT_STANDALONE)
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

    # If we are building standalone as part of a subcmake build, we add the
    # -external suffix to all of our cmake target names. This enables the main
    # swift build to simple create -external targets and forward them via
    # AddExternalProject to the standalone benchmark project. The reason why
    # this is necessary is that we want to be able to support in-tree and
    # out-of-tree benchmark builds at the same time implying that we need some
    # sort of way to distinguish the in-tree (which don't have the suffix) from
    # the out of tree target (which do).
    translate_flag(SWIFT_BENCHMARK_SUBCMAKE_BUILD "-external" external)
    set(executable_target "swift-benchmark-${SWIFT_BENCHMARK_COMPILE_PLATFORM}-${arch}${external}")

    add_custom_target("${executable_target}"
        DEPENDS ${platform_executables})

    if(NOT SWIFT_BENCHMARK_BUILT_STANDALONE AND "${SWIFT_BENCHMARK_COMPILE_PLATFORM}" STREQUAL "macosx")
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
