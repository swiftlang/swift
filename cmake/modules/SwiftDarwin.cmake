# On Darwin platforms that have xcrun, returns the path to the
# default toolchain directory.
function(get_default_toolchain_dir result_var_name)
  execute_process(
      COMMAND "xcrun" "--toolchain" "default" "--find" "clang"
      OUTPUT_VARIABLE toolchain_dir
      OUTPUT_STRIP_TRAILING_WHITESPACE)
  get_filename_component(toolchain_dir "${toolchain_dir}" PATH)
  get_filename_component(toolchain_dir "${toolchain_dir}" PATH)
  get_filename_component(toolchain_dir "${toolchain_dir}" PATH)
  set("${result_var_name}" "${toolchain_dir}" PARENT_SCOPE)
endfunction()

