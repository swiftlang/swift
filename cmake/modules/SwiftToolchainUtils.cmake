function(find_toolchain_tool result_var_name toolchain tool)
  execute_process(
      COMMAND "xcrun" "--toolchain" "${toolchain}" "--find" "${tool}"
      OUTPUT_VARIABLE tool_path
      OUTPUT_STRIP_TRAILING_WHITESPACE)
  set("${result_var_name}" "${tool_path}" PARENT_SCOPE)
endfunction()
