# Find the version of ld.gold, if installed.
#
# Versions prior to 2.36 break Swift programs because they won't coalesce
# sections with different SHF_GNU_RETAIN flags.
function(get_gold_version result_var_name)
  find_program(gold_executable "ld.gold")
  if(gold_executable)
    execute_process(
      COMMAND "${gold_executable}" "--version"
      COMMAND "head" "-n" "1"
      COMMAND "sed" "-e" "s/^.* (\\([^)]*\\)).*$/\\1/g;s/.* \\([0-9][0-9]*\\(\\.[0-9][0-9]*\\)*\\).*/\\1/g"
      OUTPUT_VARIABLE gold_version
      OUTPUT_STRIP_TRAILING_WHITESPACE)
    set("${result_var_name}" "${gold_version}" PARENT_SCOPE)
  else()
    set("${result_var_name}" "" PARENT_SCOPE)
  endif()
endfunction()
