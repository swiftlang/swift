function(check_cxx_native_regex result_var_name)
  if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    # Apple operating systems use libc++, which has a working std::regex.
    set("${result_var_name}" TRUE PARENT_SCOPE)
  else()
    if(CMAKE_CROSSCOMPILING)
      # Can't run C source when cross-compiling; assume false until we have a static check.
      set("${result_var_name}" FALSE PARENT_SCOPE)
    else()
      # libstdc++ 4.8 has an incomplete std::regex implementation, and crashes
      # on many regexes.
      # libstdc++ 4.9 works.
      set(std_regex_test_source
  "
  #include <regex>
  const std::regex broken_regex{
    \"([a]+)\",
    std::regex::ECMAScript | std::regex::nosubs};

  int main() {}
  ")

      check_cxx_source_runs("${std_regex_test_source}" "${result_var_name}_TEST")
      if ("${${result_var_name}_TEST}")
        set("${result_var_name}" TRUE PARENT_SCOPE)
      else()
        set("${result_var_name}" FALSE PARENT_SCOPE)
      endif()
    endif()
  endif()
endfunction()

