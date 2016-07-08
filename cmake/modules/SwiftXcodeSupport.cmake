# This file contains cmake configuration specifically related to support for the
# Xcode generator in CMake.

function(apply_xcode_substitutions config path result_var_name)
  # Hack to deal with the fact that paths contain the build-time
  # variables. Note that this fix is Xcode-specific.
  string(REPLACE "$(CONFIGURATION)" "${config}" result "${path}")
  string(REPLACE "$(EFFECTIVE_PLATFORM_NAME)" "" result "${result}")

  set("${result_var_name}" "${result}" PARENT_SCOPE)
endfunction()

