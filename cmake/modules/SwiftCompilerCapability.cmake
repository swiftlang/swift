# Test if the Swift compiler returns success for supplied compiler arguments....
function(swift_supports_compiler_arguments out_var)
  file(WRITE "${CMAKE_BINARY_DIR}/tmp/dummy.swift" "")
  execute_process(
    COMMAND "${CMAKE_Swift_COMPILER}" -parse ${ARGN} -
    INPUT_FILE "${CMAKE_BINARY_DIR}/tmp/dummy.swift"
    OUTPUT_QUIET ERROR_QUIET
    RESULT_VARIABLE result
  )
  if(NOT result)
    set("${out_var}" "TRUE" PARENT_SCOPE)
  else()
    set("${out_var}" "FALSE" PARENT_SCOPE)
  endif()
endfunction()

# Test if the Swift compiler supports -disable-implicit-<module>-module-import.
macro(swift_supports_implicit_module module_name out_var)
  swift_supports_compiler_arguments(${out_var}
    -Xfrontend -disable-implicit-${module_name}-module-import
  )
endmacro()

# Get "package cross-module-optimization" compiler arguments suitable for the compiler.
function(swift_get_package_cmo_support out_var)
  # > 6.0 : Fixed feature.
  swift_supports_compiler_arguments(result
    -package-name my-package
    -Xfrontend -package-cmo
    -Xfrontend -allow-non-resilient-access
  )
  if(result)
    set(${out_var} IMPLEMENTED PARENT_SCOPE)
    return()
  endif()

  # == 6.0 : Experimental.
  swift_supports_compiler_arguments(result
    -package-name my-package
    -Xfrontend -experimental-package-cmo
    -Xfrontend -experimental-allow-non-resilient-access
    -Xfrontend -experimental-package-bypass-resilience
  )
  if(result)
    set(${out_var} EXPERIMENTAL PARENT_SCOPE)
    return()
  endif()

  # < 6.0 : Not supported.
  set(${out_var} NO PARENT_SCOPE)
endfunction()
