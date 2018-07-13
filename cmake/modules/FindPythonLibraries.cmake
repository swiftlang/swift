# SWIFT_ENABLE_TENSORFLOW
# Find Python libraries.

# This file becomes irrelevant in CMake 3.12, which defines FindPython2 and
# FindPython3.

# The Python versions supported for interop with Swift.
set(PYTHON_INTEROP_VERSIONS 2.7 3)

function(reset_python_variables)
  foreach(scope "CACHE" "PARENT_SCOPE")
    unset(PYTHON_INCLUDE_DIRS ${scope})
    unset(PYTHON_INCLUDE_DIR ${scope})
    unset(PYTHON_LIBRARIES ${scope})
    unset(PYTHON_LIBRARY ${scope})
    unset(FIND_PACKAGE_MESSAGE_DETAILS_PythonLibs ${scope})
  endforeach()
endfunction()
