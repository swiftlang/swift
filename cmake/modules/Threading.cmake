# Get the default threading package for the platform
function(threading_package_default sdk out_var)
    if(sdk IN_LIST SWIFT_DARWIN_PLATFORMS)
      set("${out_var}" "darwin" PARENT_SCOPE)
    elseif(sdk STREQUAL "LINUX")
      set("${out_var}" "linux" PARENT_SCOPE)
    elseif(sdk STREQUAL "WINDOWS")
      set("${out_var}" "win32" PARENT_SCOPE)
    elseif(sdk STREQUAL "WASI")
      set("${out_var}" "none" PARENT_SCOPE)
    else()
      set("${out_var}" "pthreads" PARENT_SCOPE)
    endif()
endfunction()

# Given the threading package, find the name for the preprocessor
# define that we need to make.  Also deals with the default platform
# setting.
function(threading_package_name sdk out_var)
  precondition(SWIFT_HOST_VARIANT_SDK)
  precondition(SWIFT_DARWIN_PLATFORMS)

  string(TOUPPER "${SWIFT_THREADING_PACKAGE}" package)
  if(package STREQUAL "")
    threading_package_default(package)
    string(TOUPPER "${package}" package)
  endif()
  set("${out_var}" "${package}" PARENT_SCOPE)
endfunction()
