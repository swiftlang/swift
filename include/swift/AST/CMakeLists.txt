if(CMAKE_HOST_SYSTEM_NAME STREQUAL Windows)
  set(SWIFT_GYB_FLAGS --line-directive "^\"#line %(line)d \\\"%(file)s\\\"^\"")
else()
  set(SWIFT_GYB_FLAGS --line-directive "\'#line" "%(line)d" "\"%(file)s\"\'")
endif()

set(generated_include_sources
    Attr.def.gyb)

add_gyb_target(swift-ast-generated-headers
    "${generated_include_sources}")
set_property(TARGET swift-ast-generated-headers
    PROPERTY FOLDER "Miscellaneous")
