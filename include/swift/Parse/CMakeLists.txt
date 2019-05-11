if(CMAKE_HOST_SYSTEM_NAME STREQUAL Windows)
  set(SWIFT_GYB_FLAGS --line-directive "^\"#line %(line)d \\\"%(file)s\\\"^\"")
else()
  set(SWIFT_GYB_FLAGS --line-directive "\'#line" "%(line)d" "\"%(file)s\"\'")
endif()

set(generated_include_sources
  ParsedSyntaxBuilders.h.gyb
  ParsedSyntaxNodes.h.gyb
  ParsedSyntaxRecorder.h.gyb
)

add_gyb_target(swift-parse-syntax-generated-headers
    "${generated_include_sources}")
set_property(TARGET swift-parse-syntax-generated-headers
    PROPERTY FOLDER "Miscellaneous")
