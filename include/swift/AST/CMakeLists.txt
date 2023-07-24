if(CMAKE_HOST_SYSTEM_NAME STREQUAL "Windows")
  set(SWIFT_GYB_FLAGS --line-directive "^\"#line %(line)d \\\"%(file)s\\\"^\"")
else()
  set(SWIFT_GYB_FLAGS --line-directive "\'#line" "%(line)d" "\"%(file)s\"\'")
endif()

set(generated_include_sources
    Attr.def.gyb
    TokenKinds.def.gyb)

add_gyb_target(swift-ast-generated-headers
    "${generated_include_sources}"
    DEPENDS
      "${SWIFT_SOURCE_DIR}/utils/gyb_syntax_support/AttributeKinds.py"
      "${SWIFT_SOURCE_DIR}/utils/gyb_syntax_support/Token.py")
set_property(TARGET swift-ast-generated-headers
    PROPERTY FOLDER "Miscellaneous")
