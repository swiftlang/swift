add_swift_host_library(swiftMarkup STATIC
  AST.cpp
  LineList.cpp
  Markup.cpp)
target_link_libraries(swiftMarkup PRIVATE
  libcmark-gfm_static)
target_compile_definitions(swiftMarkup
                           PRIVATE
                             CMARK_STATIC_DEFINE)

