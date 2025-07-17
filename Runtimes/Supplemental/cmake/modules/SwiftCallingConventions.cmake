include(CheckSourceCompiles)
include(CheckCompilerFlag)

check_source_compiles(CXX
"#if !(__has_attribute(swiftcall) && \
  __has_attribute(swift_context) && \
  __has_attribute(swift_error_result) && \
  __has_attribute(swift_indirect_result))
#error CXX compiler must support Swift calling conventions
#endif
int main(void) { return 0; }"
HAVE_SWIFTCALL)

if(NOT HAVE_SWIFTCALL)
  message(SEND_ERROR "CXX Compiler must support Swift calling conventions")
endif()

check_source_compiles(CXX
"#if !(__has_attribute(swiftasynccall) && \
  __has_attribute(swift_async_context))
#error CXX compiler must support Swift async calling conventions
#endif
int main(void) { return 0; }"
HAVE_SWIFT_ASYNC_CALL)

if(NOT HAVE_SWIFT_ASYNC_CALL)
  message(SEND_ERROR "CXX Compiler must support Swift async calling conventions")
endif()
