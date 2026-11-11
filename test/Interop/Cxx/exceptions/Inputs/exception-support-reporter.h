#ifndef TEST_INTEROP_CXX_EXCEPTIONS_INPUTS_EXCEPTION_SUPPORT_REPORTER_H
#define TEST_INTEROP_CXX_EXCEPTIONS_INPUTS_EXCEPTION_SUPPORT_REPORTER_H

#pragma clang module import _SwiftCxxExceptionSupport

#include <stdexcept>

/// Throws the exception selected by `kind`, if any, and reports it from the
/// catch handler, like the adapters that the compiler generates.
inline int catchAndReport(int kind, void *context,
                          void (*callback)(void *, const char *) noexcept) {
  try {
    if (kind == 1)
      throw std::runtime_error("runtime error");
    if (kind == 2)
      throw 42;
    return 7;
  } catch (...) {
    __swift_cxx_report_current_exception(context, callback);
    return -1;
  }
}

#endif // TEST_INTEROP_CXX_EXCEPTIONS_INPUTS_EXCEPTION_SUPPORT_REPORTER_H
