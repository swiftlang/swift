#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_STD_SPAN_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_STD_SPAN_H

#include <cstddef>
#include <string>
#include <span>

using Span = std::span<const int>;
using SpanOfString = std::span<const std::string>;

static int iarray[]{1, 2, 3};
static std::string sarray[]{"", "ab", "abc"};
static Span ispan = {iarray};
static SpanOfString sspan = {sarray};

struct SpanBox {
  std::span<const int> ispan;
  std::span<const std::string> sspan;
};

inline Span initSpan() { 
  const int a[]{1, 2, 3};
  return Span(a);
}

inline struct SpanBox getStructSpanBox() { return {iarray, sarray}; }

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_STD_SPAN_H
