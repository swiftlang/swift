#include "throwing.h"
#include <utility>

using ThrowingNoexcept::Value;

static_assert(!noexcept(ThrowingNoexcept::freeFunction()), "throwing function");
static_assert(!noexcept(Value::init(0)), "throwing initializer");
static_assert(!noexcept(std::declval<const Value &>().read()),
              "throwing method");
static_assert(!noexcept(std::declval<Value &>().update()), "throwing mutator");
static_assert(!noexcept(Value::make()), "throwing static method");
