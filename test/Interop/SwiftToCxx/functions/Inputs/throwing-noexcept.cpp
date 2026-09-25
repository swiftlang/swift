#include "throwing.h"
#include <utility>

using ThrowingNoexcept::Reference;
using ThrowingNoexcept::Value;

static_assert(!noexcept(ThrowingNoexcept::freeFunction()), "throwing function");
static_assert(!noexcept(Value::init(0)), "throwing initializer");
static_assert(!noexcept(std::declval<const Value &>().read()),
              "throwing method");
static_assert(!noexcept(std::declval<Value &>().update()), "throwing mutator");
static_assert(!noexcept(Value::make()), "throwing static method");
static_assert(!noexcept(std::declval<const Value &>().getComputed()),
              "throwing getter");
static_assert(!noexcept(std::declval<const Value &>().isPositive()),
              "throwing Bool getter");
static_assert(!noexcept(Value::getAnswer()), "throwing static getter");
static_assert(!noexcept(std::declval<const Value &>()[0]),
              "throwing subscript");
static_assert(!noexcept(std::declval<Reference &>().getComputed()),
              "throwing class getter");
static_assert(!noexcept(std::declval<const Reference &>()[0]),
              "throwing class subscript");
