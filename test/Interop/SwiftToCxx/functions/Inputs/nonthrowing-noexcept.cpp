#include "noexcept.h"
#include <utility>

using Noexcept::Reference;
using Noexcept::Value;

static_assert(noexcept(Noexcept::freeFunction()), "non-throwing free function");
static_assert(noexcept(Noexcept::identity(std::declval<const swift::Int &>())),
              "non-throwing generic free function");
static_assert(noexcept(Value::init(0)), "non-throwing initializer");
static_assert(noexcept(std::declval<const Value &>().read()), "const method");
static_assert(noexcept(std::declval<Value &>().increment()), "mutating method");
static_assert(noexcept(Value::make()), "static method");
static_assert(noexcept(std::declval<const Value &>().identity(
                  std::declval<const swift::Int &>())),
              "generic method");
static_assert(noexcept(std::declval<const Value &>().getNumber()), "getter");
static_assert(noexcept(std::declval<Value &>().setNumber(0)), "setter");
static_assert(noexcept(std::declval<const Value &>().isPositive()),
              "bool getter");
static_assert(noexcept(Value::getAnswer()), "static getter");
static_assert(noexcept(std::declval<const Value &>()[0]), "subscript");
static_assert(noexcept(Reference::init()), "class initializer");
static_assert(noexcept(std::declval<Reference &>().read()), "class method");
static_assert(noexcept(Reference::make()), "static class method");
static_assert(noexcept(std::declval<Reference &>().getNumber()),
              "class getter");
static_assert(noexcept(std::declval<Reference &>().setNumber(0)),
              "class setter");
static_assert(noexcept(std::declval<const Reference &>()[0]),
              "class subscript");
