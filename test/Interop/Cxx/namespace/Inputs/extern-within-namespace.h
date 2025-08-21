namespace Outer {
namespace Inner {
extern "C" {
int foobar() { return 123; }
struct NestedType {
  char c;
};
}
} // namespace Inner

inline namespace InnerInline {
extern "C" {
int baz() { return 321; }
}
} // namespace InnerInline
} // namespace Outer

namespace ExternWithinExtern {
extern "C" {
extern "C++" {
namespace Inner {
extern "C" {
int deep() { return 42; }
}
} // namespace Inner
}
}
} // namespace ExternWithinExtern
