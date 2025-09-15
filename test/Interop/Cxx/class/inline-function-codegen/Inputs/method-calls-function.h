#ifndef TEST_INTEROP_CXX_CLASS_INLINE_FUNCTION_THROUGH_MEMBER_INPUTS_METHOD_CALLS_FUNCTION_H
#define TEST_INTEROP_CXX_CLASS_INLINE_FUNCTION_THROUGH_MEMBER_INPUTS_METHOD_CALLS_FUNCTION_H

inline int increment(int t) { return t + 1; }

struct Incrementor {
  int callIncrement(int value) { return increment(value); }
};

inline int callMethod(int value) { return Incrementor().callIncrement(value); }

class __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal")))
__attribute__((swift_attr("unsafe"))) Cell {
public:
  bool is_marked() const { return m_marked; }
  void set_marked(bool b) { m_marked = b; }

private:
  bool m_marked : 1 {false};
};

inline Cell *_Nonnull createCell() { return new Cell{}; }

#endif // TEST_INTEROP_CXX_CLASS_INLINE_FUNCTION_THROUGH_MEMBER_INPUTS_METHOD_CALLS_FUNCTION_H
