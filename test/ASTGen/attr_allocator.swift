// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend-dump-parse \
// RUN:   -enable-experimental-feature ClassAllocators \
// RUN:   -enable-experimental-feature ParserASTGen \
// RUN:   | %sanitize-address > %t/astgen.ast

// RUN: %target-swift-frontend-dump-parse \
// RUN:   -enable-experimental-feature ClassAllocators \
// RUN:   | %sanitize-address > %t/cpp-parser.ast

// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_ClassAllocators

// rdar://116686158
// UNSUPPORTED: asan

@allocator(Pool) final class Simple {}
@allocator(Module.Pool) class Qualified {}
@allocator(Pool<Int>) class GenericAllocator<T> {}
@allocator(Outer.Nested.Pool) open class Nested {}
