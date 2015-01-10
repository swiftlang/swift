#include "swift/Driver/DependencyGraph.h"
#include "gtest/gtest.h"

using namespace swift;
using LoadResult = DependencyGraphImpl::LoadResult;

TEST(DependencyGraph, BasicLoad) {
  DependencyGraph<uintptr_t> graph;
  uintptr_t i = 0;

  EXPECT_EQ(graph.loadFromString(i++, "top-level: [a, b]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(i++, "member-access: [c, d]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(i++, "provides: [e, f]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(i++, "nominals: [g, h]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(i++, "class-members: [i, j]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(i++, "dynamic-lookup: [k, l]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(i++, "cross-module: [/foo, /bar]"),
            LoadResult::UpToDate);

  EXPECT_EQ(graph.loadFromString(i++,
                                 "nominals: [a, b]\n"
                                 "provides: [b, c]\n"
                                 "member-access: [c, d]\n"
                                 "top-level: [d, a]\n"),
            LoadResult::UpToDate);
}

TEST(DependencyGraph, IndependentNodes) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "top-level: [a]\nprovides: [a0]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "top-level: [b]\nprovides: [b0]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(2, "top-level: [c]\nprovides: [c0]"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 0);
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_FALSE(graph.isMarked(1));
  EXPECT_FALSE(graph.isMarked(2));

  // Mark 0 again -- should be no change.
  graph.markTransitive(marked, 0);
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_FALSE(graph.isMarked(1));
  EXPECT_FALSE(graph.isMarked(2));

  graph.markTransitive(marked, 2);
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_FALSE(graph.isMarked(1));
  EXPECT_TRUE(graph.isMarked(2));

  graph.markTransitive(marked, 1);
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
  EXPECT_TRUE(graph.isMarked(2));
}

TEST(DependencyGraph, IndependentDepKinds) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "member-access: [a]\nnominals: [b]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "top-level: [b]\nprovides: [a]"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 0);
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_FALSE(graph.isMarked(1));
}

TEST(DependencyGraph, IndependentDepKinds2) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "member-access: [a]\nnominals: [b]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "top-level: [b]\nprovides: [a]"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 1);
  EXPECT_EQ(0u, marked.size());
  EXPECT_FALSE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
}

TEST(DependencyGraph, SimpleDependent) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "provides: [a, b, c]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "top-level: [x, b, z]"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 0);
  EXPECT_EQ(1u, marked.size());
  EXPECT_EQ(1u, marked.front());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));

  marked.clear();
  graph.markTransitive(marked, 0);
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
}

TEST(DependencyGraph, SimpleDependentReverse) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "top-level: [a, b, c]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "provides: [x, b, z]"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 1);
  EXPECT_EQ(1u, marked.size());
  EXPECT_EQ(0u, marked.front());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));

  marked.clear();
  graph.markTransitive(marked, 0);
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
}

TEST(DependencyGraph, SimpleDependent2) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "nominals: [a, b, c]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "member-access: [x, b, z]"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 0);
  EXPECT_EQ(1u, marked.size());
  EXPECT_EQ(1u, marked.front());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));

  marked.clear();
  graph.markTransitive(marked, 0);
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
}

TEST(DependencyGraph, SimpleDependent3) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "nominals: [a]\nprovides: [a]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "member-access: [a]"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 0);
  EXPECT_EQ(1u, marked.size());
  EXPECT_EQ(1u, marked.front());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));

  marked.clear();
  graph.markTransitive(marked, 0);
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
}

TEST(DependencyGraph, SimpleDependent4) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "nominals: [a]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "member-access: [a]\ntop-level: [a]"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 0);
  EXPECT_EQ(1u, marked.size());
  EXPECT_EQ(1u, marked.front());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));

  marked.clear();
  graph.markTransitive(marked, 0);
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
}

TEST(DependencyGraph, SimpleDependent5) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "nominals: [a]\nprovides: [a]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "member-access: [a]\ntop-level: [a]"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 0);
  EXPECT_EQ(1u, marked.size());
  EXPECT_EQ(1u, marked.front());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));

  marked.clear();
  graph.markTransitive(marked, 0);
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
}

TEST(DependencyGraph, SimpleDependent6) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "class-members: [a, b, c]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "dynamic-lookup: [x, b, z]"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 0);
  EXPECT_EQ(1u, marked.size());
  EXPECT_EQ(1u, marked.front());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));

  marked.clear();
  graph.markTransitive(marked, 0);
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
}


template <typename Range, typename T>
static bool contains(const Range &range, const T &value) {
  return std::find(std::begin(range),std::end(range),value) != std::end(range);
}

TEST(DependencyGraph, MultipleDependentsSame) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "nominals: [a, b, c]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "member-access: [x, b, z]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(2, "member-access: [q, b, s]"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 0);
  EXPECT_EQ(2u, marked.size());
  EXPECT_TRUE(contains(marked, 1));
  EXPECT_TRUE(contains(marked, 2));
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
  EXPECT_TRUE(graph.isMarked(2));

  marked.clear();
  graph.markTransitive(marked, 0);
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
  EXPECT_TRUE(graph.isMarked(2));
}

TEST(DependencyGraph, MultipleDependentsDifferent) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "nominals: [a, b, c]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "member-access: [x, b, z]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(2, "member-access: [q, r, c]"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 0);
  EXPECT_EQ(2u, marked.size());
  EXPECT_TRUE(contains(marked, 1));
  EXPECT_TRUE(contains(marked, 2));
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
  EXPECT_TRUE(graph.isMarked(2));

  marked.clear();
  graph.markTransitive(marked, 0);
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
  EXPECT_TRUE(graph.isMarked(2));
}

TEST(DependencyGraph, ChainedDependents) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "nominals: [a, b, c]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "member-access: [x, b]\nnominals: [z]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(2, "member-access: [z]"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 0);
  EXPECT_EQ(2u, marked.size());
  EXPECT_TRUE(contains(marked, 1));
  EXPECT_TRUE(contains(marked, 2));
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
  EXPECT_TRUE(graph.isMarked(2));

  marked.clear();
  graph.markTransitive(marked, 0);
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
  EXPECT_TRUE(graph.isMarked(2));
}

TEST(DependencyGraph, MarkTwoNodes) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "nominals: [a, b]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "member-access: [a]\nnominals: [z]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(2, "member-access: [z]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(10,"nominals: [y, z]\nmember-access: [q]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(11, "member-access: [y]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(12, "member-access: [q]\nnominals: [q]"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 0);
  EXPECT_EQ(2u, marked.size());
  EXPECT_TRUE(contains(marked, 1));
  EXPECT_TRUE(contains(marked, 2));
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
  EXPECT_TRUE(graph.isMarked(2));
  EXPECT_FALSE(graph.isMarked(10));
  EXPECT_FALSE(graph.isMarked(11));
  EXPECT_FALSE(graph.isMarked(12));

  marked.clear();
  graph.markTransitive(marked, 10);
  EXPECT_EQ(1u, marked.size());
  EXPECT_EQ(11u, marked.front());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
  EXPECT_TRUE(graph.isMarked(2));
  EXPECT_TRUE(graph.isMarked(10));
  EXPECT_TRUE(graph.isMarked(11));
  EXPECT_FALSE(graph.isMarked(12));
}

TEST(DependencyGraph, MarkOneNodeTwice) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "nominals: [a]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "member-access: [a]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(2, "member-access: [b]"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 0);
  EXPECT_EQ(1u, marked.size());
  EXPECT_EQ(1u, marked.front());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
  EXPECT_FALSE(graph.isMarked(2));

  // Reload 0.
  EXPECT_EQ(graph.loadFromString(0, "nominals: [b]"),
            LoadResult::UpToDate);
  marked.clear();

  graph.markTransitive(marked, 0);
  EXPECT_EQ(1u, marked.size());
  EXPECT_EQ(2u, marked.front());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
  EXPECT_TRUE(graph.isMarked(2));
}

TEST(DependencyGraph, MarkOneNodeTwice2) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "nominals: [a]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "member-access: [a]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(2, "member-access: [b]"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 0);
  EXPECT_EQ(1u, marked.size());
  EXPECT_EQ(1u, marked.front());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
  EXPECT_FALSE(graph.isMarked(2));

  // Reload 0.
  EXPECT_EQ(graph.loadFromString(0, "nominals: [a, b]"),
            LoadResult::UpToDate);
  marked.clear();

  graph.markTransitive(marked, 0);
  EXPECT_EQ(1u, marked.size());
  EXPECT_EQ(2u, marked.front());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
  EXPECT_TRUE(graph.isMarked(2));
}

TEST(DependencyGraph, NotTransitiveOnceMarked) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "nominals: [a]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "member-access: [a]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(2, "member-access: [b]"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 1);
  EXPECT_EQ(0u, marked.size());
  EXPECT_FALSE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
  EXPECT_FALSE(graph.isMarked(2));

  // Reload 1.
  EXPECT_EQ(graph.loadFromString(1, "member-access: [a]\nnominals: [b]"),
            LoadResult::UpToDate);
  marked.clear();

  graph.markTransitive(marked, 0);
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
  EXPECT_FALSE(graph.isMarked(2));

  // Re-mark 1.
  graph.markTransitive(marked, 1);
  EXPECT_EQ(1u, marked.size());
  EXPECT_EQ(2u, marked.front());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
  EXPECT_TRUE(graph.isMarked(2));
}

TEST(DependencyGraph, DependencyLoops) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "provides: [a, b, c]\ntop-level: [a]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "provides: [x]\ntop-level: [x, b, z]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(2, "top-level: [x]"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 0);
  EXPECT_EQ(2u, marked.size());
  EXPECT_TRUE(contains(marked, 1));
  EXPECT_TRUE(contains(marked, 2));
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
  EXPECT_TRUE(graph.isMarked(2));

  marked.clear();
  graph.markTransitive(marked, 0);
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
  EXPECT_TRUE(graph.isMarked(2));
}

TEST(DependencyGraph, MarkIntransitive) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "provides: [a, b, c]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "top-level: [x, b, z]"),
            LoadResult::UpToDate);

  EXPECT_TRUE(graph.markIntransitive(0));
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_FALSE(graph.isMarked(1));

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 0);
  EXPECT_EQ(1u, marked.size());
  EXPECT_EQ(1u, marked.front());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
}

TEST(DependencyGraph, MarkIntransitiveTwice) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "provides: [a, b, c]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "top-level: [x, b, z]"),
            LoadResult::UpToDate);

  EXPECT_TRUE(graph.markIntransitive(0));
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_FALSE(graph.isMarked(1));

  EXPECT_FALSE(graph.markIntransitive(0));
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_FALSE(graph.isMarked(1));
}

TEST(DependencyGraph, MarkIntransitiveThenIndirect) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "provides: [a, b, c]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "top-level: [x, b, z]"),
            LoadResult::UpToDate);

  EXPECT_TRUE(graph.markIntransitive(1));
  EXPECT_FALSE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 0);
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
}

TEST(DependencyGraph, SimpleExternal) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "cross-module: [/foo, /bar]"),
            LoadResult::UpToDate);

  EXPECT_TRUE(contains(graph.getExternalDependencies(), "/foo"));
  EXPECT_TRUE(contains(graph.getExternalDependencies(), "/bar"));

  SmallVector<uintptr_t, 4> marked;
  graph.markExternal(marked, "/foo");
  EXPECT_EQ(1u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));

  marked.clear();
  graph.markExternal(marked, "/foo");
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
}

TEST(DependencyGraph, SimpleExternal2) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "cross-module: [/foo, /bar]"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;
  graph.markExternal(marked, "/bar");
  EXPECT_EQ(1u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));

  marked.clear();
  graph.markExternal(marked, "/bar");
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
}

TEST(DependencyGraph, ChainedExternal) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "cross-module: [/foo]\nprovides: [a]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "cross-module: [/bar]\ntop-level: [a]"),
            LoadResult::UpToDate);

  EXPECT_TRUE(contains(graph.getExternalDependencies(), "/foo"));
  EXPECT_TRUE(contains(graph.getExternalDependencies(), "/bar"));

  SmallVector<uintptr_t, 4> marked;
  graph.markExternal(marked, "/foo");
  EXPECT_EQ(2u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));

  marked.clear();
  graph.markExternal(marked, "/foo");
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
}

TEST(DependencyGraph, ChainedExternalReverse) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "cross-module: [/foo]\nprovides: [a]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "cross-module: [/bar]\ntop-level: [a]"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;
  graph.markExternal(marked, "/bar");
  EXPECT_EQ(1u, marked.size());
  EXPECT_EQ(1u, marked.front());
  EXPECT_FALSE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));

  marked.clear();
  graph.markExternal(marked, "/bar");
  EXPECT_EQ(0u, marked.size());
  EXPECT_FALSE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));

  marked.clear();
  graph.markExternal(marked, "/foo");
  EXPECT_EQ(1u, marked.size());
  EXPECT_EQ(0u, marked.front());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
}

TEST(DependencyGraph, ChainedExternalPreMarked) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "cross-module: [/foo]\nprovides: [a]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "cross-module: [/bar]\ntop-level: [a]"),
            LoadResult::UpToDate);

  graph.markIntransitive(0);

  SmallVector<uintptr_t, 4> marked;
  graph.markExternal(marked, "/foo");
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_FALSE(graph.isMarked(1));
}
