#include "swift/Driver/DependencyGraph.h"
#include "gtest/gtest.h"

using namespace swift;
using LoadResult = DependencyGraphImpl::LoadResult;

TEST(DependencyGraph, BasicLoad) {
  DependencyGraph<uintptr_t> graph;
  uintptr_t i = 0;

  EXPECT_EQ(graph.loadFromString(i++, "depends-top-level: [a, b]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(i++, "depends-nominal: [c, d]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(i++, "provides-top-level: [e, f]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(i++, "provides-nominal: [g, h]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(i++, "provides-dynamic-lookup: [i, j]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(i++, "depends-dynamic-lookup: [k, l]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(i++, "provides-member: [[m, mm], [n, nn]]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(i++, "depends-member: [[o, oo], [p, pp]]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(i++, "depends-external: [/foo, /bar]"),
            LoadResult::UpToDate);

  EXPECT_EQ(graph.loadFromString(i++,
                                 "provides-nominal: [a, b]\n"
                                 "provides-top-level: [b, c]\n"
                                 "depends-nominal: [c, d]\n"
                                 "depends-top-level: [d, a]\n"),
            LoadResult::UpToDate);
}

TEST(DependencyGraph, IndependentNodes) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0,
                                 "depends-top-level: [a]\n"
                                 "provides-top-level: [a0]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1,
                                 "depends-top-level: [b]\n"
                                 "provides-top-level: [b0]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(2,
                                 "depends-top-level: [c]\n"
                                 "provides-top-level: [c0]"),
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

  EXPECT_EQ(graph.loadFromString(0,
                                 "depends-nominal: [a]\n"
                                 "provides-nominal: [b]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1,
                                 "depends-top-level: [b]\n"
                                 "provides-top-level: [a]"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 0);
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_FALSE(graph.isMarked(1));
}

TEST(DependencyGraph, IndependentDepKinds2) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0,
                                 "depends-nominal: [a]\n"
                                 "provides-nominal: [b]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1,
                                 "depends-top-level: [b]\n"
                                 "provides-top-level: [a]"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 1);
  EXPECT_EQ(0u, marked.size());
  EXPECT_FALSE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
}

TEST(DependencyGraph, IndependentMembers) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "provides-member: [[a,aa]]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "depends-member: [[a,bb]]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(2, "depends-member: [[a,\"\"]]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(3, "depends-member: [[b,aa]]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(4, "depends-member: [[b,bb]]"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 0);
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_FALSE(graph.isMarked(1));
  EXPECT_FALSE(graph.isMarked(2));
  EXPECT_FALSE(graph.isMarked(3));
  EXPECT_FALSE(graph.isMarked(4));
}

TEST(DependencyGraph, SimpleDependent) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0, "provides-top-level: [a, b, c]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "depends-top-level: [x, b, z]"),
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

  EXPECT_EQ(graph.loadFromString(0, "depends-top-level: [a, b, c]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "provides-top-level: [x, b, z]"),
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

  EXPECT_EQ(graph.loadFromString(0, "provides-nominal: [a, b, c]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "depends-nominal: [x, b, z]"),
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

  EXPECT_EQ(graph.loadFromString(0,
                                 "provides-nominal: [a]\n"
                                 "provides-top-level: [a]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "depends-nominal: [a]"),
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

  EXPECT_EQ(graph.loadFromString(0, "provides-nominal: [a]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1,
                                 "depends-nominal: [a]\n"
                                 "depends-top-level: [a]"),
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

  EXPECT_EQ(graph.loadFromString(0,
                                 "provides-nominal: [a]\n"
                                 "provides-top-level: [a]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1,
                                 "depends-nominal: [a]\n"
                                 "depends-top-level: [a]"),
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

  EXPECT_EQ(graph.loadFromString(0, "provides-dynamic-lookup: [a, b, c]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "depends-dynamic-lookup: [x, b, z]"),
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


TEST(DependencyGraph, SimpleDependentMember) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(graph.loadFromString(0,
                                 "provides-member: [[a,aa], [b,bb], [c,cc]]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1,
                                 "depends-member: [[x, xx], [b,bb], [z,zz]]"),
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

  EXPECT_EQ(graph.loadFromString(0, "provides-nominal: [a, b, c]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "depends-nominal: [x, b, z]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(2, "depends-nominal: [q, b, s]"),
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

  EXPECT_EQ(graph.loadFromString(0, "provides-nominal: [a, b, c]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "depends-nominal: [x, b, z]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(2, "depends-nominal: [q, r, c]"),
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

  EXPECT_EQ(graph.loadFromString(0, "provides-nominal: [a, b, c]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1,
                                 "depends-nominal: [x, b]\n"
                                 "provides-nominal: [z]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(2, "depends-nominal: [z]"),
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

  EXPECT_EQ(graph.loadFromString(0, "provides-nominal: [a, b]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1,
                                 "depends-nominal: [a]\n"
                                 "provides-nominal: [z]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(2, "depends-nominal: [z]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(10,
                                 "provides-nominal: [y, z]\n"
                                 "depends-nominal: [q]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(11, "depends-nominal: [y]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(12,
                                 "depends-nominal: [q]\n"
                                 "provides-nominal: [q]"),
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

  EXPECT_EQ(graph.loadFromString(0, "provides-nominal: [a]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "depends-nominal: [a]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(2, "depends-nominal: [b]"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 0);
  EXPECT_EQ(1u, marked.size());
  EXPECT_EQ(1u, marked.front());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
  EXPECT_FALSE(graph.isMarked(2));

  // Reload 0.
  EXPECT_EQ(graph.loadFromString(0, "provides-nominal: [b]"),
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

  EXPECT_EQ(graph.loadFromString(0, "provides-nominal: [a]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "depends-nominal: [a]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(2, "depends-nominal: [b]"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 0);
  EXPECT_EQ(1u, marked.size());
  EXPECT_EQ(1u, marked.front());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
  EXPECT_FALSE(graph.isMarked(2));

  // Reload 0.
  EXPECT_EQ(graph.loadFromString(0, "provides-nominal: [a, b]"),
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

  EXPECT_EQ(graph.loadFromString(0, "provides-nominal: [a]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "depends-nominal: [a]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(2, "depends-nominal: [b]"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 1);
  EXPECT_EQ(0u, marked.size());
  EXPECT_FALSE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
  EXPECT_FALSE(graph.isMarked(2));

  // Reload 1.
  EXPECT_EQ(graph.loadFromString(1,
                                 "depends-nominal: [a]\n"
                                 "provides-nominal: [b]"),
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

  EXPECT_EQ(graph.loadFromString(0,
                                 "provides-top-level: [a, b, c]\n"
                                 "depends-top-level: [a]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1,
                                 "provides-top-level: [x]\n"
                                 "depends-top-level: [x, b, z]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(2, "depends-top-level: [x]"),
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

  EXPECT_EQ(graph.loadFromString(0, "provides-top-level: [a, b, c]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "depends-top-level: [x, b, z]"),
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

  EXPECT_EQ(graph.loadFromString(0, "provides-top-level: [a, b, c]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "depends-top-level: [x, b, z]"),
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

  EXPECT_EQ(graph.loadFromString(0, "provides-top-level: [a, b, c]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1, "depends-top-level: [x, b, z]"),
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

  EXPECT_EQ(graph.loadFromString(0, "depends-external: [/foo, /bar]"),
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

  EXPECT_EQ(graph.loadFromString(0, "depends-external: [/foo, /bar]"),
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

  EXPECT_EQ(graph.loadFromString(0,
                                 "depends-external: [/foo]\n"
                                 "provides-top-level: [a]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1,
                                 "depends-external: [/bar]\n"
                                 "depends-top-level: [a]"),
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

  EXPECT_EQ(graph.loadFromString(0,
                                 "depends-external: [/foo]\n"
                                 "provides-top-level: [a]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1,
                                 "depends-external: [/bar]\n"
                                 "depends-top-level: [a]"),
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

  EXPECT_EQ(graph.loadFromString(0,
                                 "depends-external: [/foo]\n"
                                 "provides-top-level: [a]"),
            LoadResult::UpToDate);
  EXPECT_EQ(graph.loadFromString(1,
                                 "depends-external: [/bar]\n"
                                 "depends-top-level: [a]"),
            LoadResult::UpToDate);

  graph.markIntransitive(0);

  SmallVector<uintptr_t, 4> marked;
  graph.markExternal(marked, "/foo");
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_FALSE(graph.isMarked(1));
}
