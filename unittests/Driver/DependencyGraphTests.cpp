#include "swift/Driver/DependencyGraph.h"
#include "gtest/gtest.h"

using namespace swift;

TEST(DependencyGraph, BasicLoad) {
  DependencyGraph<uintptr_t> graph;
  uintptr_t i = 0;

  EXPECT_FALSE(graph.loadFromString(i++, "top-level: [a, b]"));
  EXPECT_FALSE(graph.loadFromString(i++, "member-access: [c, d]"));
  EXPECT_FALSE(graph.loadFromString(i++, "provides: [e, f]"));
  EXPECT_FALSE(graph.loadFromString(i++, "nominals: [g, h]"));

  EXPECT_FALSE(graph.loadFromString(i++,
    "nominals: [a, b]\n"
    "provides: [b, c]\n"
    "member-access: [c, d]\n"
    "top-level: [d, a]\n"));
}

TEST(DependencyGraph, IndependentNodes) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_FALSE(graph.loadFromString(0, "top-level: [a]\nprovides: [a0]"));
  EXPECT_FALSE(graph.loadFromString(1, "top-level: [b]\nprovides: [b0]"));
  EXPECT_FALSE(graph.loadFromString(2, "top-level: [c]\nprovides: [c0]"));

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

  EXPECT_FALSE(graph.loadFromString(0, "member-access: [a]\nnominals: [b]"));
  EXPECT_FALSE(graph.loadFromString(1, "top-level: [b]\nprovides: [a]"));

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 0);
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_FALSE(graph.isMarked(1));
}

TEST(DependencyGraph, IndependentDepKinds2) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_FALSE(graph.loadFromString(0, "member-access: [a]\nnominals: [b]"));
  EXPECT_FALSE(graph.loadFromString(1, "top-level: [b]\nprovides: [a]"));

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 1);
  EXPECT_EQ(0u, marked.size());
  EXPECT_FALSE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
}

TEST(DependencyGraph, SimpleDependent) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_FALSE(graph.loadFromString(0, "provides: [a, b, c]"));
  EXPECT_FALSE(graph.loadFromString(1, "top-level: [x, b, z]"));

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

  EXPECT_FALSE(graph.loadFromString(0, "top-level: [a, b, c]"));
  EXPECT_FALSE(graph.loadFromString(1, "provides: [x, b, z]"));

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

  EXPECT_FALSE(graph.loadFromString(0, "nominals: [a, b, c]"));
  EXPECT_FALSE(graph.loadFromString(1, "member-access: [x, b, z]"));

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

  EXPECT_FALSE(graph.loadFromString(0, "nominals: [a]\nprovides: [a]"));
  EXPECT_FALSE(graph.loadFromString(1, "member-access: [a]"));

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

  EXPECT_FALSE(graph.loadFromString(0, "nominals: [a]"));
  EXPECT_FALSE(graph.loadFromString(1, "member-access: [a]\ntop-level: [a]"));

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

  EXPECT_FALSE(graph.loadFromString(0, "nominals: [a]\nprovides: [a]"));
  EXPECT_FALSE(graph.loadFromString(1, "member-access: [a]\ntop-level: [a]"));

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

  EXPECT_FALSE(graph.loadFromString(0, "nominals: [a, b, c]"));
  EXPECT_FALSE(graph.loadFromString(1, "member-access: [x, b, z]"));
  EXPECT_FALSE(graph.loadFromString(2, "member-access: [q, b, s]"));

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

  EXPECT_FALSE(graph.loadFromString(0, "nominals: [a, b, c]"));
  EXPECT_FALSE(graph.loadFromString(1, "member-access: [x, b, z]"));
  EXPECT_FALSE(graph.loadFromString(2, "member-access: [q, r, c]"));

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

  EXPECT_FALSE(graph.loadFromString(0, "nominals: [a, b, c]"));
  EXPECT_FALSE(graph.loadFromString(1, "member-access: [x, b]\nnominals: [z]"));
  EXPECT_FALSE(graph.loadFromString(2, "member-access: [z]"));

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

  EXPECT_FALSE(graph.loadFromString(0, "nominals: [a, b]"));
  EXPECT_FALSE(graph.loadFromString(1, "member-access: [a]\nnominals: [z]"));
  EXPECT_FALSE(graph.loadFromString(2, "member-access: [z]"));
  EXPECT_FALSE(graph.loadFromString(10,"nominals: [y, z]\nmember-access: [q]"));
  EXPECT_FALSE(graph.loadFromString(11, "member-access: [y]"));
  EXPECT_FALSE(graph.loadFromString(12, "member-access: [q]\nnominals: [q]"));

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

  EXPECT_FALSE(graph.loadFromString(0, "nominals: [a]"));
  EXPECT_FALSE(graph.loadFromString(1, "member-access: [a]"));
  EXPECT_FALSE(graph.loadFromString(2, "member-access: [b]"));

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 0);
  EXPECT_EQ(1u, marked.size());
  EXPECT_EQ(1u, marked.front());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
  EXPECT_FALSE(graph.isMarked(2));

  // Reload 0.
  EXPECT_FALSE(graph.loadFromString(0, "nominals: [b]"));
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

  EXPECT_FALSE(graph.loadFromString(0, "nominals: [a]"));
  EXPECT_FALSE(graph.loadFromString(1, "member-access: [a]"));
  EXPECT_FALSE(graph.loadFromString(2, "member-access: [b]"));

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 0);
  EXPECT_EQ(1u, marked.size());
  EXPECT_EQ(1u, marked.front());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
  EXPECT_FALSE(graph.isMarked(2));

  // Reload 0.
  EXPECT_FALSE(graph.loadFromString(0, "nominals: [a, b]"));
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

  EXPECT_FALSE(graph.loadFromString(0, "nominals: [a]"));
  EXPECT_FALSE(graph.loadFromString(1, "member-access: [a]"));
  EXPECT_FALSE(graph.loadFromString(2, "member-access: [b]"));

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 1);
  EXPECT_EQ(0u, marked.size());
  EXPECT_FALSE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
  EXPECT_FALSE(graph.isMarked(2));

  // Reload 1.
  EXPECT_FALSE(graph.loadFromString(1, "member-access: [a]\nnominals: [b]"));
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

  EXPECT_FALSE(graph.loadFromString(0, "provides: [a, b, c]\ntop-level: [a]"));
  EXPECT_FALSE(graph.loadFromString(1, "provides: [x]\ntop-level: [x, b, z]"));
  EXPECT_FALSE(graph.loadFromString(2, "top-level: [x]"));

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
