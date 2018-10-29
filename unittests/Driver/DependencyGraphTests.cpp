#include "swift/Basic/ReferenceDependencyKeys.h"
#include "swift/Driver/DependencyGraph.h"
#include "gtest/gtest.h"

using namespace swift;
using LoadResult = DependencyGraphImpl::LoadResult;
using namespace reference_dependency_keys;

static LoadResult loadFromString(DependencyGraph<uintptr_t> &dg, uintptr_t node,
                                 StringRef key, StringRef data) {
  return dg.loadFromString(node, key.str() + ": [" + data.str() + "]");
}

static LoadResult loadFromString(DependencyGraph<uintptr_t> &dg, uintptr_t node,
                                 StringRef key1, StringRef data1,
                                 StringRef key2, StringRef data2) {
  return dg.loadFromString(node,
                           key1.str() + ": [" + data1.str() + "]\n" +
                           key2.str() + ": [" + data2.str() + "]");
}

static LoadResult loadFromString(DependencyGraph<uintptr_t> &dg, uintptr_t node,
                                 StringRef key1, StringRef data1,
                                 StringRef key2, StringRef data2,
                                 StringRef key3, StringRef data3,
                                 StringRef key4, StringRef data4) {
  return dg.loadFromString(node,
                           key1.str() + ": [" + data1.str() + "]\n" +
                           key2.str() + ": [" + data2.str() + "]\n" +
                           key3.str() + ": [" + data3.str() + "]\n" +
                           key4.str() + ": [" + data4.str() + "]\n");
}

TEST(DependencyGraph, BasicLoad) {
  DependencyGraph<uintptr_t> graph;
  uintptr_t i = 0;

  EXPECT_EQ(loadFromString(graph, i++, dependsTopLevel, "a, b"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, i++, dependsNominal, "c, d"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, i++, providesTopLevel, "e, f"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, i++, providesNominal, "g, h"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, i++, providesDynamicLookup, "i, j"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, i++, dependsDynamicLookup, "k, l"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, i++, providesMember, "[m, mm], [n, nn]"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, i++, dependsMember, "[o, oo], [p, pp]"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, i++, dependsExternal, "/foo, /bar"),
            LoadResult::UpToDate);

  EXPECT_EQ(loadFromString(graph, i++,
                           providesNominal, "a, b",
                           providesTopLevel, "b, c",
                           dependsNominal, "c, d",
                           dependsTopLevel, "d, a"),
            LoadResult::UpToDate);
}

TEST(DependencyGraph, IndependentNodes) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(loadFromString(graph, 0,
                           dependsTopLevel, "a",
                           providesTopLevel, "a0"),
      LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 1,
                           dependsTopLevel, "b",
                           providesTopLevel, "b0"),
      LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 2,
                           dependsTopLevel, "c",
                           providesTopLevel, "c0"),
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

  EXPECT_EQ(loadFromString(graph, 0,
                           dependsNominal, "a",
                           providesNominal, "b"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 1,
                           dependsTopLevel, "b",
                           providesTopLevel, "a"),
      LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 0);
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_FALSE(graph.isMarked(1));
}

TEST(DependencyGraph, IndependentDepKinds2) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(loadFromString(graph, 0,
                           dependsNominal, "a",
                           providesNominal, "b"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 1,
                           dependsTopLevel, "b",
                           providesTopLevel, "a"),
      LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 1);
  EXPECT_EQ(0u, marked.size());
  EXPECT_FALSE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
}

TEST(DependencyGraph, IndependentMembers) {
  DependencyGraph<uintptr_t> graph;

  EXPECT_EQ(loadFromString(graph, 0, providesMember, "[a,aa]"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 1, dependsMember, "[a,bb]"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 2, dependsMember, "[a,\"\"]"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 3, dependsMember, "[b,aa]"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 4, dependsMember, "[b,bb]"),
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

  EXPECT_EQ(loadFromString(graph, 0, providesTopLevel, "a, b, c"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 1, dependsTopLevel, "x, b, z"),
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

  EXPECT_EQ(loadFromString(graph, 0, dependsTopLevel, "a, b, c"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 1, providesTopLevel, "x, b, z"),
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

  EXPECT_EQ(loadFromString(graph, 0, providesNominal, "a, b, c"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 1, dependsNominal, "x, b, z"),
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

  EXPECT_EQ(loadFromString(graph, 0,
                           providesNominal, "a",
                           providesTopLevel, "a"),
      LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 1, dependsNominal, "a"),
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

  EXPECT_EQ(loadFromString(graph, 0, providesNominal, "a"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 1,
                           dependsNominal, "a",
                           dependsTopLevel, "a"),
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

  EXPECT_EQ(loadFromString(graph, 0,
                           providesNominal, "a",
                           providesTopLevel, "a"),
      LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 1,
                           dependsNominal, "a",
                           dependsTopLevel, "a"),
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

  EXPECT_EQ(loadFromString(graph, 0, providesDynamicLookup, "a, b, c"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 1, dependsDynamicLookup, "x, b, z"),
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

  EXPECT_EQ(loadFromString(graph, 0,
                           providesMember, "[a,aa], [b,bb], [c,cc]"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 1,
                           dependsMember, "[x, xx], [b,bb], [z,zz]"),
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

  EXPECT_EQ(loadFromString(graph, 0, providesNominal, "a, b, c"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 1, dependsNominal, "x, b, z"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 2, dependsNominal, "q, b, s"),
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

  EXPECT_EQ(loadFromString(graph, 0, providesNominal, "a, b, c"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 1, dependsNominal, "x, b, z"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 2, dependsNominal, "q, r, c"),
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

  EXPECT_EQ(loadFromString(graph, 0, providesNominal, "a, b, c"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 1,
                           dependsNominal, "x, b",
                           providesNominal, "z"),
      LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 2, dependsNominal, "z"),
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

  EXPECT_EQ(loadFromString(graph, 0, providesNominal, "a, b"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 1,
                           dependsNominal, "a",
                           providesNominal, "z"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 2, dependsNominal, "z"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 10,
                           providesNominal, "y, z",
                           dependsNominal, "q"),
      LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 11, dependsNominal, "y"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 12,
                           dependsNominal, "q",
                           providesNominal, "q"),
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

  EXPECT_EQ(loadFromString(graph, 0, providesNominal, "a"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 1, dependsNominal, "a"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 2, dependsNominal, "b"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 0);
  EXPECT_EQ(1u, marked.size());
  EXPECT_EQ(1u, marked.front());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
  EXPECT_FALSE(graph.isMarked(2));

  // Reload 0.
  EXPECT_EQ(loadFromString(graph, 0, providesNominal, "b"),
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

  EXPECT_EQ(loadFromString(graph, 0, providesNominal, "a"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 1, dependsNominal, "a"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 2, dependsNominal, "b"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 0);
  EXPECT_EQ(1u, marked.size());
  EXPECT_EQ(1u, marked.front());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
  EXPECT_FALSE(graph.isMarked(2));

  // Reload 0.
  EXPECT_EQ(loadFromString(graph, 0, providesNominal, "a, b"),
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

  EXPECT_EQ(loadFromString(graph, 0, providesNominal, "a"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 1, dependsNominal, "a"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 2, dependsNominal, "b"),
            LoadResult::UpToDate);

  SmallVector<uintptr_t, 4> marked;

  graph.markTransitive(marked, 1);
  EXPECT_EQ(0u, marked.size());
  EXPECT_FALSE(graph.isMarked(0));
  EXPECT_TRUE(graph.isMarked(1));
  EXPECT_FALSE(graph.isMarked(2));

  // Reload 1.
  EXPECT_EQ(loadFromString(graph, 1,
                           dependsNominal, "a",
                           providesNominal, "b"),
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

  EXPECT_EQ(loadFromString(graph, 0,
                           providesTopLevel, "a, b, c",
                           dependsTopLevel, "a"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 1,
                           providesTopLevel, "x",
                           dependsTopLevel,
                           "x, b, z"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 2, dependsTopLevel, "x"),
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

  EXPECT_EQ(loadFromString(graph, 0, providesTopLevel, "a, b, c"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 1, dependsTopLevel, "x, b, z"),
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

  EXPECT_EQ(loadFromString(graph, 0, providesTopLevel, "a, b, c"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 1, dependsTopLevel, "x, b, z"),
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

  EXPECT_EQ(loadFromString(graph, 0, providesTopLevel, "a, b, c"),
            LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 1, dependsTopLevel, "x, b, z"),
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

  EXPECT_EQ(loadFromString(graph, 0, dependsExternal, "/foo, /bar"),
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

  EXPECT_EQ(loadFromString(graph, 0, dependsExternal, "/foo, /bar"),
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

  EXPECT_EQ(loadFromString(graph, 0,
                           dependsExternal, "/foo",
                           providesTopLevel, "a"),
      LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 1,
                           dependsExternal, "/bar",
                           dependsTopLevel, "a"),
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

  EXPECT_EQ(loadFromString(graph, 0,
                           dependsExternal, "/foo",
                           providesTopLevel, "a"),
      LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 1,
                           dependsExternal, "/bar",
                           dependsTopLevel, "a"),
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

  EXPECT_EQ(loadFromString(graph, 0,
                           dependsExternal, "/foo",
                           providesTopLevel, "a"),
      LoadResult::UpToDate);
  EXPECT_EQ(loadFromString(graph, 1,
                           dependsExternal, "/bar",
                           dependsTopLevel, "a"),
      LoadResult::UpToDate);

  graph.markIntransitive(0);

  SmallVector<uintptr_t, 4> marked;
  graph.markExternal(marked, "/foo");
  EXPECT_EQ(0u, marked.size());
  EXPECT_TRUE(graph.isMarked(0));
  EXPECT_FALSE(graph.isMarked(1));
}
