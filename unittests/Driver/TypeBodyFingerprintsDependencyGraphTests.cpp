#include "MockingFineGrainedDependencyGraphs.h"
#include "swift/Basic/ReferenceDependencyKeys.h"
#include "swift/Driver/CoarseGrainedDependencyGraph.h"
#include "swift/Driver/FineGrainedDependencyDriverGraph.h"
#include "swift/Driver/Job.h"
#include "gtest/gtest.h"

// This file adapts the unit tests from the older, coarse-grained, dependency
// graph to the new fine-grained graph.

// \c findJobsToRecompileWhenWholeJobChanges,
// \c findExternallyDependentUntracedJobs, and \c simulateReload
// may include jobs in their result that
// would be excluded in the coarse-grained graph. But since these will be jobs
// that have already been scheduled, downstream mechanisms will filter them out.

// To debug a test, create the \c ModuleDepGraph and pass true as the second
// argument to the constructor, then find the dot files in the directory
// where the tests run,
// and inspect them with, e.g. OmniGraffle.

using namespace swift;
using namespace fine_grained_dependencies;
using namespace mocking_fine_grained_dependency_graphs;
using Job = driver::Job;

static OutputFileMap OFM;

static Job
  job0(OFM, "0"),
  job1(OFM, "1"),
  job2(OFM, "2"),
  job3(OFM, "3"),
  job4(OFM, "4"),
  job5(OFM, "5"),
  job6(OFM, "6"),
  job7(OFM, "7"),
  job8(OFM, "8"),
  job9(OFM, "9"),
  job10(OFM, "10"),
  job11(OFM, "11"),
  job12(OFM, "12");

template <typename Range, typename T>
static bool contains(const Range &range, const T &value) {
  return std::find(std::begin(range), std::end(range), value) !=
         std::end(range);
}

TEST(ModuleDepGraph, BasicLoad) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0, {{NodeKind::topLevel, {"a->", "b->"}}});
  simulateLoad(graph, &job1, {{NodeKind::nominal, {"c->", "d->"}}});
  simulateLoad(graph, &job2, {{NodeKind::topLevel, {"e", "f"}}});
  simulateLoad(graph, &job3, {{NodeKind::nominal, {"g", "h"}}});
  simulateLoad(graph, &job4, {{NodeKind::dynamicLookup, {"i", "j"}}});
  simulateLoad(graph, &job5, {{NodeKind::dynamicLookup, {"k->", "l->"}}});
  simulateLoad(graph, &job6, {{NodeKind::member, {"m,mm", "n,nn"}}});
  simulateLoad(graph, &job7, {{NodeKind::member, {"o,oo->", "p,pp->"}}});
  simulateLoad(graph, &job8,
               {{NodeKind::externalDepend, {"/foo->", "/bar->"}}});

  simulateLoad(graph, &job9,
               {{NodeKind::nominal, {"a", "b", "c->", "d->"}},
                {NodeKind::topLevel, {"b", "c", "d->", "a->"}}});
}

TEST(ModuleDepGraph, IndependentNodes) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0, {{NodeKind::topLevel, {"a0", "a->"}}});
  simulateLoad(graph, &job1, {{NodeKind::topLevel, {"b0", "b->"}}});
  simulateLoad(graph, &job2, {{NodeKind::topLevel, {"c0", "c->"}}});

  EXPECT_EQ(1u, graph.findJobsToRecompileWhenWholeJobChanges(&job0).size());
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job2));

  // Mark 0 again -- should be no change.
  EXPECT_EQ(0u, graph.findJobsToRecompileWhenWholeJobChanges(&job0).size());
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job2));

  EXPECT_EQ(1u, graph.findJobsToRecompileWhenWholeJobChanges(&job2).size());
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job2));

  EXPECT_EQ(1u, graph.findJobsToRecompileWhenWholeJobChanges(&job1).size());
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job2));
}

TEST(ModuleDepGraph, IndependentDepKinds) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0, {{NodeKind::nominal, {"a", "a->"}}});
  simulateLoad(graph, &job1, {{NodeKind::topLevel, {"a", "b->"}}});

  EXPECT_EQ(1u, graph.findJobsToRecompileWhenWholeJobChanges(&job0).size());
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job1));
}

TEST(ModuleDepGraph, IndependentDepKinds2) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0, {{NodeKind::nominal, {"a->", "b"}}});
  simulateLoad(graph, &job1, {{NodeKind::topLevel, {"b->", "a"}}});

  EXPECT_EQ(1u, graph.findJobsToRecompileWhenWholeJobChanges(&job1).size());
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
}

TEST(ModuleDepGraph, IndependentMembers) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0, {{NodeKind::member, {"a,aa"}}});
  simulateLoad(graph, &job1, {{NodeKind::member, {"a,bb->"}}});
  simulateLoad(graph, &job2, {{NodeKind::potentialMember, {"a"}}});
  simulateLoad(graph, &job3, {{NodeKind::member, {"b,aa->"}}});
  simulateLoad(graph, &job4, {{NodeKind::member, {"b,bb->"}}});

  EXPECT_EQ(1u, graph.findJobsToRecompileWhenWholeJobChanges(&job0).size());
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job2));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job3));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job4));
}

TEST(ModuleDepGraph, SimpleDependent) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0, {{NodeKind::topLevel, {"a", "b", "c"}}});
  simulateLoad(graph, &job1, {{NodeKind::topLevel, {"x->", "b->", "z->"}}});
  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
    EXPECT_EQ(2u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job1));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));

  EXPECT_EQ(0u, graph.findJobsToRecompileWhenWholeJobChanges(&job0).size());
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
}

TEST(ModuleDepGraph, SimpleDependentReverse) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0, {{NodeKind::topLevel, {"a->", "b->", "c->"}}});
  simulateLoad(graph, &job1, {{NodeKind::topLevel, {"x", "b", "z"}}});

  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job1);
    EXPECT_EQ(2u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job0));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  {
    const auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
    EXPECT_EQ(1u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job0));
  }
  EXPECT_EQ(0u, graph.findJobsToRecompileWhenWholeJobChanges(&job0).size());
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
}

TEST(ModuleDepGraph, SimpleDependent2) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0, {{NodeKind::nominal, {"a", "b", "c"}}});
  simulateLoad(graph, &job1, {{NodeKind::nominal, {"x->", "b->", "z->"}}});

  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
    EXPECT_EQ(2u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job1));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));

  EXPECT_EQ(0u, graph.findJobsToRecompileWhenWholeJobChanges(&job0).size());
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
}

TEST(ModuleDepGraph, SimpleDependent3) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0,
               {{NodeKind::nominal, {"a"}}, {NodeKind::topLevel, {"a"}}});
  simulateLoad(graph, &job1, {{NodeKind::nominal, {"a->"}}});

  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
    EXPECT_EQ(2u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job1));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));

  EXPECT_EQ(0u, graph.findJobsToRecompileWhenWholeJobChanges(&job0).size());
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
}

TEST(ModuleDepGraph, SimpleDependent4) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0, {{NodeKind::nominal, {"a"}}});
  simulateLoad(graph, &job1,
               {{NodeKind::nominal, {"a->"}}, {NodeKind::topLevel, {"a->"}}});

  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
    EXPECT_EQ(2u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job1));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));

  EXPECT_EQ(0u, graph.findJobsToRecompileWhenWholeJobChanges(&job0).size());
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
}

TEST(ModuleDepGraph, SimpleDependent5) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0,
               {{NodeKind::nominal, {"a"}}, {NodeKind::topLevel, {"a"}}});
  simulateLoad(graph, &job1,
               {{NodeKind::nominal, {"a->"}}, {NodeKind::topLevel, {"a->"}}});

  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
    EXPECT_EQ(2u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job1));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));

  auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
  EXPECT_EQ(0u, graph.findJobsToRecompileWhenWholeJobChanges(&job0).size());
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
}

TEST(ModuleDepGraph, SimpleDependent6) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0, {{NodeKind::dynamicLookup, {"a", "b", "c"}}});
  simulateLoad(graph, &job1,
               {{NodeKind::dynamicLookup, {"x->", "b->", "z->"}}});
  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
    EXPECT_EQ(2u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job1));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));

  EXPECT_EQ(0u, graph.findJobsToRecompileWhenWholeJobChanges(&job0).size());
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
}

TEST(ModuleDepGraph, SimpleDependentMember) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0, {{NodeKind::member, {"a,aa", "b,bb", "c,cc"}}});
  simulateLoad(graph, &job1,
               {{NodeKind::member, {"x,xx->", "b,bb->", "z,zz->"}}});

  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
    EXPECT_EQ(2u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job1));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));

  EXPECT_EQ(0u, graph.findJobsToRecompileWhenWholeJobChanges(&job0).size());
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
}

TEST(ModuleDepGraph, MultipleDependentsSame) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0, {{NodeKind::nominal, {"a", "b", "c"}}});
  simulateLoad(graph, &job1, {{NodeKind::nominal, {"x->", "b->", "z->"}}});
  simulateLoad(graph, &job2, {{NodeKind::nominal, {"q->", "b->", "s->"}}});

  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
    EXPECT_EQ(3u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job1));
    EXPECT_TRUE(contains(jobs, &job2));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job2));

  EXPECT_EQ(0u, graph.findJobsToRecompileWhenWholeJobChanges(&job0).size());
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job2));
}

TEST(ModuleDepGraph, MultipleDependentsDifferent) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0, {{NodeKind::nominal, {"a", "b", "c"}}});
  simulateLoad(graph, &job1, {{NodeKind::nominal, {"x->", "b->", "z->"}}});
  simulateLoad(graph, &job2, {{NodeKind::nominal, {"q->", "r->", "c->"}}});

  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
    EXPECT_EQ(3u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job1));
    EXPECT_TRUE(contains(jobs, &job2));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job2));

  EXPECT_EQ(0u, graph.findJobsToRecompileWhenWholeJobChanges(&job0).size());
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job2));
}

TEST(ModuleDepGraph, ChainedDependents) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0, {{NodeKind::nominal, {"a", "b", "c"}}});
  simulateLoad(graph, &job1, {{NodeKind::nominal, {"x->", "b->", "z"}}});
  simulateLoad(graph, &job2, {{NodeKind::nominal, {"z->"}}});

  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
    EXPECT_EQ(3u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job1));
    EXPECT_TRUE(contains(jobs, &job2));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job2));

  EXPECT_EQ(0u, graph.findJobsToRecompileWhenWholeJobChanges(&job0).size());
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job2));
}

TEST(ModuleDepGraph, ChainedNoncascadingDependents) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0, {{NodeKind::nominal, {"a", "b", "c"}}});
  simulateLoad(graph, &job1, {{NodeKind::nominal, {"x->", "b->", "#z"}}});
  simulateLoad(graph, &job2, {{NodeKind::nominal, {"#z->"}}});

  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
    EXPECT_EQ(3u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job1));
    EXPECT_TRUE(contains(jobs, &job2));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job2));

  EXPECT_EQ(0u, graph.findJobsToRecompileWhenWholeJobChanges(&job0).size());
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job2));
}

TEST(ModuleDepGraph, ChainedNoncascadingDependents2) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0, {{NodeKind::topLevel, {"a", "b", "c"}}});
  simulateLoad(
      graph, &job1,
      {{NodeKind::topLevel, {"x->", "#b->"}}, {NodeKind::nominal, {"z"}}});
  simulateLoad(graph, &job2, {{NodeKind::nominal, {"z->"}}});

  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
    EXPECT_EQ(2u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job1));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job2));
}

TEST(ModuleDepGraph, MarkTwoNodes) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0, {{NodeKind::topLevel, {"a", "b"}}});
  simulateLoad(graph, &job1, {{NodeKind::topLevel, {"a->", "z"}}});
  simulateLoad(graph, &job2, {{NodeKind::topLevel, {"z->"}}});
  simulateLoad(graph, &job10, {{NodeKind::topLevel, {"y", "z", "q->"}}});
  simulateLoad(graph, &job11, {{NodeKind::topLevel, {"y->"}}});
  simulateLoad(graph, &job12, {{NodeKind::topLevel, {"q->", "q"}}});

  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
    EXPECT_EQ(3u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job1));
    EXPECT_TRUE(contains(jobs, &job2)); //?????
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job2));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job10));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job11));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job12));

  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job10);
    EXPECT_EQ(2u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job10));
    EXPECT_TRUE(contains(jobs, &job11));
    EXPECT_FALSE(contains(jobs, &job2));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job2));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job10));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job11));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job12));
}

TEST(ModuleDepGraph, MarkOneNodeTwice) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0, {{NodeKind::nominal, {"a"}}});
  simulateLoad(graph, &job1, {{NodeKind::nominal, {"a->"}}});
  simulateLoad(graph, &job2, {{NodeKind::nominal, {"b->"}}});

  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
    EXPECT_EQ(2u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job1));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job2));

  {
    auto jobs = simulateReload(graph, &job0, {{NodeKind::nominal, {"b"}}});
    EXPECT_EQ(2u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job2));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job2));
}

TEST(ModuleDepGraph, MarkOneNodeTwice2) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0, {{NodeKind::nominal, {"a"}}});
  simulateLoad(graph, &job1, {{NodeKind::nominal, {"a->"}}});
  simulateLoad(graph, &job2, {{NodeKind::nominal, {"b->"}}});

  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
    EXPECT_EQ(2u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job1));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job2));

  {
    auto jobs = simulateReload(graph, &job0, {{NodeKind::nominal, {"a", "b"}}});
    EXPECT_EQ(2u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job2));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job2));
}

TEST(ModuleDepGraph, ReloadDetectsChange) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0, {{NodeKind::nominal, {"a"}}});
  simulateLoad(graph, &job1, {{NodeKind::nominal, {"a->"}}});
  simulateLoad(graph, &job2, {{NodeKind::nominal, {"b->"}}});
  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job1);
    EXPECT_EQ(1u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job1));
  }
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job2));

  {
    auto jobs =
        simulateReload(graph, &job1, {{NodeKind::nominal, {"b", "a->"}}});
    EXPECT_EQ(2u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job1));
    EXPECT_TRUE(contains(jobs, &job2));
  }
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job2));
}

TEST(ModuleDepGraph, NotTransitiveOnceMarked) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0, {{NodeKind::nominal, {"a"}}});
  simulateLoad(graph, &job1, {{NodeKind::nominal, {"a->"}}});
  simulateLoad(graph, &job2, {{NodeKind::nominal, {"b->"}}});

  {
    const auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job1);
    EXPECT_EQ(1u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job1));
  }
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job2));

  {
    const auto jobs =
        simulateReload(graph, &job1, {{NodeKind::nominal, {"b", "a->"}}});
    EXPECT_EQ(2u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job1));
    EXPECT_TRUE(contains(jobs, &job2));
  }
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job2));
}

TEST(ModuleDepGraph, DependencyLoops) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0, {{NodeKind::topLevel, {"a", "b", "c", "a->"}}});
  simulateLoad(graph, &job1,
               {{NodeKind::topLevel, {"x", "x->", "b->", "z->"}}});
  simulateLoad(graph, &job2, {{NodeKind::topLevel, {"x->"}}});

  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
    EXPECT_EQ(3u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job1));
    EXPECT_TRUE(contains(jobs, &job2));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job2));

  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
    EXPECT_EQ(0u, jobs.size());
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job2));
}

TEST(ModuleDepGraph, MarkIntransitive) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0, {{NodeKind::topLevel, {"a", "b", "c"}}});
  simulateLoad(graph, &job1, {{NodeKind::topLevel, {"x->", "b->", "z->"}}});

  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job1));

  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
    EXPECT_EQ(2u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job1));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
}

TEST(ModuleDepGraph, MarkIntransitiveTwice) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0, {{NodeKind::topLevel, {"a", "b", "c"}}});
  simulateLoad(graph, &job1, {{NodeKind::topLevel, {"x->", "b->", "z->"}}});

  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job1));
}

TEST(ModuleDepGraph, MarkIntransitiveThenIndirect) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0, {{NodeKind::topLevel, {"a", "b", "c"}}});
  simulateLoad(graph, &job1, {{NodeKind::topLevel, {"x->", "b->", "z->"}}});

  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job1));

  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
    EXPECT_EQ(2u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job0));
    EXPECT_TRUE(contains(jobs, &job1));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
}

TEST(ModuleDepGraph, SimpleExternal) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0,
               {{NodeKind::externalDepend, {"/foo->", "/bar->"}}});

  EXPECT_TRUE(contains(graph.getExternalDependencies(), "/foo"));
  EXPECT_TRUE(contains(graph.getExternalDependencies(), "/bar"));

  {
    auto jobs = graph.findExternallyDependentUntracedJobs("/foo");
    EXPECT_EQ(jobs.size(), 1u);
    EXPECT_TRUE(contains(jobs, &job0));
  }

  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));

  EXPECT_EQ(0u, graph.findExternallyDependentUntracedJobs("/foo").size());
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
}

TEST(ModuleDepGraph, SimpleExternal2) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0,
               {{NodeKind::externalDepend, {"/foo->", "/bar->"}}});

  EXPECT_EQ(1u, graph.findExternallyDependentUntracedJobs("/bar").size());
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));

  EXPECT_EQ(0u, graph.findExternallyDependentUntracedJobs("/bar").size());
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
}

TEST(ModuleDepGraph, ChainedExternal) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(
      graph, &job0,
      {{NodeKind::externalDepend, {"/foo->"}}, {NodeKind::topLevel, {"a"}}});
  simulateLoad(
      graph, &job1,
      {{NodeKind::externalDepend, {"/bar->"}}, {NodeKind::topLevel, {"a->"}}});

  EXPECT_TRUE(contains(graph.getExternalDependencies(), "/foo"));
  EXPECT_TRUE(contains(graph.getExternalDependencies(), "/bar"));

  {
    auto jobs = graph.findExternallyDependentUntracedJobs("/foo");
    EXPECT_EQ(jobs.size(), 2u);
    EXPECT_TRUE(contains(jobs, &job0));
    EXPECT_TRUE(contains(jobs, &job1));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));

  {
    auto jobs = graph.findExternallyDependentUntracedJobs("/foo");
    EXPECT_EQ(jobs.size(), 0u);
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
}

TEST(ModuleDepGraph, ChainedExternalReverse) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(
      graph, &job0,
      {{NodeKind::externalDepend, {"/foo->"}}, {NodeKind::topLevel, {"a"}}});
  simulateLoad(
      graph, &job1,
      {{NodeKind::externalDepend, {"/bar->"}}, {NodeKind::topLevel, {"a->"}}});

  {
    auto jobs = graph.findExternallyDependentUntracedJobs("/bar");
    EXPECT_EQ(1u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job1));
  }
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));

  EXPECT_EQ(0u, graph.findExternallyDependentUntracedJobs("/bar").size());
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));

  {
    auto jobs = graph.findExternallyDependentUntracedJobs("/foo");
    EXPECT_EQ(1u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job0));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
}

TEST(ModuleDepGraph, ChainedExternalPreMarked) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(
      graph, &job0,
      {{NodeKind::externalDepend, {"/foo->"}}, {NodeKind::topLevel, {"a"}}});
  simulateLoad(
      graph, &job1,
      {{NodeKind::externalDepend, {"/bar->"}}, {NodeKind::topLevel, {"a->"}}});

  {
    auto jobs = graph.findExternallyDependentUntracedJobs("/foo");
    EXPECT_EQ(2u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job0));
    EXPECT_TRUE(contains(jobs, &job1));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
}

TEST(ModuleDepGraph, MutualInterfaceHash) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);
  simulateLoad(graph, &job0, {{NodeKind::topLevel, {"a", "b->"}}});
  simulateLoad(graph, &job1, {{NodeKind::topLevel, {"a->", "b"}}});

  const auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
  EXPECT_TRUE(contains(jobs, &job1));
}

TEST(ModuleDepGraph, EnabledTypeBodyFingerprints) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  simulateLoad(graph, &job0, {{NodeKind::nominal, {"B2->"}}});
  simulateLoad(graph, &job1, {{NodeKind::nominal, {"B1", "B2"}}});
  simulateLoad(graph, &job2, {{NodeKind::nominal, {"B1->"}}});

  {
    const auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job1);
    EXPECT_EQ(3u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job0));
    EXPECT_TRUE(contains(jobs, &job1));
    EXPECT_TRUE(contains(jobs, &job2));
  }
}

TEST(ModuleDepGraph, BaselineForPrintsAndCrossType) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  // Because when A1 changes, B1 and not B2 is affected, only jobs1 and job2
  // should be recompiled, except type fingerprints is off!

  simulateLoad(graph, &job0, {{NodeKind::nominal, {"A1", "A2"}}});
  simulateLoad(graph, &job1, {{NodeKind::nominal, {"B1", "A1->"}}});
  simulateLoad(graph, &job2, {{NodeKind::nominal, {"C1", "A2->"}}});
  simulateLoad(graph, &job3, {{NodeKind::nominal, {"D1"}}});

  {
    const auto jobs = simulateReload(
        graph, &job0, {{NodeKind::nominal, {"A1", "A2"}}}, "changed");
    EXPECT_EQ(3u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job0));
    EXPECT_TRUE(contains(jobs, &job1));
    EXPECT_TRUE(contains(jobs, &job2));
    EXPECT_FALSE(contains(jobs, &job3));
  }
}

TEST(ModuleDepGraph, LoadPassesWithFingerprint) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);
  EXPECT_TRUE(
      getChangesForSimulatedLoad(graph, &job0, {{NodeKind::nominal, {"A@1"}}}));
}

TEST(ModuleDepGraph, UseFingerprints) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);

  // Because when A1 changes, B1 and not B2 is affected, only jobs1 and job2
  // should be recompiled, except type fingerprints is off!
  // Include a dependency on A1, to ensure it does not muck things up.

  simulateLoad(graph, &job0, {{NodeKind::nominal, {"A1@1", "A2@2", "A1->"}}});
  simulateLoad(graph, &job1, {{NodeKind::nominal, {"B1", "A1->"}}});
  simulateLoad(graph, &job2, {{NodeKind::nominal, {"C1", "A2->"}}});
  simulateLoad(graph, &job3, {{NodeKind::nominal, {"D1"}}});

  {
    const auto jobs =
        simulateReload(graph, &job0, {{NodeKind::nominal, {"A1@11", "A2@2"}}});
    EXPECT_EQ(3u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job0));
    EXPECT_TRUE(contains(jobs, &job1));
    EXPECT_TRUE(contains(jobs, &job2));
    EXPECT_FALSE(contains(jobs, &job3));
  }
}

TEST(ModuleDepGraph, CrossTypeDependencyBaseline) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);
  simulateLoad(graph, &job0, {{NodeKind::nominal, {"A"}}});
  simulateLoad(graph, &job1, {{NodeKind::nominal, {"B", "C", "A->"}}});
  simulateLoad(graph, &job2, {{NodeKind::nominal, {"B->"}}});
  simulateLoad(graph, &job3, {{NodeKind::nominal, {"C->"}}});

  const auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
  EXPECT_TRUE(contains(jobs, &job0));
  EXPECT_TRUE(contains(jobs, &job1));
  EXPECT_TRUE(contains(jobs, &job2));
  EXPECT_TRUE(contains(jobs, &job3));
}

TEST(ModuleDepGraph, CrossTypeDependency) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);
  // Because of the cross-type dependency, A->B,
  // when A changes, only B is dirtied in job1.

  simulateLoad(graph, &job0, {{NodeKind::nominal, {"A"}}});
  simulateLoad(graph, &job1, {{NodeKind::nominal, {"B", "C", "A->B"}}});
  simulateLoad(graph, &job2, {{NodeKind::nominal, {"B->"}}});
  simulateLoad(graph, &job3, {{NodeKind::nominal, {"C->"}}});

  const auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
  EXPECT_TRUE(contains(jobs, &job0));
  EXPECT_TRUE(contains(jobs, &job1));
  EXPECT_TRUE(contains(jobs, &job2));
  EXPECT_FALSE(contains(jobs, &job3));
}

TEST(ModuleDepGraph, CrossTypeDependencyBaselineWithFingerprints) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);
  simulateLoad(graph, &job0, {{NodeKind::nominal, {"A1@1", "A2@2"}}});
  simulateLoad(graph, &job1, {{NodeKind::nominal, {"B1", "C1", "A1->"}}});
  simulateLoad(graph, &job2, {{NodeKind::nominal, {"B1->"}}});
  simulateLoad(graph, &job3, {{NodeKind::nominal, {"C1->"}}});
  simulateLoad(graph, &job4, {{NodeKind::nominal, {"B2", "C2", "A2->"}}});
  simulateLoad(graph, &job5, {{NodeKind::nominal, {"B2->"}}});
  simulateLoad(graph, &job6, {{NodeKind::nominal, {"C2->"}}});

  const auto jobs =
      simulateReload(graph, &job0, {{NodeKind::nominal, {"A1@11", "A2@2"}}});
  EXPECT_TRUE(contains(jobs, &job0));
  EXPECT_TRUE(contains(jobs, &job1));
  EXPECT_TRUE(contains(jobs, &job2));
  EXPECT_TRUE(contains(jobs, &job3));
  EXPECT_FALSE(contains(jobs, &job4));
  EXPECT_FALSE(contains(jobs, &job5));
  EXPECT_FALSE(contains(jobs, &job6));
}

TEST(ModuleDepGraph, CrossTypeDependencyWithFingerprints) {
  ModuleDepGraph graph(/*EnableTypeFingerprints=*/true);
  // Because of the cross-type dependency, A->B,
  // when A changes, only B is dirtied in job1.

  simulateLoad(graph, &job0, {{NodeKind::nominal, {"A1@1", "A2@2"}}});
  simulateLoad(graph, &job1, {{NodeKind::nominal, {"B1", "C1", "A1->B1"}}});
  simulateLoad(graph, &job2, {{NodeKind::nominal, {"B1->"}}});
  simulateLoad(graph, &job3, {{NodeKind::nominal, {"C1->"}}});
  simulateLoad(graph, &job4, {{NodeKind::nominal, {"B2", "C2", "A2->B2"}}});
  simulateLoad(graph, &job5, {{NodeKind::nominal, {"B2->"}}});
  simulateLoad(graph, &job6, {{NodeKind::nominal, {"C2->"}}});

  const auto jobs =
      simulateReload(graph, &job0, {{NodeKind::nominal, {"A1@11", "A2@2"}}});
  EXPECT_TRUE(contains(jobs, &job0));
  EXPECT_TRUE(contains(jobs, &job1));
  EXPECT_TRUE(contains(jobs, &job2));
  EXPECT_FALSE(contains(jobs, &job3));
  EXPECT_FALSE(contains(jobs, &job4));
  EXPECT_FALSE(contains(jobs, &job5));
  EXPECT_FALSE(contains(jobs, &job6));
}
