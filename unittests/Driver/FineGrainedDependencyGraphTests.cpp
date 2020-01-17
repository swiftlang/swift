#include "swift/Basic/ReferenceDependencyKeys.h"
#include "swift/Driver/CoarseGrainedDependencyGraph.h"
#include "swift/Driver/FineGrainedDependencyDriverGraph.h"
#include "swift/Driver/Job.h"
#include "gtest/gtest.h"

// This file adapts the unit tests from the older, coarse-grained, dependency
// graph to the new fine-grained graph.

// \c findJobsToRecompileWhenWholeJobChanges and \c
// findExternallyDependentUntracedJobs may include jobs in their result that
// would be excluded in the coarse-grained graph. But since these will be jobs
// that have already been scheduled, downstream mechanisms will filter them out.

using namespace swift;
using namespace reference_dependency_keys;
using namespace fine_grained_dependencies;
using Job = driver::Job;

/// Initial underscore makes non-cascading, on member means private.
static ModuleDepGraph::Changes
simulateLoad(ModuleDepGraph &dg, const Job *cmd,
             llvm::StringMap<std::vector<std::string>> simpleNames,
             llvm::StringMap<std::vector<std::pair<std::string, std::string>>>
                 compoundNames = {},
             const bool includePrivateDeps = false,
             const bool hadCompilationError = false) {
  StringRef swiftDeps =
      cmd->getOutput().getAdditionalOutputForType(file_types::TY_SwiftDeps);
  assert(!swiftDeps.empty());
  StringRef interfaceHash = swiftDeps;
  auto sfdg = SourceFileDepGraph::simulateLoad(
      swiftDeps, includePrivateDeps, hadCompilationError, interfaceHash,
      simpleNames, compoundNames);

  return dg.loadFromSourceFileDepGraph(cmd, sfdg);
}

static std::string noncascading(std::string name) {
  std::string s{SourceFileDepGraph::noncascadingOrPrivatePrefix};
  s += name;
  return s;
}

LLVM_ATTRIBUTE_UNUSED
static std::string privatize(std::string name) {
  std::string s{SourceFileDepGraph::noncascadingOrPrivatePrefix};
  s += name;
  return s;
}

LLVM_ATTRIBUTE_UNUSED
static std::vector<const Job *>
printForDebugging(std::vector<const Job *> jobs) {
  llvm::errs() << "\nprintForDebugging: ";
  for (auto *j : jobs) {
    const auto swiftDeps =
        j->getOutput().getAdditionalOutputForType(file_types::TY_SwiftDeps);
    assert(!swiftDeps.empty());
    llvm::errs() << "job" << swiftDeps << ", ";
  }
  llvm::errs() << "\n";
  return jobs;
}

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
  ModuleDepGraph graph;

  EXPECT_TRUE(simulateLoad(graph, &job0, {{dependsTopLevel, {"a", "b"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job1, {{dependsNominal, {"c", "d"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job2, {{providesTopLevel, {"e", "f"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job3, {{providesNominal, {"g", "h"}}}));
  EXPECT_TRUE(
      simulateLoad(graph, &job4, {{providesDynamicLookup, {"i", "j"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job5, {{dependsDynamicLookup, {"k", "l"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job6, {},
                           {{providesMember, {{"m", "mm"}, {"n", "nn"}}}}));
  EXPECT_TRUE(simulateLoad(graph, &job7, {},
                           {{dependsMember, {{"o", "oo"}, {"p", "pp"}}}}));
  EXPECT_TRUE(
      simulateLoad(graph, &job8, {{dependsExternal, {"/foo", "/bar"}}}));

  EXPECT_TRUE(simulateLoad(graph, &job9,
                           {{providesNominal, {"a", "b"}},
                            {providesTopLevel, {"b", "c"}},
                            {dependsNominal, {"c", "d"}},
                            {dependsTopLevel, {"d", "a"}}}));
}

TEST(ModuleDepGraph, IndependentNodes) {
  ModuleDepGraph graph;

  EXPECT_TRUE(simulateLoad(
      graph, &job0, {{dependsTopLevel, {"a"}}, {providesTopLevel, {"a0"}}}));
  EXPECT_TRUE(simulateLoad(
      graph, &job1, {{dependsTopLevel, {"b"}}, {providesTopLevel, {"b0"}}}));
  EXPECT_TRUE(simulateLoad(
      graph, &job2, {{dependsTopLevel, {"c"}}, {providesTopLevel, {"c0"}}}));

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
  ModuleDepGraph graph;

  EXPECT_TRUE(simulateLoad(
      graph, &job0, {{dependsNominal, {"a"}}, {providesNominal, {"b"}}}));
  EXPECT_TRUE(simulateLoad(
      graph, &job1, {{dependsTopLevel, {"b"}}, {providesTopLevel, {"a"}}}));

  EXPECT_EQ(1u, graph.findJobsToRecompileWhenWholeJobChanges(&job0).size());
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job1));
}

TEST(ModuleDepGraph, IndependentDepKinds2) {
  ModuleDepGraph graph;

  EXPECT_TRUE(simulateLoad(
      graph, &job0, {{dependsNominal, {"a"}}, {providesNominal, {"b"}}}));
  EXPECT_TRUE(simulateLoad(
      graph, &job1, {{dependsTopLevel, {"b"}}, {providesTopLevel, {"a"}}}));

  EXPECT_EQ(1u, graph.findJobsToRecompileWhenWholeJobChanges(&job1).size());
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
}

TEST(ModuleDepGraph, IndependentMembers) {
  ModuleDepGraph graph;

  EXPECT_TRUE(
      simulateLoad(graph, &job0, {}, {{providesMember, {{"a", "aa"}}}}));
  EXPECT_TRUE(simulateLoad(graph, &job1, {}, {{dependsMember, {{"a", "bb"}}}}));
  EXPECT_TRUE(simulateLoad(graph, &job2, {}, {{dependsMember, {{"a", ""}}}}));
  EXPECT_TRUE(simulateLoad(graph, &job3, {}, {{dependsMember, {{"b", "aa"}}}}));
  EXPECT_TRUE(simulateLoad(graph, &job4, {}, {{dependsMember, {{"b", "bb"}}}}));

  EXPECT_EQ(1u, graph.findJobsToRecompileWhenWholeJobChanges(&job0).size());
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job2));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job3));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job4));
}

TEST(ModuleDepGraph, SimpleDependent) {
  ModuleDepGraph graph;

  EXPECT_TRUE(
      simulateLoad(graph, &job0, {{providesTopLevel, {"a", "b", "c"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job1, {{dependsTopLevel, {"x", "b", "z"}}}));
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
  ModuleDepGraph graph;

  EXPECT_TRUE(simulateLoad(graph, &job0, {{dependsTopLevel, {"a", "b", "c"}}}));
  EXPECT_TRUE(
      simulateLoad(graph, &job1, {{providesTopLevel, {"x", "b", "z"}}}));

  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job1);
    EXPECT_EQ(2u, jobs.size());
    EXPECT_EQ(&job0, jobs.front());
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
  ModuleDepGraph graph;

  EXPECT_TRUE(simulateLoad(graph, &job0, {{providesNominal, {"a", "b", "c"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job1, {{dependsNominal, {"x", "b", "z"}}}));

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
  ModuleDepGraph graph;

  EXPECT_TRUE(simulateLoad(
      graph, &job0, {{providesNominal, {"a"}}, {providesTopLevel, {"a"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job1, {{dependsNominal, {"a"}}}));

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
  ModuleDepGraph graph;

  EXPECT_TRUE(simulateLoad(graph, &job0, {{providesNominal, {"a"}}}));
  EXPECT_TRUE(simulateLoad(
      graph, &job1, {{dependsNominal, {"a"}}, {dependsTopLevel, {"a"}}}));

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
  ModuleDepGraph graph;

  EXPECT_TRUE(simulateLoad(
      graph, &job0, {{providesNominal, {"a"}}, {providesTopLevel, {"a"}}}));
  EXPECT_TRUE(simulateLoad(
      graph, &job1, {{dependsNominal, {"a"}}, {dependsTopLevel, {"a"}}}));

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
  ModuleDepGraph graph;

  EXPECT_TRUE(
      simulateLoad(graph, &job0, {{providesDynamicLookup, {"a", "b", "c"}}}));
  EXPECT_TRUE(
      simulateLoad(graph, &job1, {{dependsDynamicLookup, {"x", "b", "z"}}}));
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
  ModuleDepGraph graph;

  EXPECT_TRUE(simulateLoad(
      graph, &job0, {},
      {{providesMember, {{"a", "aa"}, {"b", "bb"}, {"c", "cc"}}}}));
  EXPECT_TRUE(
      simulateLoad(graph, &job1, {},
                   {{dependsMember, {{"x", "xx"}, {"b", "bb"}, {"z", "zz"}}}}));

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
  ModuleDepGraph graph;

  EXPECT_TRUE(simulateLoad(graph, &job0, {{providesNominal, {"a", "b", "c"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job1, {{dependsNominal, {"x", "b", "z"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job2, {{dependsNominal, {"q", "b", "s"}}}));

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
  ModuleDepGraph graph;

  EXPECT_TRUE(simulateLoad(graph, &job0, {{providesNominal, {"a", "b", "c"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job1, {{dependsNominal, {"x", "b", "z"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job2, {{dependsNominal, {"q", "r", "c"}}}));

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
  ModuleDepGraph graph;

  EXPECT_TRUE(simulateLoad(graph, &job0, {{providesNominal, {"a", "b", "c"}}}));
  EXPECT_TRUE(simulateLoad(
      graph, &job1, {{dependsNominal, {"x", "b"}}, {providesNominal, {"z"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job2, {{dependsNominal, {"z"}}}));

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
  ModuleDepGraph graph;

  EXPECT_TRUE(simulateLoad(graph, &job0, {{providesNominal, {"a", "b", "c"}}}));
  EXPECT_TRUE(simulateLoad(
      graph, &job1, {{dependsNominal, {"x", "b"}}, {providesNominal, {noncascading("z")}}}));
  EXPECT_TRUE(simulateLoad(graph, &job2, {{dependsNominal, {noncascading("z")}}}));

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
  ModuleDepGraph graph;

  EXPECT_TRUE(
      simulateLoad(graph, &job0, {{providesTopLevel, {"a", noncascading("b"), "c"}}}));
  EXPECT_TRUE(
      simulateLoad(graph, &job1,
                   {{dependsTopLevel, {"x", noncascading("b")}}, {providesNominal, {"z"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job2, {{dependsNominal, {"z"}}}));

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
  ModuleDepGraph graph;

  EXPECT_TRUE(simulateLoad(graph, &job0, {{providesTopLevel, {"a", "b"}}}));
  EXPECT_TRUE(simulateLoad(
      graph, &job1, {{dependsTopLevel, {"a"}}, {providesTopLevel, {"z"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job2, {{dependsTopLevel, {"z"}}}));
  EXPECT_TRUE(
      simulateLoad(graph, &job10,
                   {{providesTopLevel, {"y", "z"}}, {dependsTopLevel, {"q"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job11, {{dependsTopLevel, {"y"}}}));
  EXPECT_TRUE(simulateLoad(
      graph, &job12, {{dependsTopLevel, {"q"}}, {providesTopLevel, {"q"}}}));

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
  ModuleDepGraph graph;

  EXPECT_TRUE(simulateLoad(graph, &job0, {{providesNominal, {"a"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job1, {{dependsNominal, {"a"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job2, {{dependsNominal, {"b"}}}));

  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
    EXPECT_EQ(2u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job1));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job2));

  // Reload 0.
  EXPECT_TRUE(simulateLoad(graph, &job0, {{providesNominal, {"b"}}}));

  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
    EXPECT_EQ(2u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job2));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job2));
}

TEST(ModuleDepGraph, MarkOneNodeTwice2) {
  ModuleDepGraph graph;

  EXPECT_TRUE(simulateLoad(graph, &job0, {{providesNominal, {"a"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job1, {{dependsNominal, {"a"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job2, {{dependsNominal, {"b"}}}));

  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
    EXPECT_EQ(2u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job1));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job2));

  // Reload 0.
  EXPECT_TRUE(simulateLoad(graph, &job0, {{providesNominal, {"a", "b"}}}));

  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
    EXPECT_EQ(2u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job2));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job2));
}

TEST(ModuleDepGraph, ReloadDetectsChange) {
  ModuleDepGraph graph;

  EXPECT_TRUE(simulateLoad(graph, &job0, {{providesNominal, {"a"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job1, {{dependsNominal, {"a"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job2, {{dependsNominal, {"b"}}}));
  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job1);
    EXPECT_EQ(1u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job1));
  }
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job2));

  // Reload 1.
  EXPECT_TRUE(simulateLoad(
      graph, &job1, {{dependsNominal, {"a"}}, {providesNominal, {"b"}}}));

  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
    EXPECT_EQ(1u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job0));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job2));

  // Re-mark 1.
  {
    auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job1);
    EXPECT_EQ(2u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job1));
    EXPECT_TRUE(contains(jobs, &job2));
  }

  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job2));
}

TEST(ModuleDepGraph, NotTransitiveOnceMarked) {
  ModuleDepGraph graph;

  EXPECT_TRUE(simulateLoad(graph, &job0, {{providesNominal, {"a"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job1, {{dependsNominal, {"a"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job2, {{dependsNominal, {"b"}}}));

  {
  const auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job1);
    EXPECT_EQ(1u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job1));
  }
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job2));

  // Reload 1.
  EXPECT_TRUE(simulateLoad(graph, &job1,
  {{dependsNominal, {"a"}}, {providesNominal, {"b"}}}));

  {
  const auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
    EXPECT_EQ(1u, jobs.size());
    EXPECT_TRUE(contains(jobs, &job0));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job2));

  // Re-mark 1.
  {
    auto found = graph.findJobsToRecompileWhenWholeJobChanges(&job1);
    EXPECT_EQ(2u, found.size());
    EXPECT_TRUE(contains(found, &job1));
    EXPECT_TRUE(contains(found, &job2));
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job2));
}

TEST(ModuleDepGraph, DependencyLoops) {
  ModuleDepGraph graph;

  EXPECT_TRUE(simulateLoad(
      graph, &job0,
      {{providesTopLevel, {"a", "b", "c"}}, {dependsTopLevel, {"a"}}}));
  EXPECT_TRUE(simulateLoad(
      graph, &job1,
      {{providesTopLevel, {"x"}}, {dependsTopLevel, {"x", "b", "z"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job2, {{dependsTopLevel, {"x"}}}));

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
  ModuleDepGraph graph;

  EXPECT_TRUE(
      simulateLoad(graph, &job0, {{providesTopLevel, {"a", "b", "c"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job1, {{dependsTopLevel, {"x", "b", "z"}}}));

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
  ModuleDepGraph graph;

  EXPECT_TRUE(
      simulateLoad(graph, &job0, {{providesTopLevel, {"a", "b", "c"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job1, {{dependsTopLevel, {"x", "b", "z"}}}));

  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_FALSE(graph.haveAnyNodesBeenTraversedIn(&job1));
}

TEST(ModuleDepGraph, MarkIntransitiveThenIndirect) {
  ModuleDepGraph graph;

  EXPECT_TRUE(
      simulateLoad(graph, &job0, {{providesTopLevel, {"a", "b", "c"}}}));
  EXPECT_TRUE(simulateLoad(graph, &job1, {{dependsTopLevel, {"x", "b", "z"}}}));

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
  ModuleDepGraph graph;

  EXPECT_TRUE(
      simulateLoad(graph, &job0, {{dependsExternal, {"/foo", "/bar"}}}));

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
  ModuleDepGraph graph;

  EXPECT_TRUE(
      simulateLoad(graph, &job0, {{dependsExternal, {"/foo", "/bar"}}}));

  EXPECT_EQ(1u, graph.findExternallyDependentUntracedJobs("/bar").size());
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));

  EXPECT_EQ(0u, graph.findExternallyDependentUntracedJobs("/bar").size());
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
}

TEST(ModuleDepGraph, ChainedExternal) {
  ModuleDepGraph graph;

  EXPECT_TRUE(simulateLoad(
      graph, &job0, {{dependsExternal, {"/foo"}}, {providesTopLevel, {"a"}}}));
  EXPECT_TRUE(simulateLoad(
      graph, &job1, {{dependsExternal, {"/bar"}}, {dependsTopLevel, {"a"}}}));

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
  ModuleDepGraph graph;

  EXPECT_TRUE(simulateLoad(
      graph, &job0, {{dependsExternal, {"/foo"}}, {providesTopLevel, {"a"}}}));
  EXPECT_TRUE(simulateLoad(
      graph, &job1, {{dependsExternal, {"/bar"}}, {dependsTopLevel, {"a"}}}));

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
    EXPECT_EQ(&job0, jobs.front());
  }
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job0));
  EXPECT_TRUE(graph.haveAnyNodesBeenTraversedIn(&job1));
}

TEST(ModuleDepGraph, ChainedExternalPreMarked) {
  ModuleDepGraph graph;

  EXPECT_TRUE(simulateLoad(
      graph, &job0, {{dependsExternal, {"/foo"}}, {providesTopLevel, {"a"}}}));
  EXPECT_TRUE(simulateLoad(
      graph, &job1, {{dependsExternal, {"/bar"}}, {dependsTopLevel, {"a"}}}));

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
  ModuleDepGraph graph;
  simulateLoad(graph, &job0, {
    {providesTopLevel, {"a"}},
    {dependsTopLevel, {"b"}}
  });
  simulateLoad(graph, &job1, {
    {dependsTopLevel, {"a"}},
    {providesTopLevel, {"b"}}
  });

  const auto jobs = graph.findJobsToRecompileWhenWholeJobChanges(&job0);
  EXPECT_TRUE(contains(jobs, &job1));
}
