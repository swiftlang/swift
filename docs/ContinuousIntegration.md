
# Continuous Integration for Swift

**Table of Contents**

- [Introduction](#introduction)
- [Pull Request Testing](#pull-request-testing)
    - [@swift-ci](#swift-ci)
    - [Smoke Testing](#smoke-testing)
    - [Validation Testing](#validation-testing)
    - [Lint Testing](#lint-testing)
- [Cross Repository Testing](#cross-repository-testing)
- [ci.swift.org bots](#ciswiftorg-bots)

## Introduction

FIXME: FILL ME IN!

## Pull Request Testing

In order for the Swift project to be able to advance quickly, it is important that we maintain a green build [[1]](#footnote-1). In order to help maintain this green build, the Swift project heavily uses pull request (PR) testing. Specifically, an important general rule is that **all** non-trivial checkins to any Swift Project repository should should at least perform a [smoke test](#smoke-testing) if simulators will not be impacted *or* a full [validation test](#validation-testing) if simulators may be impacted. If in addition one is attempting to make a source breaking change across multiple repositories one should follow the cross repo source breaking changes workflow. We now continue by describing the Swift system for Pull Request testing, @swift-ci:

### @swift-ci

swift-ci pull request testing is triggered by writing a comment on this PR addressed to the GitHub user @swift-ci. Different tests will run depending on the specific comment that you use. The current test types are:

1. Smoke Testing
2. Validation Testing
3. Lint Testing

We describe each in detail below:

### Smoke Testing

        Platform     | Comment
        ------------ | -------------
        All supported platforms     | @swift-ci Please smoke test
        All supported platforms     | @swift-ci Please smoke test and merge
        OS X platform               | @swift-ci Please smoke test OS X platform
        Linux platform              | @swift-ci Please smoke test Linux platform

A smoke test on macOS does the following:

1. Builds the compiler incrementally.
2. Builds the standard library only for macOS. Simulator standard libraries and
   device standard libraries are not built.
3. lldb is not built.
4. The test and validation-test targets are run only for macOS. The optimized
   version of these tests are not run.

A smoke test on Linux does the following:

1. Builds the compiler incrementally.
2. Builds the standard library incrementally.
3. lldb is built incrementally.
4. The swift test and validation-test targets are run. The optimized version of these
   tests are not run.
5. lldb is tested.

### Validation Testing

        Platform     | Comment
        ------------ | -------------
        All supported platforms     | @swift-ci Please test
        All supported platforms     | @swift-ci Please test and merge
        OS X platform               | @swift-ci Please test OS X platform
        OS X platform               | @swift-ci Please benchmark
        Linux platform              | @swift-ci Please test Linux platform

### Lint Testing

        Language     | Comment
        ------------ | -------------
        Python       | @swift-ci Please Python lint

## Cross Repository Testing

Currently @swift-ci pull request testing only supports testing changes against individual repositories. This is something that will most likely be fixed in the future. But in the short term, please follow the following workflow for performing cross repository testing:

1. Make sure that all repos have been checked out:

     ./swift/utils/update-checkout --clone

2. On Darwin and Linux run:

     ./swift/utils/build-toolchain local.swift

If everything passes, a .tar.gz package file will be produced in the . directory.

3. Create a separate PR for each repository that needs to be changed. Each should reference the main Swift PR and create a reference to all of the others from the main PR.

4. Gate all commits on @swift-ci smoke test and merge. As stated above, it is important that *all* checkins perform PR testing since if breakage enters the tree PR testing becomes less effective. If you have done local testing (using build-toolchain) and have made appropriate changes to the other repositories then perform a smoke test and merge should be sufficient for correctness. This is not meant to check for correctness in your commits, but rather to be sure that no one landed changes in other repositories or in swift that cause your PR to no longer be correct. If you were unable to make workarounds to th eother repositories, this smoke test will break *after* Swift has built. Check the log to make sure that it is the expected failure for that platform/repository that coincides with the failure your PR is supposed to fix.

5. Merge all of the pull requests simultaneously.

6. Watch the public incremental build on ci.swift.org to make sure that you did not make any mistakes. It should complete within 30-40 minutes depending on what else was being committed in the mean time.

## ci.swift.org bots

FIXME: FILL ME IN!

<a name="footnote-1">[1]</a> Even though it should be without saying, the reason why having a green build is important is that:

1. A full build break can prevent other developers from testing their work.
2. A test break can make it difficult for developers to know whether or not their specific commit has broken a test, requiring them to perform an initial clean build, wasting time.
3. @swift-ci pull request testing becomes less effective since one can not perform a test and merge and one must reason about the source of a given failure.
