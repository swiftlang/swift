
# Continuous Integration for Swift

**Table of Contents**

- [Introduction](#introduction)
- [Pull Request Testing](#pull-request-testing)
    - [Smoke Testing](#smoke-testing)
    - [Validation Testing](#validation-testing)
    - [Lint Testing](#lint-testing)
- [ci.swift.org bots](#ciswiftorg-bots)

## Introduction

FIXME: FILL ME IN!

## Pull Request Testing

In order for the Swift project to be able to advance quickly, it is important that we maintain a green build [[1]](#footnote-1). In order to help maintain this green build, the Swift project heavily uses pull request (PR) testing. Specifically, an important general rule is that *all* non-trivial checkins to any Swift Project repository should should at least perform a [smoke test](#smoke-testing) if simulators will not be impacted *or* a full [validation test](#validation-testing) if simulators may be impacted. If in addition one is attempting to make a source breaking change across multiple repositories one should follow the cross repo source breaking changes workflow. We now continue by describing the Swift system for Pull Request testing, @swift-ci:

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

## ci.swift.org bots

FIXME: FILL ME IN!

<a name="footnote-1">[1]</a> Even though it should be without saying, the reason why having a green build is important is that:

1. A full build break can prevent other developers from testing their work.
2. A test break can make it difficult for developers to know whether or not their specific commit has broken a test, requiring them to perform an initial clean build, wasting time.
3. @swift-ci pull request testing becomes less effective since one can not perform a test and merge and one must reason about the source of a given failure.
