
# Continuous Integration for Swift

**Table of Contents**

- [Introduction](#introduction)
- [@swiftci Pull Request Testing](#swiftci-pull-request-testing)
    - [Smoke Testing](#smoke-testing)
    - [Validation Testing](#validation-testing)
    - [Lint Testing](#lint-testing)
- [ci.swift.org bots](#ciswiftorg-bots)

## Introduction

FIXME: FILL ME IN!

## @swiftci Pull Request Testing

The swift-ci is triggered by writing a comment on this PR addressed to the
GitHub user @swift-ci. Different tests will run depending on the specific
comment that you use. The current test types are:

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

