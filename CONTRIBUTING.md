# Contributing to Swift

Contributions to Swift are welcomed and encouraged!
For contributions to the broader Swift project, please see the
[Contributing to Swift guide](https://swift.org/contributing/).

To give clarity of what is expected of our members, Swift has adopted the code of conduct defined by the [Contributor Covenant](http://contributor-covenant.org/).
This document is used across many open source communities, and it articulates our values.
For more detail, read the project's [Code of Conduct](https://swift.org/code-of-conduct/).

It is highly recommended that you become familiar with using Swift in your own projects before contributing directly to the language itself.
We put together handy [Getting Started](https://www.swift.org/getting-started/) guide and tutorials with step-by-step instructions to get you up and running.

## Reporting Bugs

Reporting bugs is a great way for anyone to help improve Swift.
The open source Swift project uses GitHub Issues for tracking bugs.

> [!NOTE]
> If a bug can be reproduced only within an Xcode project or a playground, or if the bug is associated with an Apple NDA, please file a report to Apple’s [bug reporter](https://bugreport.apple.com/) instead.

Because Swift is under very active development, we receive a lot of bug reports.
Before opening a new issue, take a moment to [browse our existing issues](https://github.com/swiftlang/swift/issues) to reduce the chance of reporting a duplicate.

## Good First Issues

Good first issues are bugs, ideas, and tasks that are intended to be accessible for contributors that are new to working on the Swift project, and even new to the patterns and concepts behind subprojects such as the Swift compiler.
Good first issues are decorated with a corresponding label and are most easily found by visiting https://github.com/swiftlang/swift/contribute.
They are expected to be low-priority and of modest scope, and not require excessive refactoring, research, or debugging — rather, they should encourage newcomers to dip their toes in some part of Swift, learn more about it, and make a real contribution.

Anyone with [commit access](https://www.swift.org/contributing/#commit-access) and insight into a particular area is welcome and encouraged to pin down or think up good first issues.

## Contributing Code

If you are interested in:

- Contributing fixes and features to the compiler: See our
  [How to Submit Your First Pull Request guide](/docs/HowToGuides/FirstPullRequest.md).
- Building the compiler as a one-off: See our [Getting Started guide](/docs/HowToGuides/GettingStarted.md).

We also host answers to [Frequently Asked Questions](/docs/HowToGuides/FAQ.md) that may be of interest.

### Proposing changes - Swift Evolution

Shaping the future of Swift is a community effort that anyone can participate in via the [Evolution sections on the Swift forums](https://www.swift.org/community/#swift-evolution).
The [Swift evolution process](https://github.com/swiftlang/swift-evolution/blob/main/process.md) covers all changes to the Swift language and the public interface of the Swift standard library, including new language features and APIs, changes to existing language features or APIs, removal of existing features, and so on.

See the [Swift evolution review schedule](https://www.swift.org/swift-evolution) for current and upcoming proposal reviews.

### Incremental Development

The Swift project uses small, incremental changes as its preferred development model.
Sometimes these changes are small bug fixes.
Other times, these changes are small steps along the path to reaching larger stated goals.
In contrast, long-term development branches can leave the community without a voice during development. Some additional problems with long-term branches include:

- Resolving merge conflicts can take a lot of time if branch development and mainline development occur in the same pieces of code.
- People in the community tend to ignore work on branches.
- Very large changes are difficult to code review.
- Branches are not routinely tested by the continuous integration infrastructure.

To address these problems, Swift uses an incremental development style. Small changes are preferred whenever possible.
We require contributors to follow this practice when making large or otherwise invasive changes. Some tips follow:

- Large or invasive changes usually have secondary changes that must be made before the large change (for example, API cleanup or addition).
Commit these changes before the major change, independently of that work.

- If possible, decompose the remaining interrelated work into unrelated sets of changes.
Next, define the first increment and get consensus on the development goal of the change.

- Make each change in the set either stand alone (for example, to fix a bug) or part of a planned series of changes that work toward the development goal.
Explaining these relationships to the community can be helpful.

If you are interested in making a large change and feel unsure about its overall effect, please make sure to first discuss the change and reach a consensus through the [Swift Forums](https://forums.swift.org).
Then ask about the best way to go about making the change.

### Commit Messages

Although we don’t enforce a strict format for commit messages, we prefer that you follow the guidelines below, which are common among open source projects.
Following these guidelines helps with the review process, searching commit logs, and email formatting.
At a high level, the contents of the commit message should be to convey the rationale of the change, without delving into much detail.
For example, “bits were not set right” leaves the reviewer wondering about which bits and why they weren’t “right”.
In contrast, “Correctly compute ‘is dependent type’ bits in ‘Type’” conveys almost all there is to the change.

Below are some guidelines about the format of the commit message itself:

- Separate the commit message into a single-line title and a separate body that describes the change.
- Make the title concise to be easily read within a commit log and to fit in the subject line of a commit email.
- In changes that are restricted to a specific part of the code, include a [tag] at the start of the line in square brackets—for example, “[stdlib] …” or “[SILGen] …”.
This tag helps email filters and searches for post-commit reviews.
- When there is a body, separate it from the title by an empty line.
- Make body concise, while including the complete reasoning.
Unless required to understand the change, additional code examples or other details should be left to bug comments or the mailing list.
- If the commit fixes an issue in the bug tracking system, include a link to the issue in the message.
- For text formatting and spelling, follow the same rules as documentation and in-code comments—for example, the use of capitalization and periods.
- If the commit is a bug fix on top of another recently committed change, or a revert or reapply of a patch, include the Git revision number of the prior related commit, for example “Revert abcdef because it caused bug#”.



### Attribution of Changes

When contributors submit a change to a Swift subproject, after that change is approved, other developers with commit access may commit it for the author.
When doing so, it is important to retain correct attribution of the contribution. Generally speaking, Git handles attribution automatically.

We do not want the source code to be littered with random attributions like “this code written by J. Random Hacker”, which is noisy and distracting.
Do not add contributor names to the source code or documentation.

In addition, don’t commit changes authored by others unless they have submitted the change to the project or you have been authorized to submit on their behalf — for example, you work together and your company authorized you to contribute the changes.

### Code Templates

Code Templates

The license and copyright protections for Swift.org code are called out at the top of every source code file.
If you contribute a change that includes a new source file, ensure that the header is filled out appropriately.

For Swift source files the code header should look this:

```swift
//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
```

For C or C++ source or header files, the code header should look this:

```c
//===-- subfolder/Filename.h - Very brief description -----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains stuff that I am describing here in the header and will
/// be sure to keep up to date.
///
//===----------------------------------------------------------------------===//
```

The divider lines should be exactly 80 characters wide to aid in adherence to the code style guidelines.
The bottom section contains an optional description intended for generated documentation (these lines begin with `///` rather than `//`).
If there is no description, this area can be skipped.

### Code Review

The Swift project relies heavily on code review to improve software quality:

- All significant changes, by all developers, must be reviewed before they are committed to the repository.
Smaller changes (or changes where the developer owns the component) can be reviewed after being committed.
- Code reviews are conducted on GitHub.
- The developer responsible for a code change is also responsible for making all necessary review-related changes.

Code review can be an iterative process, which continues until the change is ready to be merged.
After a change is sent out for review it needs an explicit approval before it’s submitted.
Do not assume silent approval or request active objections to the patch by setting a deadline.

Sometimes code reviews will take longer than you would hope for, especially for larger features.
Here are some accepted ways to speed up review times for your patches:

- **Review other people’s changes**. If you help out, everybody will be more willing to do the same for you. Goodwill is our currency.
- **Split your change into multiple smaller changes**. The smaller your change, the higher the probability that somebody will take a quick look at it.
- **Ping the change**. If it is urgent, provide reasons why it is important to get this change landed and ping it every couple of days.
If it is not urgent, the common courtesy ping rate is one week. Remember that you’re asking for valuable time from other professional developers.

Note that anyone is welcome to review and give feedback on a change, but only people with commit access to the repository can approve it.

### Testing

Developers are required to create test cases for any bugs fixed and any new features added, and to contribute them along with the changes.

- All feature and regression test cases are added to the appropriate test directory—for example, the `swift/test` directory.
- Write test cases at the abstraction level nearest to the actual feature.
For example, if it’s a Swift language feature, write it in Swift; if it’s a SIL optimization, write it in SIL.
- Reduce test cases as much as possible, especially for regressions.
It’s unacceptable to place an entire failing program into `swift/test` because this slows down testing for all developers.
Please keep them short.

### Quality

People depend on Swift to create their production software.
This means that a bug in Swift could cause bugs in thousands, even millions of developers’ products.
Because of this, the Swift project maintains a high bar for quality.
The minimum quality standards that any change must satisfy before being committed to the main development branch include:

1. Code must compile without errors on all supported platforms and should be free of warnings on at least one platform.
2. Bug fixes and new features must include a test case to pinpoint any future regressions, or include a justification for why a test case would be impractical.
3. Code must pass the appropriate test suites—for example, the `swift/test` and `swift/validation-test` test suites in the Swift compiler.

Additionally, the committer is responsible for addressing any problems found in the future that the change may cause.
This responsibility means that you may need to update your change in order to:

- Ensure the code compiles cleanly on all primary platforms.
- Fix any correctness regressions found in other test suites.
- Fix any major performance regressions.
- Fix any performance or correctness regressions in the downstream Swift tools.
- Fix any performance or correctness regressions that result in customer code that uses Swift.
- Address any bugs that appear in the bug tracker as a result from your change.

Commits that clearly violate these quality standards may be reverted, in particular when the change blocks other developers from making progress.
The developer is welcome to recommit the change after the problem has been fixed.

### Release Branch Pull Requests

A pull request targeting a release branch (`release/x.y` or `swift/release/x.y`) cannot be merged without a GitHub approval by a corresponding branch manager.
In order for a change to be considered for inclusion in a release branch, the pull request must have:

A title starting with a designation containing the release version number of the target branch.

[This form](https://github.com/swiftlang/.github/blob/main/PULL_REQUEST_TEMPLATE/release.md?plain=1) filled out in its description.
An item that is not applicable may be left blank or completed with an indication thereof, but must not be omitted altogether.

To switch to this template when drafting a pull request in a [swiftlang](https://github.com/swiftlang) repository in a browser, append the `template=release.md` query parameter to the current URL and refresh. For example:

```diff
-https://github.com/swiftlang/swift/compare/main...my-branch?quick_pull=1
+https://github.com/swiftlang/swift/compare/main...my-branch?quick_pull=1&template=release.md
```

[Here](https://github.com/swiftlang/swift/pull/73697) is an example.

### Commit Access

Commit access is granted to contributors with a track record of submitting high-quality changes.
If you would like commit access, please send an email to [the code owners list](mailto:code-owners@forums.swift.org) with the GitHub user name that you want to use and a list of 5 non-trivial pull requests that were accepted without modifications.

Once you’ve been granted commit access, you will be able to commit to all of the GitHub repositories that host Swift.org projects.
To verify that your commit access works, please make a test commit (for example, change a comment or add a blank line). The following policies apply to users with commit access:

- You are granted commit-after-approval to all parts of Swift.
To get approval, create a pull request. When the pull request is approved, you may merge it yourself.

- You may commit an obvious change without first getting approval.
The community expects you to use good judgment.
Examples are reverting obviously broken patches, correcting code comments, and other minor changes.

Multiple violations of these policies or a single egregious violation may cause commit access to be revoked.
Even with commit access, your changes are still subject to [code review](#code-review).
Of course, you are also encouraged to review other peoples’ changes.

### Adding External Library Dependencies

There may be times where it is appropriate for one of the Swift projects (compiler, Core Libraries, etc.) to make use of libraries that provide functionality on a given platform.
Adding library dependencies impacts the portability of Swift projects, and may involve legal questions as well.

As a rule, all new library dependencies must be explicitly approved by the [Project lead](https://www.swift.org/community/#community-structure).

## LLVM and Swift

Swift is built on the [LLVM Compiler Infrastructure](http://llvm.org/).
Swift uses the LLVM Core for code generation and optimization (among other things), [Clang](http://clang.llvm.org/) for interoperability with C and Objective-C, and [LLDB](http://lldb.llvm.org/) for debugging and the REPL.

Apple maintains a fork of the [LLVM Core](https://github.com/llvm/llvm-project) source repository on GitHub at [llvm-project](https://github.com/apple/llvm-project).
This repository track upstreams LLVM development and contains additional changes for Swift.
The upstream LLVM repository are merged into the Swift-specific repository frequently.
Every attempt is made to minimize the differences between upstream LLVM and the Apple fork to only those changes specifically required for Swift.

### Where Do LLVM Changes Go?

Swift follows a policy of making a change in the most upstream repository that is feasible.
Contributions to Swift that involve Apple’s version of LLVM Project should go directly into the upstream LLVM repository unless they are specific to Swift.
For example, an improvement to LLDB’s data formatters for a Swift type belongs in the Apple LLVM Project repository, whereas a bug fix to an LLVM optimizer—even if it’s only been observed when operating on Swift-generated LLVM IR—belongs in upstream LLVM.

Commits to an upstream LLVM repository are automatically merged into the appropriate upstream branches in the corresponding Swift repository (`next`) in the [llvm-project](https://github.com/swiftlang/llvm-project).

### Swift and LLVM Developer Policies

Contributions to [llvm-project clone](https://github.com/swiftlang/llvm-project) are governed by the [LLVM Developer Policy](http://llvm.org/docs/DeveloperPolicy.html) and should follow the appropriate [licensing](http://llvm.org/docs/DeveloperPolicy.html#copyright-license-and-patents) and [coding standards](http://llvm.org/docs/CodingStandards.html).
Issues with LLVM code are tracked using the [LLVM bug database](https://github.com/llvm/llvm-project/issues).
For LLDB, changes to files with llvm.org comment headers must go to the [upstream LLDB at llvm.org](https://github.com/llvm/llvm-project/tree/main/lldb) and abide by the [LLVM Developer Policy](http://llvm.org/docs/DeveloperPolicy.html) and [LLDB coding conventions](https://llvm.org/docs/CodingStandards.html).
Contributions to the Swift-specific parts of LLDB (that is, those with a Swift.org comment header) use the [Swift license](https://www.swift.org/community/#license) but still follow the LLDB coding conventions.
