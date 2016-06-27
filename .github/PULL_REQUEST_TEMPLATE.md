<!-- Please complete this template before creating the pull request. -->
#### What's in this pull request?
<!-- Description about pull request. -->

#### Resolved bug number: ([SR-](https://bugs.swift.org/browse/SR-))
<!-- If this pull request resolves any bugs from Swift bug tracker -->

* * * *

<!-- This selection should only be completed by Swift admin -->
Before merging this pull request to apple/swift repository:
- [ ] Test pull request on Swift continuous integration.

<details>
  <summary>Triggering Swift CI</summary>

The swift-ci is triggered by writing a comment on this PR addressed to the GitHub user @swift-ci. Different tests will run depending on the specific comment that you use. The currently available comments are:

**Smoke Testing**

        Platform     | Comment
        ------------ | -------------
        All supported platforms     | @swift-ci Please smoke test
        All supported platforms     | @swift-ci Please smoke test and merge
        OS X platform               | @swift-ci Please smoke test OS X platform
        Linux platform              | @swift-ci Please smoke test Linux platform

**Validation Testing**

        Platform     | Comment
        ------------ | -------------
        All supported platforms     | @swift-ci Please test
        All supported platforms     | @swift-ci Please test and merge
        OS X platform               | @swift-ci Please test OS X platform
        OS X platform               | @swift-ci Please benchmark
        Linux platform              | @swift-ci Please test Linux platform


**Lint Testing**

        Language     | Comment
        ------------ | -------------
        Python       | @swift-ci Please Python lint

Note: Only members of the Apple organization can trigger swift-ci.
</details>
<!-- Thank you for your contribution to Swift! -->
