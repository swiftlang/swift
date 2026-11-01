# What Builds With What?

## The Development Branches

| Swift  | LLVM Project
| ------ | ------------
| main   | the current `stable/*` branch (for example, `stable/23.x`)

`main` is the place for active development on Swift. If you're just working on Swift, that's where you'll spend most of your time.

In the LLVM Project repo, Swift's `main` branch is paired with a periodic stabilization branch named `stable/*`. The paired branch changes over time, so check the `main` scheme in [`update-checkout-config.json`](https://github.com/swiftlang/swift/blob/main/utils/update_checkout/update-checkout-config.json) for the current one. If you want to do Swift-related development on LLVM projects, see "The Upstream Branches" below and LLVM Project's [branching scheme](https://github.com/swiftlang/llvm-project/blob/next/apple-docs/AppleBranchingScheme.md) document.

To switch from one set of branches to another, you can use `utils/update-checkout` in the Swift repository with the `--scheme` option. You can use the Swift branch name or any of its aliases as the argument to `--scheme`: in this case, `main`.

  [release manager]: https://swift.org/blog/5-2-release-process/
  [lldb]: http://lldb.llvm.org


## The Release Branches

| Swift       | LLVM Project
| ----------- | -----------------
| release/x.y | swift/release/x.y

At some point before a release, a *release branch* will be created in every repository with a name like `release/5.3`. (The actual number is chosen by Apple.) After the branch has been created, commits must make it to this branch to make it into the release. In some cases, the [release manager][] for the branch will decide to merge in all additional changes from `main`; otherwise, cherry-picking changes and making a new pull request is the way to go. If there are any "patch" releases (e.g. Swift 5.3.1), they will also come from this branch.

Note that these branches come not from the "development" branches (above), but the "upstream" branches (below). This is because they need to contain the latest changes not just from Swift, but from the LLVM project as well. For some releases, the release branch for the LLVM project will be timed to coincide with the corresponding llvm.org release branch.


## The Upstream Branches

| Swift       | LLVM Project
| ----------- | -----------------
| next        | next

LLVM Project's `next` branch includes all changes necessary to support Swift. Changes from llvm.org's main branch are merged in by a gated automerger. Why isn't Swift's `main` paired with this branch? Well, because LLVM changes *very* rapidly, and that wouldn't be very stable. However, we do want to make sure the Swift stuff keeps working.

If you are making changes to LLVM to support Swift, you'll probably need to work on them in the `stable/*` branch paired with Swift's `main` to test them against Swift itself. Changes made to a stabilization branch are cherry-picked to `next`, and to the current release branch (`swift/release/x.y`) if needed.

(If you're making changes to LLVM Project that *aren't* about Swift, they should generally be made on llvm.org instead, then cherry-picked to the active `stable/*` or release branch.)

`next` is an effort to keep Swift building with the latest LLVM changes. Ideally when LLVM changes, no Swift updates are needed, but that isn't always the case. In these situations, any adjustments can go into Swift's `next` branch. Changes from Swift's `main` are automatically merged into `next` as well.


# Reference

## Updating

```
swift/utils/update-checkout --scheme [branch]
```

You can use any of the branch names as the argument to `--scheme`, such as `main` or `next`. See `update-checkout --help` for more options.

## Committing

- Swift: new commits go to `main`

- LLVM Project: the destination branch depends on the kind of change that must be made:

  1) LLVM Project changes that don't depend on Swift
  
     - New commits go to `main` in the upstream [llvm-project](https://github.com/llvm/llvm-project).

     - Then cherry-pick these commits to the `stable/*` branch paired with Swift's `main` in [swiftlang/llvm-project](https://github.com/swiftlang/llvm-project). Please see the [branching scheme](https://github.com/swiftlang/llvm-project/blob/next/apple-docs/AppleBranchingScheme.md) document for details.
  
     Note that **no new changes should be submitted directly to `next`** unless they are related to upstreaming or are needed because of existing differences from upstream LLVM. We are actively working on eliminating the differences from upstream LLVM.

  2) Changes that depend on Swift (this only applies to LLDB)
     - New commits go to the `stable/*` branch paired with Swift's `main`.

     - Then cherry-pick these commits to `next`.

     - If necessary, cherry-pick to the release branch (`swift/release/x.y`), following the appropriate release process. (Usually this means filling out a standard template, finding someone to review your code if that hasn't already happened, and getting approval from that repo's *release manager.)*

  In the long term we want to eliminate the differences from upstream LLVM for these changes as well, but for now there is no concrete plan.

## Automerging

Some branches are *automerged* into other branches, to keep them in sync. This is just a process that runs `git merge` at a regular interval. These are run by Apple and are either on a "frequent" (sub-hourly) or nightly schedule.

### Swift
- `main` is automerged into `next`

### LLVM Project
- llvm.org's `main` is automerged into `next`, through a gated automerger that tests changes before merging them
- llvm.org's release branch *may* be automerged into `swift/release/x.y`, if they are in sync


## Updating Branch/Tag

To update the Branch/Tag requirements, please follow these steps:

  1. Create pull requests to update the branch/tag in the following repositories:
     - in `swiftlang/swift` modify [`update-checkout-config.json`](https://github.com/swiftlang/swift/blob/main/utils/update_checkout/update-checkout-config.json)
  1. Notify @shahmishal after creating the pull requests, and @shahmishal to merge the PR once CI is updated.
