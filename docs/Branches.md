# What Builds With What?

## The Development Branches

| Swift  | LLVM*  | LLDB 
| ------ | ------ | ------
| master | stable | stable

`master` is the place for active development on Swift. If you're just working on Swift, that's where you'll spend most of your time.

LLVM repos automatically merge changes from the latest release branch (see below) into `stable`. This generally means `stable` is just an alias for the latest release branch. If you want to do Swift-related development on LLVM projects, see "The Upstream Branches" below.

LLDB is a bit more complicated. Because it's an [LLVM project][lldb] originally, it follows the LLVM branch names. However, it also contains extra support for Swift, and so it depends on being in sync with Swift as well. Work on LLDB is usually paired with work on Swift, so following Swift's branch is the way to go.

\* "LLVM" refers to several repositories from the LLVM project that do *not* depend on Swift: LLVM, Clang, and compiler-rt. LLDB is different because it imports some of Swift's own headers and thus depends on Swift.

  [release manager]: https://swift.org/blog/swift-4-0-release-process/
  [lldb]: http://lldb.llvm.org


## The Release Branches

| Swift            | LLVM*            | LLDB 
| ---------------- | ---------------- | ----------------
| swift-x.y-branch | swift-x.y-branch | swift-x.y-branch

At some point before a release, a *release branch* will be created in every repository with a name like `swift-4.0-branch`. (The actual number is chosen by Apple.) After the branch has been created, commits must make it to this branch to make it into the release. In some cases, the [release manager][] for the branch will decide to merge in all additional changes from `master`; otherwise, cherry-picking changes and making a new pull request is the way to go. If there are any "patch" releases (e.g. Swift 4.0.1), they will also come from this branch.

Note that these branches come not from the "development" branches (above), but the "upstream" branches (below). This is because they need to contain the latest changes not just from Swift, but from the LLVM projects (LLVM, Clang, compiler-rt, and LLDB) as well. For some releases, the release branch for the LLVM projects will be timed to coincide with the corresponding llvm.org release branch.


## The Upstream Branches

| Swift       | LLVM                | LLDB 
| ----------- | ------------------- | -------------------
| master-next | upstream-with-swift | upstream-with-swift

`upstream-with-swift` is a branch for LLVM that includes all changes necessary to support Swift. Changes from llvm.org's master branch are automatically merged in. Why isn't this just `stable`? Well, because LLVM changes *very* rapidly, and that wouldn't be very stable. However, we do want to make sure the Swift stuff keeps working.

If you are making changes to LLVM to support Swift, you'll probably need to work on them in `stable` to test them against Swift itself, but they should be committed to `upstream-with-swift`, and cherry-picked to the current release branch (`swift-x.y-branch`) if needed. Remember, the release branches are automerged into `stable` on a regular basis.

(If you're making changes to LLVM or LLDB that *aren't* about Swift, they should generally be made on llvm.org instead, then cherry-picked to the active release branch or `stable`.)

`master-next` is an effort to keep Swift building with the latest LLVM changes. Ideally when LLVM changes, no Swift updates are needed, but that isn't always the case. In these situations, any adjustments can go into Swift's `master-next` branch. Changes from Swift's `master` are automatically merged into `master-next` as well.

LLDB's `upstream-with-swift` has *both* aspects: changes are automatically merged in from `stable` *and* from llvm.org's master branch. Again, ideally there are no changes necessary here, but in practice LLDB may need updates to continue building against LLVM's `upstream-with-swift` and Swift's `master-next`.


# Reference

## Committing

- Swift: new commits go to `master`

- LLVM/Clang/compiler-rt: new commits go to `upstream-with-swift`

- LLDB: new commits go to `stable`

...then cherry-pick to the release branch (`swift-x.y-branch`) if necessary, following the appropriate release process. (Usually this means filling out a standard template, finding someone to review your code if that hasn't already happened, and getting approval from that repo's *release manager.)*

## Automerging

Some branches are *automerged* into other branches, to keep them in sync. This is just a process that runs `git merge` at a regular interval. These are run by Apple and are either on a "frequent" (sub-hourly) or nightly schedule.

### Swift
- `master` is automerged into `master-next`

### LLVM/Clang/compiler-rt
- `swift-x.y-branch` (the *latest* release branch) is automerged into `stable`
- llvm.org's `master` is automerged into `upstream-with-swift`
- llvm.org's release branch *may* be automerged into `swift-x.y-branch`, if they are in sync

### LLDB
- `swift-x.y-branch` (the *latest* release branch) is automerged into `stable`
- `stable` is automerged into `upstream-with-swift`
- llvm.org's `master` is *also* automerged into `upstream-with-swift`
- llvm.org's release branch *may* be automerged into `swift-x.y-branch`, if they are in sync
