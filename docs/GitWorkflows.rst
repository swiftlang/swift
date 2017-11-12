:orphan:

.. highlight:: bash

Git Workflows
=============

Purpose
-------

Swift development has been based on SVN since its inception.  As part of the
transition to Git this document helps to address questions about how common SVN
workflows we use today translate to their Git counterparts as well as to discuss
Git workflow practices we plan on having -- at least initially -- after the Git
transition.  Notably we will follow a model where commits to trunk -- which is
the 'master' branch in Git -- has commits land (in the common case) via rebasing
instead of merging.  This model is open to evolution later, but this mimics the
workflow we have today with SVN.

SVN -> GIT Workflows
====================

The general SVN workflow consists of the following commands:

1. Checkout: This means checking out/setting up a new repository.
2. Update: Pulling the latest remote changes into a local repository.
3. Committing: Committing a change to the remote repository.
4. Reverting: Reverting a change from a remote repository.
5. Browsing: Looking at commits.

This document will show how to translate these commands to Git and additionally
how to configure Git. It assumes that one is attempting to manipulate a Git
repository via bash in a terminal. A lot of information since this is supposed
to be a short, actionable guide. For more information, please see the Git crash
course guide for SVN users at <https://git-scm.com/course/svn.html>

*NOTE* Whenever we say the Swift repository, we mean any repository in the
Swift project.

Quicksetup (TLDR)
-----------------

For those who do not want to read the full document, use the following commands
to perform a simple repo setup for the Swift repository::

  $ git config --global user.name "<My Name>"
  $ git config --global user.email "<My Email>"
  $ mkdir swift-source && cd swift-source
  $ git clone <LLVM_REPO_URL>
  $ git clone <CLANG_REPO_URL>
  $ git clone <SWIFT_REPO_URL>
  $ (cd swift && git config branch.autosetuprebase always)
  $ git clone <CMARK_REPO_URL>
  $ git clone <NINJA_REPO_URL>

Then to commit a new commit to the remote swift repository::

  $ git commit
  $ git push origin master

and to pull new commits from the remote swift repository::

  $ git pull origin master

In order to ease updating all repositories, consider using the script in
'./utils/update-checkout'. This will automate updating the repositories in the
proper way.

Preliminary
-----------

Before beginning, we need to perform some global configuration of Git. Git
includes a username/email of the committer in every commit. By default this is
the current logged in user and the hostname of the machine. This is /not/ what
one wants. We configure Git globally (i.e. across all repositories) to have our
proper name and email by running the following commands::

  $ git config --global user.name "<My Name>"
  $ git config --global user.email "<My Email>"

Checkout
--------

Normally if one wishes to checkout a repository in SVN, one would use a command
like this::

  $ SVN co <repository url> <local directory>

This would then checkout the latest revision from the repository specified by
'repository url' and place it into the directory 'local directory'. In Git,
instead of checking out the repository, one clones the repository. This is done
as follows::

  $ git clone <repository url> <local directory>

This will cause Git to clone the repository at 'repository url' and check out
the 'master' branch. The 'master' branch corresponds to 'trunk' in SVN. For more
information about branching in Git please see
<https://git-scm.com/course/svn.html#branch>

Before beginning to commit though, we /must/ perform some default configuration
of our repository to match the Swift repository default configuration by
enabling default rebasing.

Repository Configuration (Enabling Default Rebasing)
----------------------------------------------------

Once we have cloned the repository, we need to configure the repository for our
use. Specifically we want to configure the swift repository so that we rebase
whenever we update the repository (see the update section below for more
details)::

  $ git config branch.autosetuprebase always

By default when updating, Git will attempt to merge the remote changes and your
local changes. Ignoring what that sentence means, this is not an SVN-esque
model. Instead we want any local changes that we have to be applied on top of
any new remote changes. The 'branch.autosetuprebase' flag causes this to be done
automatically whenever one updates the local repository.

Update
------

In SVN, one updates your local repository by running the update command::

  $ SVN update

In Git, instead of performing SVN update, one pulls from the remote repository::

  $ git pull --rebase origin master

This will pull any new remote commits into your local repository and then replay
your current local commits on top of those new commits.

By default the '--rebase' flag is not necessary for the Swift repository because
it is configured to always rebase by setting the 'branch.autosetuprebase' flag
(see the section 'Repository Configuration (Enabling Default Rebasing)' above).

Commit
------

In SVN, committing always means submitting changes to a remote repository. In
Git, committing refers to the process of first telling Git to track a change by
staging the change and then committing all staged changes into a change in the
local repository. One can have many such commits. Then when one is ready, one
pushes the new local changes to the remote repository. We go through these steps
in more detail below:

In terms of replicating the SVN model, there are now two steps. In order to
commit changes one first stages a changed file using 'git add'::

  $ git add <path>

Then once all changes that you want to be apart of the commit have been staged,
a commit is created in the local repository via the 'commit' command::

  $ git commit

As a shortcut to commit /all/ changes to local files that are already being
tracked by Git to the local repository, you can use the '-a' command::

  $ git commit -a

In both of these cases, an editor will pop up to accept a commit message. To
specify a short commit message at the commandline, you can use the '-m' flag::

  $ git commit -m 'My great commit message.'

In order to see the diff of changes that have not been staged, run the command::

  $ git diff

To see all changes that have been staged, run the command::

  $ git diff --staged

To get a diff for a specific revision/path, perform the following command::

  $ git diff <revision> <path>

In order to get a more concise view of the files that have staged and or
unstaged changes, run the command::

  $ git status

In order to restore a file from the last revision, one uses the checkout
command::

  $ git checkout <path>

To restore a file to a specific revision, one must use a longer form of the
command::

  $ git checkout <revision> -- <path>

To unstage a file, one uses the 'reset' command::

  $ git reset <path>

This tells Git to reset '<path>' in the staging area to the top of tree commit
(which in Git is called 'HEAD'). In order to correct a mistake, you can pass the
'amend' flag to Git::

  $ git commit --amend

This will cause all staged changes to be merged into 'HEAD'. Once one has made
all the relevant commits, in order to push the changes to the remote repository
the 'push' command is used::

  $ git push origin master

If a different committer has committed changes such that there are remote
commits that are not present locally, this will fail. In order to get around
this issue, perform::

  $ git pull --rebase origin master

in order to pull the new remote commits and replay your new commits on top. Then
try to push again. See the 'Checkout' section above how to configure the local
swift repository to always rebase allowing you to drop the '--rebase' flag.

Revert
------

In SVN reverting a commit implies performing a reverse merge. In Git, this is no
longer true. Instead one now just uses the 'revert' command::

  $ git revert <revision>

This will cause Git to perform the reverse merge of that revision for you
against HEAD and bring up a message window for you to write a commit
message. This will be autofilled in with the title of the commit that is going
to be reverted and the revision number of that commit like so::

  Revert "<FIRST LINE OF REVERTED COMMITS COMMIT MSG>"

  This reverts commit <REVISION>.

One can edit this message as one sees fit. Once this has been done, the revert
will become a normal commit in your repository like any other commit. Thus to
revert the commit in the remote repository, you need to perform a Git push::

  $ git push origin master

Browsing
--------

This section explains how one can view Git changes. In order to view a history
of all changes on a branch to the beginning of time use the 'log' command::

  $ git log

This will for each commit show the following information::

  commit <REVISION>
  Author: <AUTHOR NAME> <AUTHOR EMAIL>
  Date:   <TIMESTAMP>

      <COMMIT MSG>

To see history starting at a specific commit use the following form of a Git log
command::

  $ git log <REVISION>

To see an oneline summary that includes just the title of the commit and its
hash, pass the '--oneline' command::

  $ git log --oneline

It will not show you what was actually changed in each commit. In order to see
what was actually changed in a commit, use the command 'show'::

  $ git show

This will show the aforementioned information shown by Git log, but additionally
will perform a diff against top of tree showing you the contents of the
change. To see the changes for a specific commit, pass the revision to Git
show::

  $ git show <REVISION>
