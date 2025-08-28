#!/usr/bin/env zsh
#===--- git-clang-format-all.sh --------------------------------------------===#
#
#  This source file is part of the Swift.org open source project
# 
#  Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
#  Licensed under Apache License v2.0 with Runtime Library Exception
# 
#  See https://swift.org/LICENSE.txt for license information
#  See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===------------------------------------------------------------------------===#
#
# This is a script that uses git-clang-format, git commit --fixup, and git
# rebase --autosquash to apply git-clang-format fixups automatically to all
# commits up to the parent commit of the passed in hash. The intent is that one
# can work on a branch, get it to the point one is ready to commit, run this
# script so that all git commits on ones branch are formatted and the create a
# PR.
#
#===------------------------------------------------------------------------===#

set -e

if [[ $# -ne 1 ]]; then
    echo "Usage: $0 <git-hash>"
    echo "Applies git-clang-format to each commit backwards from HEAD to the given git hash and squashes the fixes into those commits using git commit --fixup and git-rebase --autosquash"
    exit 1
fi

TARGET_HASH="$1^"

if ! git rev-parse --verify "$TARGET_HASH" >/dev/null 2>&1; then
    echo "Error: '$TARGET_HASH' is not a valid git hash"
    exit 1
fi

if ! command -v git-clang-format >/dev/null 2>&1; then
    echo "Error: git-clang-format not found in PATH"
    exit 1
fi

if ! git diff --quiet >/dev/null; then
    echo "Have changes in tree. Must not have changes in tree to run this script"
    exit 1
fi

if ! git diff --cached --quiet >/dev/null; then
    echo "Have staged changes in tree. Must not have staged changes in tree to run this script"
    exit 1
fi

echo "Formatting up to $(git log --oneline -1 ${TARGET_HASH})"

COMMITS=($(git rev-list "$TARGET_HASH"..HEAD))

if [[ ${#COMMITS[@]} -eq 0 ]]; then
    echo "No commits found between $TARGET_HASH and HEAD"
    exit 0
fi

echo "Found ${#COMMITS[@]} commits to format"

for commit in "${COMMITS[@]}"; do
    echo "Processing commit: $commit ($(git log --oneline -1 $commit))"
    
    if ! git-clang-format "${commit}^" >/dev/null ; then
        echo "    Committed fixup commit with formatting changes"
        git commit --quiet -a --fixup "${commit}"
    else
        echo "    No formatting changes needed"
    fi
done

echo "Squashing fixup commits using git rebase autosquash"
git -c sequence.editor=: rebase -i --autosquash --quiet "${TARGET_HASH}^"

echo "All commits formatted successfully"
