# ===--- test_remote_url.py -----------------------------------------------===#
#
#  This source file is part of the Swift.org open source project
#
#  Copyright (c) 2025 Apple Inc. and the Swift project authors
#  Licensed under Apache License v2.0 with Runtime Library Exception
#
#  See https:#swift.org/LICENSE.txt for license information
#  See https:#swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ===----------------------------------------------------------------------===#

import unittest

from update_checkout.update_checkout import remote_url_for_repo

CONFIG = {
    "ssh-clone-pattern": "ssh://git@example.com/org/%s.git",
    "https-clone-pattern": "https://example.com/org/%s.git",
}

SSH_ONLY_CONFIG = {
    "ssh-clone-pattern": "ssh://git@example.com/org/%s.git",
}


class RemoteURLTestCase(unittest.TestCase):
    def _url(self, remote, *, clone_with_ssh, config=CONFIG):
        return remote_url_for_repo("repo", remote, config, clone_with_ssh)

    def test_interpolated_from_id(self):
        remote = {"id": "repo"}
        self.assertEqual(
            self._url(remote, clone_with_ssh=False),
            "https://example.com/org/repo.git",
        )
        self.assertEqual(
            self._url(remote, clone_with_ssh=True),
            "ssh://git@example.com/org/repo.git",
        )

    def test_interpolated_without_https_clone_pattern(self):
        # A config that only knows how to form ssh URLs uses them even when
        # ssh was not requested.
        remote = {"id": "repo"}
        self.assertEqual(
            self._url(remote, clone_with_ssh=False, config=SSH_ONLY_CONFIG),
            "ssh://git@example.com/org/repo.git",
        )

    def test_url_override_wins_over_interpolation(self):
        remote = {"id": "repo", "url": "https://other.example/repo.git"}
        self.assertEqual(
            self._url(remote, clone_with_ssh=False),
            "https://other.example/repo.git",
        )
        self.assertEqual(
            self._url(remote, clone_with_ssh=True),
            "https://other.example/repo.git",
        )

    def test_protocol_specific_override(self):
        remote = {
            "id": "repo",
            "ssh-url": "git@other.example:org/repo.git",
            "https-url": "https://other.example/org/repo.git",
        }
        self.assertEqual(
            self._url(remote, clone_with_ssh=False),
            "https://other.example/org/repo.git",
        )
        self.assertEqual(
            self._url(remote, clone_with_ssh=True),
            "git@other.example:org/repo.git",
        )

    def test_protocol_specific_override_wins_over_url(self):
        remote = {
            "id": "repo",
            "url": "https://other.example/org/repo.git",
            "ssh-url": "git@other.example:org/repo.git",
        }
        self.assertEqual(
            self._url(remote, clone_with_ssh=True),
            "git@other.example:org/repo.git",
        )
        # No https-specific override, so the protocol-agnostic one is used.
        self.assertEqual(
            self._url(remote, clone_with_ssh=False),
            "https://other.example/org/repo.git",
        )

    def test_override_for_other_protocol_is_last_resort(self):
        # An override is always more specific than an interpolated URL, so it
        # is used even when it does not match the requested protocol.
        self.assertEqual(
            self._url(
                {"id": "repo", "https-url": "https://other.example/org/repo.git"},
                clone_with_ssh=True,
            ),
            "https://other.example/org/repo.git",
        )
        self.assertEqual(
            self._url(
                {"id": "repo", "ssh-url": "git@other.example:org/repo.git"},
                clone_with_ssh=False,
            ),
            "git@other.example:org/repo.git",
        )

    def test_protocol_specific_override_without_https_clone_pattern(self):
        # An ssh-only config picks the ssh override, since it can only clone
        # over ssh anyway.
        remote = {
            "id": "repo",
            "ssh-url": "git@other.example:org/repo.git",
            "https-url": "https://other.example/org/repo.git",
        }
        self.assertEqual(
            self._url(remote, clone_with_ssh=False, config=SSH_ONLY_CONFIG),
            "git@other.example:org/repo.git",
        )

    def test_remote_without_id_or_url(self):
        with self.assertRaises(RuntimeError):
            self._url({}, clone_with_ssh=False)
