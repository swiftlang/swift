# utils/profdata_merge/server.py
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

# This file contains the server and handler definitions that pass files to
# the merge worker processes.

import SocketServer
import thread
import logging

from main import SERVER_ADDRESS, TESTS_FINISHED_SENTINEL


class ProfdataTCPHandler(SocketServer.StreamRequestHandler):

    def report(self, msg):
        """Convenience method for reporting status from the workers."""
        logging.info("[ProfdataTCPHandler]: %s" % msg)

    def handle(self):
        """Receive a newline-separated list of filenames from a TCP connection
        and add them to the shared merge queue, where the workers will
        execute llvm-profdata merge commands."""
        data = self.rfile.read()
        self.report("received data (length %d): %s" % (len(data), repr(data)))

        # Stop once we receive the sentinel
        if data.startswith(TESTS_FINISHED_SENTINEL):
            self.report("received sentinel; killing server...")
            self.finish()
            self.connection.close()

            def kill_server(server):
                server.shutdown()

            # must be killed on another thread, or else deadlock
            thread.start_new_thread(kill_server, (self.server,))
        else:
            # Add all the files to the queue
            for f in data.splitlines():
                f = f.strip()
                if f in self.server.files_merged:
                    return
                self.server.files_merged.add(f)
                self.server.file_queue.put(f)


class ProfdataServer(SocketServer.TCPServer, object):
    def __init__(self, file_queue):
        super(ProfdataServer, self).__init__(SERVER_ADDRESS,
                                             ProfdataTCPHandler)
        self.file_queue = file_queue
        self.files_merged = set()
