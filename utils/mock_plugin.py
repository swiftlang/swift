#!/usr/bin/env python3
# ===-----------------------------------------------------------------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2023 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ===-----------------------------------------------------------------------===#
#
# Helper python module to write a mock compiler plugin for testing compiler
# plugin mechanism. Test plugins can be like
#
#   import sys
#   sys.path.append('/path/to/utils')
#   import mock_plugin
#
#   mock_plugin.TEST_SPEC = [ ... ]
#   mock_plugin.main()
#
# where 'TEST_SPEC' is a list of pairs of an expected request and the result to
# respond. For example:
#   {'expect': {'testRequest': {'params': []}},
#    'response': {'testRequestResult': [1,2,"foo"]}}
# this spec matches a request that is an object with 'testRequest' key with an
# object with 'params' key with an empty array. When the host system sends a
# request matching, this mock plugin replies to it with the 'response' object.
# ===-----------------------------------------------------------------------===#

import json
import struct
import sys

# This should be filled by the client.
TEST_SPEC = []


def match(value, expect):
    if isinstance(value, dict) and isinstance(expect, dict):
        # For dictionary, only check the expected keys. i.e. the value can
        # contain extra items.
        for key, expectVal in expect.items():
            if key not in value:
                return False
            if not match(value[key], expectVal):
                return False
        return True

    if isinstance(value, list) and isinstance(expect, list):
        # Check all elements.
        if len(value) != len(expect):
            return False
        for element, expectVal in zip(value, expect):
            if not match(element, expectVal):
                return False
        return True

    # Otherwise, just check the equality.
    return value == expect


def substitute(value, req):
    if isinstance(value, str):
        result = value.format(req=req)
        # for "={req[...]}" format, covert it to integer.
        if value.startswith('={') and value.endswith("}"):
            return int(result[1:])
        return result

    if isinstance(value, dict):
        result = {}
        for key, value in value.items():
            result[key] = substitute(value, req)
        return result

    if isinstance(value, list):
        result = []
        for value in value:
            result.append(substitute(value, req))
        return result

    return value


def handle_request(req):
    for spec in TEST_SPEC:
        if not match(req, spec['expect']):
            continue
        if spec.get('handled', False):
            raise ValueError('already handled request')
        spec['handled'] = True
        return substitute(spec['response'], req)

    raise ValueError('could not find matching request')


def main():
    # Message handling loop.
    while True:
        # Read request
        request_header = sys.stdin.buffer.read(8)
        if len(request_header) < 8:
            break
        request_size = struct.unpack('<Q', request_header)[0]
        request_data = sys.stdin.buffer.read(request_size)
        if len(request_data) != request_size:
            break
        request_object = json.loads(request_data)

        # Handle request
        response_object = handle_request(request_object)

        # Write response
        response_json = json.dumps(response_object)
        response_data = response_json.encode('utf-8')
        response_size = len(response_data)
        response_header = struct.pack('<Q', response_size)
        sys.stdout.buffer.write(response_header)
        sys.stdout.buffer.write(response_data)
        sys.stdout.buffer.flush()


if __name__ == '__main__':
    main()
