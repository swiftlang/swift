# request.py - sourcekitd Python Bindings -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import capi


def request_sync(req):
    ptr = capi.conf.lib.sourcekitd_send_request_sync(capi.Object(req))
    resp = capi.Response(ptr)
    if capi.conf.lib.sourcekitd_response_is_error(resp):
        raise SourceKitError(
            capi.conf.lib.sourcekitd_response_error_get_kind(resp),
            capi.conf.lib.sourcekitd_response_error_get_description(resp))
    return resp


class SourceKitError(Exception):

    def __init__(self, kind, message):
        self.kind = kind
        self.msg = message

    def __str__(self):
        return "%s (%s)" % (self.msg, self.kind)


def syntax_annotate_text(text):
    req = {
        'key.request': capi.UIdent('source.request.editor.open'),
        'key.sourcetext': text,
        'key.name': "annotate-source-text",
        'key.enablesyntaxmap': True
    }
    resp = request_sync(req)
    return resp.get_payload().to_python_object()

__all__ = [
    'request_sync',
    'syntax_annotate_text',
    'SourceKitError',
]
