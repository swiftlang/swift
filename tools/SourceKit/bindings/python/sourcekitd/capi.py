# capi.py - sourcekitd Python Bindings -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

from ctypes import (
    CFUNCTYPE,
    POINTER,
    Structure,
    addressof,
    c_bool,
    c_char_p,
    c_int,
    c_int64,
    c_size_t,
    c_uint64,
    c_void_p,
    cdll,
    py_object,
    string_at,
)

# ctypes doesn't implicitly convert c_void_p to the appropriate wrapper
# object. This is a problem, because it means that from_parameter will see an
# integer and pass the wrong value on platforms where int != void*. Work around
# this by marshalling object arguments as void**.
c_object_p = POINTER(c_void_p)

callbacks = {}

# Structures and Utility Classes


class CachedProperty(object):
    """Decorator that lazy-loads the value of a property.

    The first time the property is accessed, the original property function is
    executed. The value it returns is set as the new value of that instance's
    property, replacing the original method.
    """

    def __init__(self, wrapped):
        self.wrapped = wrapped
        try:
            self.__doc__ = wrapped.__doc__
        except AttributeError:
            pass

    def __get__(self, instance, instance_type=None):
        if instance is None:
            return self

        value = self.wrapped(instance)
        setattr(instance, self.wrapped.__name__, value)

        return value


class Object(object):

    def __init__(self, obj):
        if isinstance(obj, Object):
            self._obj = conf.lib.sourcekitd_request_retain(obj)
        elif isinstance(obj, (int, long, bool)):
            self._obj = conf.lib.sourcekitd_request_int64_create(obj)
        elif isinstance(obj, str):
            self._obj = conf.lib.sourcekitd_request_string_create(obj)
        elif isinstance(obj, UIdent):
            self._obj = conf.lib.sourcekitd_request_uid_create(obj)
        elif isinstance(obj, dict):
            self._obj = conf.lib.sourcekitd_request_dictionary_create(
                POINTER(c_void_p)(), POINTER(c_void_p)(), 0)
            self._as_parameter_ = self._obj
            for k, v in obj.iteritems():
                conf.lib.sourcekitd_request_dictionary_set_value(
                    self,
                    UIdent(k), Object(v))
        elif isinstance(obj, (list, tuple)):
            self._obj = conf.lib.sourcekitd_request_array_create(
                POINTER(c_void_p)(), 0)
            self._as_parameter_ = self._obj
            for v in obj:
                conf.lib.sourcekitd_request_array_set_value(
                    self, -1, Object(v))
        else:
            raise ValueError("wrong init parameter (%s)" % type(obj))
        self._as_parameter_ = self._obj

    def from_param(self):
        return self._as_parameter_

    def __del__(self):
        if self._obj:
            conf.lib.sourcekitd_request_release(self)

    def __repr__(self):
        ptr = conf.lib.sourcekitd_request_description_copy(self)
        s = string_at(ptr)
        conf.free(ptr)
        return s


class Response(object):

    def __init__(self, obj):
        if isinstance(obj, c_object_p):
            self._obj = self._as_parameter_ = obj
        else:
            raise ValueError("wrong init parameter (%s)" % type(obj))

    def get_payload(self):
        return conf.lib.sourcekitd_response_get_value(self)

    def from_param(self):
        return self._as_parameter_

    def __del__(self):
        if self._obj:
            conf.lib.sourcekitd_response_dispose(self)

    def __repr__(self):
        ptr = conf.lib.sourcekitd_response_description_copy(self)
        s = string_at(ptr)
        conf.free(ptr)
        return s


class UIdent(object):

    def __init__(self, obj):
        if isinstance(obj, c_object_p):
            self._obj = obj
        elif isinstance(obj, UIdent):
            self._obj = obj._obj
        elif isinstance(obj, str):
            self._obj = conf.lib.sourcekitd_uid_get_from_cstr(obj)
        else:
            raise ValueError("wrong init parameter (%s)" % type(obj))
        self._as_parameter_ = self._obj

    def __str__(self):
        return conf.lib.sourcekitd_uid_get_string_ptr(self)

    def from_param(self):
        return self._as_parameter_

    def __repr__(self):
        return "UIdent('%s')" % self.__str__()

    def _ptr(self):
        return addressof(self._obj.contents)

    def __eq__(self, other):
        return self._ptr() == UIdent(other)._ptr()

    def __ne__(self, other):
        return self._ptr() != UIdent(other)._ptr()

    def __hash__(self):
        return hash(self._ptr())


class ErrorKind(object):
    """Describes the kind of type."""

    # The unique kind objects, indexed by id.
    _kinds = []
    _name_map = None

    def __init__(self, value):
        if value >= len(ErrorKind._kinds):
            ErrorKind._kinds += [None] * (value - len(ErrorKind._kinds) + 1)
        if ErrorKind._kinds[value] is not None:
            raise ValueError('ErrorKind already loaded')
        self.value = value
        ErrorKind._kinds[value] = self
        ErrorKind._name_map = None

    def from_param(self):
        return self.value

    @property
    def name(self):
        """Get the enumeration name of this error kind."""
        if self._name_map is None:
            self._name_map = {}
            for key, value in ErrorKind.__dict__.items():
                if isinstance(value, ErrorKind):
                    self._name_map[value] = key
        return self._name_map[self]

    @staticmethod
    def from_id(id):
        if id >= len(ErrorKind._kinds) or ErrorKind._kinds[id] is None:
            raise ValueError('Unknown type kind {}'.format(id))
        return ErrorKind._kinds[id]

    def __repr__(self):
        return 'ErrorKind.%s' % (self.name,)

ErrorKind.CONNECTION_INTERRUPTED = ErrorKind(1)
ErrorKind.REQUEST_INVALID = ErrorKind(2)
ErrorKind.REQUEST_FAILED = ErrorKind(3)
ErrorKind.REQUEST_CANCELLED = ErrorKind(4)


class Variant(Structure):
    _fields_ = [
        ("data", c_uint64 * 3)]

    def to_python_object(self):
        var_ty = conf.lib.sourcekitd_variant_get_type(self)
        if var_ty == VariantType.NULL:
            return None
        elif var_ty == VariantType.DICTIONARY:
            return self.to_python_dictionary()
        elif var_ty == VariantType.ARRAY:
            return self.to_python_array()
        elif var_ty == VariantType.INT64:
            return conf.lib.sourcekitd_variant_int64_get_value(self)
        elif var_ty == VariantType.STRING:
            return conf.lib.sourcekitd_variant_string_get_ptr(self)
        elif var_ty == VariantType.UID:
            return UIdent(conf.lib.sourcekitd_variant_uid_get_value(self))
        else:
            assert(var_ty == VariantType.BOOL)
            return conf.lib.sourcekitd_variant_bool_get_value(self)

    def to_python_array(self):
        def applier(index, value, arr):
            arr.append(value.to_python_object())
            # continue
            return 1
        arr = []
        conf.lib.sourcekitd_variant_array_apply_f(
            self, callbacks['array_applier'](applier), arr)
        return arr

    def to_python_dictionary(self):
        def applier(cobj, value, d):
            d[str(UIdent(cobj))] = value.to_python_object()
            # continue
            return 1
        d = {}
        conf.lib.sourcekitd_variant_dictionary_apply_f(
            self, callbacks['dictionary_applier'](applier), d)
        return d


class VariantType(object):
    """Describes the kind of type."""

    # The unique kind objects, indexed by id.
    _kinds = []
    _name_map = None

    def __init__(self, value):
        if value >= len(VariantType._kinds):
            VariantType._kinds += [None] * \
                (value - len(VariantType._kinds) + 1)
        if VariantType._kinds[value] is not None:
            raise ValueError('VariantType already loaded')
        self.value = value
        VariantType._kinds[value] = self
        VariantType._name_map = None

    def from_param(self):
        return self.value

    @property
    def name(self):
        """Get the enumeration name of this variant type."""
        if self._name_map is None:
            self._name_map = {}
            for key, value in VariantType.__dict__.items():
                if isinstance(value, VariantType):
                    self._name_map[value] = key
        return self._name_map[self]

    @staticmethod
    def from_id(id):
        if id >= len(VariantType._kinds) or VariantType._kinds[id] is None:
            raise ValueError('Unknown type kind {}'.format(id))
        return VariantType._kinds[id]

    def __repr__(self):
        return 'VariantType.%s' % (self.name,)

VariantType.NULL = VariantType(0)
VariantType.DICTIONARY = VariantType(1)
VariantType.ARRAY = VariantType(2)
VariantType.INT64 = VariantType(3)
VariantType.STRING = VariantType(4)
VariantType.UID = VariantType(5)
VariantType.BOOL = VariantType(6)

# Now comes the plumbing to hook up the C library.

# Register callback types in common container.
callbacks['array_applier'] = CFUNCTYPE(c_int, c_size_t, Variant, py_object)
callbacks['dictionary_applier'] = CFUNCTYPE(
    c_int, c_object_p, Variant, py_object)

# Functions strictly alphabetical order.
functionList = [
    ("sourcekitd_cancel_request",
     [c_void_p]),

    ("sourcekitd_initialize",
     None),

    ("sourcekitd_request_array_create",
     [POINTER(c_object_p), c_size_t],
     c_object_p),

    ("sourcekitd_request_array_set_int64",
     [Object, c_size_t, c_int64]),

    ("sourcekitd_request_array_set_string",
     [Object, c_size_t, c_char_p]),

    ("sourcekitd_request_array_set_stringbuf",
     [Object, c_size_t, c_char_p, c_size_t]),

    ("sourcekitd_request_array_set_uid",
     [Object, c_size_t, UIdent]),

    ("sourcekitd_request_array_set_value",
     [Object, c_size_t, Object]),

    ("sourcekitd_request_create_from_yaml",
     [c_char_p, POINTER(c_char_p)],
     c_object_p),

    ("sourcekitd_request_description_copy",
     [Object],
     c_void_p),

    ("sourcekitd_request_description_dump",
     [Object]),

    ("sourcekitd_request_dictionary_create",
     [POINTER(c_object_p), POINTER(c_object_p), c_size_t],
     c_object_p),

    ("sourcekitd_request_dictionary_set_int64",
     [Object, UIdent, c_int64]),

    ("sourcekitd_request_dictionary_set_string",
     [Object, UIdent, c_char_p]),

    ("sourcekitd_request_dictionary_set_stringbuf",
     [Object, UIdent, c_char_p, c_size_t]),

    ("sourcekitd_request_dictionary_set_uid",
     [Object, UIdent, UIdent]),

    ("sourcekitd_request_dictionary_set_value",
     [Object, UIdent, Object]),

    ("sourcekitd_request_int64_create",
     [c_int64],
     c_object_p),

    ("sourcekitd_request_retain",
     [Object],
     c_object_p),

    ("sourcekitd_request_release",
     [Object]),

    ("sourcekitd_request_string_create",
     [c_char_p],
     c_object_p),

    ("sourcekitd_request_uid_create",
     [UIdent],
     c_object_p),

    ("sourcekitd_response_description_copy",
     [Response],
     c_char_p),

    ("sourcekitd_response_description_dump",
     [Response]),

    ("sourcekitd_response_description_dump_filedesc",
     [Response, c_int]),

    ("sourcekitd_response_dispose",
     [Response]),

    ("sourcekitd_response_error_get_description",
     [Response],
     c_char_p),

    ("sourcekitd_response_error_get_kind",
     [Response],
     ErrorKind.from_id),

    ("sourcekitd_response_get_value",
     [Response],
     Variant),

    ("sourcekitd_response_is_error",
     [Response],
     c_bool),

    ("sourcekitd_send_request_sync",
     [Object],
     c_object_p),

    ("sourcekitd_shutdown",
     None),

    ("sourcekitd_uid_get_from_buf",
     [c_char_p, c_size_t],
     c_object_p),

    ("sourcekitd_uid_get_from_cstr",
     [c_char_p],
     c_object_p),

    ("sourcekitd_uid_get_length",
     [UIdent],
     c_size_t),

    ("sourcekitd_uid_get_string_ptr",
     [UIdent],
     c_char_p),

    ("sourcekitd_variant_array_apply_f",
     [Variant, callbacks['array_applier'], py_object],
     c_bool),

    ("sourcekitd_variant_array_get_bool",
     [Variant, c_size_t],
     c_bool),

    ("sourcekitd_variant_array_get_count",
     [Variant],
     c_size_t),

    ("sourcekitd_variant_array_get_int64",
     [Variant, c_size_t],
     c_int64),

    ("sourcekitd_variant_array_get_string",
     [Variant, c_size_t],
     c_char_p),

    ("sourcekitd_variant_array_get_uid",
     [Variant, c_size_t],
     c_object_p),

    ("sourcekitd_variant_array_get_value",
     [Variant, c_size_t],
     Variant),

    ("sourcekitd_variant_bool_get_value",
     [Variant],
     c_bool),

    ("sourcekitd_variant_dictionary_apply_f",
     [Variant, callbacks['dictionary_applier'], py_object],
     c_bool),

    ("sourcekitd_variant_dictionary_get_bool",
     [Variant, UIdent],
     c_bool),

    ("sourcekitd_variant_dictionary_get_int64",
     [Variant, UIdent],
     c_int64),

    ("sourcekitd_variant_dictionary_get_string",
     [Variant, UIdent],
     c_char_p),

    ("sourcekitd_variant_dictionary_get_value",
     [Variant, UIdent],
     Variant),

    ("sourcekitd_variant_dictionary_get_uid",
     [Variant, UIdent],
     c_object_p),

    ("sourcekitd_variant_get_type",
     [Variant],
     VariantType.from_id),

    ("sourcekitd_variant_string_get_length",
     [Variant],
     c_size_t),

    ("sourcekitd_variant_string_get_ptr",
     [Variant],
     c_char_p),

    ("sourcekitd_variant_int64_get_value",
     [Variant],
     c_int64),

    ("sourcekitd_variant_uid_get_value",
     [Variant],
     c_object_p),
]


class LibsourcekitdError(Exception):

    def __init__(self, message):
        self.m = message

    def __str__(self):
        return self.m


def register_function(lib, item, ignore_errors):
    # A function may not exist, if these bindings are used with an older or
    # incompatible version of sourcekitd.
    try:
        func = getattr(lib, item[0])
    except AttributeError as e:
        msg = str(e) + ". Please ensure that your Python bindings are "\
                       "compatible with your sourcekitd version."
        if ignore_errors:
            return
        raise LibsourcekitdError(msg)

    if len(item) >= 2:
        func.argtypes = item[1]

    if len(item) >= 3:
        func.restype = item[2]

    if len(item) == 4:
        func.errcheck = item[3]


def register_functions(lib, ignore_errors):
    """Register function prototypes with a sourcekitd library instance.

    This must be called as part of library instantiation so Python knows how
    to call out to the shared library.
    """
    def register(item):
        return register_function(lib, item, ignore_errors)

    map(register, functionList)


class Config(object):
    library_path = None
    library_file = None
    loaded = False

    @staticmethod
    def set_library_path(path):
        """Set the path in which to search for sourcekitd"""
        if Config.loaded:
            raise Exception("library path must be set before before using "
                            "any other functionalities in sourcekitd.")

        Config.library_path = path

    @staticmethod
    def set_library_file(filename):
        """Set the exact location of sourcekitd"""
        if Config.loaded:
            raise Exception("library file must be set before before using "
                            "any other functionalities in sourcekitd.")

        Config.library_file = filename

    @CachedProperty
    def lib(self):
        lib = self.get_sourcekitd_library()
        register_functions(lib, False)
        Config.loaded = True
        return lib

    @CachedProperty
    def free(self):
        free = cdll.LoadLibrary('libc.dylib').free
        free.argtypes = [c_void_p]
        return free

    def get_filename(self):
        if Config.library_file:
            return Config.library_file

        import platform
        name = platform.system()

        if name == 'Darwin':
            # The XPC service cannot run via the bindings due to permissions
            # issue.
            # file = 'sourcekitd.framework/sourcekitd'
            file = 'libsourcekitdInProc.dylib'
        elif name == 'Windows':
            file = 'sourcekitd.dll'
        else:
            file = 'sourcekitd.so'

        if Config.library_path:
            file = Config.library_path + '/' + file

        return file

    def get_sourcekitd_library(self):
        try:
            library = cdll.LoadLibrary(self.get_filename())
        except OSError as e:
            msg = str(e) + ". To provide a path to sourcekitd use " \
                           "Config.set_library_path() or " \
                           "Config.set_library_file()."
            raise LibsourcekitdError(msg)

        return library

conf = Config()
conf.lib.sourcekitd_initialize()

__all__ = [
    'Config',
    'Object',
    'Response',
    'UIdent',
    'ErrorKind',
    'Variant',
    'VariantType'
]
