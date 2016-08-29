import os
from xml.sax.saxutils import escape
from json import JSONEncoder

# Test result codes.

class ResultCode(object):
    """Test result codes."""

    # We override __new__ and __getnewargs__ to ensure that pickling still
    # provides unique ResultCode objects in any particular instance.
    _instances = {}
    def __new__(cls, name, isFailure):
        res = cls._instances.get(name)
        if res is None:
            cls._instances[name] = res = super(ResultCode, cls).__new__(cls)
        return res
    def __getnewargs__(self):
        return (self.name, self.isFailure)

    def __init__(self, name, isFailure):
        self.name = name
        self.isFailure = isFailure

    def __repr__(self):
        return '%s%r' % (self.__class__.__name__,
                         (self.name, self.isFailure))

PASS        = ResultCode('PASS', False)
FLAKYPASS   = ResultCode('FLAKYPASS', False)
XFAIL       = ResultCode('XFAIL', False)
FAIL        = ResultCode('FAIL', True)
XPASS       = ResultCode('XPASS', True)
UNRESOLVED  = ResultCode('UNRESOLVED', True)
UNSUPPORTED = ResultCode('UNSUPPORTED', False)
TIMEOUT     = ResultCode('TIMEOUT', True)

# Test metric values.

class MetricValue(object):
    def format(self):
        """
        format() -> str

        Convert this metric to a string suitable for displaying as part of the
        console output.
        """
        raise RuntimeError("abstract method")

    def todata(self):
        """
        todata() -> json-serializable data

        Convert this metric to content suitable for serializing in the JSON test
        output.
        """
        raise RuntimeError("abstract method")

class IntMetricValue(MetricValue):
    def __init__(self, value):
        self.value = value

    def format(self):
        return str(self.value)

    def todata(self):
        return self.value

class RealMetricValue(MetricValue):
    def __init__(self, value):
        self.value = value

    def format(self):
        return '%.4f' % self.value

    def todata(self):
        return self.value

class JSONMetricValue(MetricValue):
    """
        JSONMetricValue is used for types that are representable in the output
        but that are otherwise uninterpreted.
    """
    def __init__(self, value):
        # Ensure the value is a serializable by trying to encode it.
        # WARNING: The value may change before it is encoded again, and may
        #          not be encodable after the change.
        try:
            e = JSONEncoder()
            e.encode(value)
        except TypeError:
            raise
        self.value = value

    def format(self):
        e = JSONEncoder(indent=2, sort_keys=True)
        return e.encode(self.value)

    def todata(self):
        return self.value

def toMetricValue(value):
    if isinstance(value, MetricValue):
        return value
    elif isinstance(value, int) or isinstance(value, long):
        return IntMetricValue(value)
    elif isinstance(value, float):
        return RealMetricValue(value)
    else:
        # Try to create a JSONMetricValue and let the constructor throw
        # if value is not a valid type.
        return JSONMetricValue(value)


# Test results.

class Result(object):
    """Wrapper for the results of executing an individual test."""

    def __init__(self, code, output='', elapsed=None):
        # The result code.
        self.code = code
        # The test output.
        self.output = output
        # The wall timing to execute the test, if timing.
        self.elapsed = elapsed
        # The metrics reported by this test.
        self.metrics = {}

    def addMetric(self, name, value):
        """
        addMetric(name, value)

        Attach a test metric to the test result, with the given name and list of
        values. It is an error to attempt to attach the metrics with the same
        name multiple times.

        Each value must be an instance of a MetricValue subclass.
        """
        if name in self.metrics:
            raise ValueError("result already includes metrics for %r" % (
                    name,))
        if not isinstance(value, MetricValue):
            raise TypeError("unexpected metric value: %r" % (value,))
        self.metrics[name] = value

# Test classes.

class TestSuite:
    """TestSuite - Information on a group of tests.

    A test suite groups together a set of logically related tests.
    """

    def __init__(self, name, source_root, exec_root, config):
        self.name = name
        self.source_root = source_root
        self.exec_root = exec_root
        # The test suite configuration.
        self.config = config

    def getSourcePath(self, components):
        return os.path.join(self.source_root, *components)

    def getExecPath(self, components):
        return os.path.join(self.exec_root, *components)

class Test:
    """Test - Information on a single test instance."""

    def __init__(self, suite, path_in_suite, config, file_path = None):
        self.suite = suite
        self.path_in_suite = path_in_suite
        self.config = config
        self.file_path = file_path
        # A list of conditions under which this test is expected to fail. These
        # can optionally be provided by test format handlers, and will be
        # honored when the test result is supplied.
        self.xfails = []
        # The test result, once complete.
        self.result = None

    def setResult(self, result):
        if self.result is not None:
            raise ArgumentError("test result already set")
        if not isinstance(result, Result):
            raise ArgumentError("unexpected result type")

        self.result = result

        # Apply the XFAIL handling to resolve the result exit code.
        if self.isExpectedToFail():
            if self.result.code == PASS:
                self.result.code = XPASS
            elif self.result.code == FAIL:
                self.result.code = XFAIL
        
    def getFullName(self):
        return self.suite.config.name + ' :: ' + '/'.join(self.path_in_suite)

    def getFilePath(self):
        if self.file_path:
            return self.file_path
        return self.getSourcePath()

    def getSourcePath(self):
        return self.suite.getSourcePath(self.path_in_suite)

    def getExecPath(self):
        return self.suite.getExecPath(self.path_in_suite)

    def isExpectedToFail(self):
        """
        isExpectedToFail() -> bool

        Check whether this test is expected to fail in the current
        configuration. This check relies on the test xfails property which by
        some test formats may not be computed until the test has first been
        executed.
        """

        # Check if any of the xfails match an available feature or the target.
        for item in self.xfails:
            # If this is the wildcard, it always fails.
            if item == '*':
                return True

            # If this is an exact match for one of the features, it fails.
            if item in self.config.available_features:
                return True

            # If this is a part of the target triple, it fails.
            if item in self.suite.config.target_triple:
                return True

        return False


    def getJUnitXML(self):
        test_name = self.path_in_suite[-1]
        test_path = self.path_in_suite[:-1]
        safe_test_path = [x.replace(".","_") for x in test_path]
        safe_name = self.suite.name.replace(".","-")

        if safe_test_path:
            class_name = safe_name + "." + "/".join(safe_test_path) 
        else:
            class_name = safe_name + "." + safe_name

        xml = "<testcase classname='" + class_name + "' name='" + \
            test_name + "'"
        xml += " time='%.2f'" % (self.result.elapsed,)
        if self.result.code.isFailure:
            xml += ">\n\t<failure >\n" + escape(self.result.output)
            xml += "\n\t</failure>\n</testcase>"
        else:
            xml += "/>"
        return xml
