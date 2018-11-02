'''
lldbCheckExpect.py

This is a driver script for testing lldb expression evaluation. It installs a
breakpoint command which executes tests inserted by the instrumentation pass,
and launches a target.

To use it, compile a target program with these flags:

    -g -Xfrontend -debugger-testing-transform

Make sure the swift standard library is built with debug info. Then, launch
swift-lldb with "-o path/to/this/script -- <program> [<args>]". There is a
utility available in SWIFT_BINARY_DIR/bin/lldb-check-expect which automates
this, e.g:

    ./bin/lldb-check-expect <program>
    ...
    Evaluating check-expect in closure #2 () -> () in CAPITest.swapTwoValues
      Checked variable: a
      Expected value  : 98
      Actual value    : ...
'''


def unwrap(s):
    '''
    Strip non-essential character sequences from a string.

    >>> unwrap('(String) value = "42\\\\n"')
    '42'
    >>> unwrap('(Int) $R0 = 42')
    '42'
    >>> unwrap('\\\\"foo\\"')
    'foo'
    >>> unwrap('foo\\nbar')
    'foo\\nbar'
    >>> unwrap(' foo ')
    'foo'
    '''
    s = s[s.find('=') + 1:]
    s = s.lstrip(' "')
    s = s.rstrip('"')
    if s.endswith('\\n'):
        s = s[:-2]
    if s.endswith('\\"'):
        s = s[:-2]
    if s.startswith('\\"'):
        s = s[2:]
    s = s.replace('\\n', '\n')
    s = s.strip()
    return s


def on_check_expect(frame, bp_loc, session):
    parent_frame = frame.get_parent_frame()
    parent_name = parent_frame.GetFunctionName()
    print "Evaluating check-expect in", parent_name

    # Note: If we fail to stringify the arguments in the check-expect frame,
    # the standard library has probably not been compiled with debug info.
    wrapped_var_name, wrapped_expected_value = map(str, frame.arguments)

    var_name = unwrap(wrapped_var_name)
    expected_value = unwrap(wrapped_expected_value)

    # Evaluate the variable in the parent frame of the check-expect.
    frame.thread.SetSelectedFrame(1)
    expr_result = parent_frame.FindVariable(var_name).GetObjectDescription()
    eval_result = unwrap(str(expr_result))

    print "  Checked variable:", var_name
    print "  Expected value  :", expected_value
    print "  Actual value    :", eval_result

    if eval_result == expected_value:
        # Do not stop execution.
        return False

    print "Found a possible expression evaluation failure."

    for i, (c1, c2) in enumerate(zip(expected_value, eval_result)):
        if c1 == c2:
            continue
        print " -> Character difference at index", i
        print " -> Expected", c1, "but found", c2
        break
    else:
        print " -> Expected string has length", len(expected_value)
        print " -> Actual string has length", len(eval_result)
    return True


def __lldb_init_module(debugger, internal_dict):
    debugger.HandleCommand('breakpoint set -n _debuggerTestingCheckExpect '
                           '--breakpoint-name check_expect_bkpt')
    debugger.HandleCommand('breakpoint command add --python-function '
                           'lldbCheckExpect.on_check_expect '
                           '--stop-on-error true check_expect_bkpt')
    debugger.HandleCommand('run')
