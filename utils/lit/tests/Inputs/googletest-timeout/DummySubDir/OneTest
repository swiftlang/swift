#!/usr/bin/env python

import sys
import time

if len(sys.argv) != 2:
    raise ValueError("unexpected number of args")

if sys.argv[1] == "--gtest_list_tests":
    print("""\
FirstTest.
  subTestA
  subTestB
  subTestC
""")
    sys.exit(0)
elif not sys.argv[1].startswith("--gtest_filter="):
    raise ValueError("unexpected argument: %r" % (sys.argv[1]))

test_name = sys.argv[1].split('=',1)[1]
if test_name == 'FirstTest.subTestA':
    print('I am subTest A, I PASS')
    print('[  PASSED  ] 1 test.')
    sys.exit(0)
elif test_name == 'FirstTest.subTestB':
    print('I am subTest B, I am slow')
    time.sleep(6)
    print('[  PASSED  ] 1 test.')
    sys.exit(0)
elif test_name == 'FirstTest.subTestC':
    print('I am subTest C, I will hang')
    while True:
        pass
else:
    raise SystemExit("error: invalid test name: %r" % (test_name,))
