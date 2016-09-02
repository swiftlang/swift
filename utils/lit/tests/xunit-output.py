# Check xunit output
# RUN: %{lit} --xunit-xml-output %t.xunit.xml %{inputs}/test-data
# RUN: FileCheck < %t.xunit.xml %s

# CHECK: <?xml version="1.0" encoding="UTF-8" ?>
# CHECK: <testsuites>
# CHECK: <testsuite name='test-data' tests='1' failures='0'>
# CHECK: <testcase classname='test-data.test-data' name='metrics.ini' time='0.{{[0-9]+}}'/>
# CHECK: </testsuite>
# CHECK: </testsuites>
