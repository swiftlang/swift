// REQUIRES: rdar92613094

// RUN: %{python} %utils/line-directive
// RUN: %{python} %utils/line-directive -- %{python} -c "print('你好')"
