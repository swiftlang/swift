// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %{python} %utils/line-directive
// RUN: %{python} %utils/line-directive -- %{python} %t/unicode.py

//--- unicode.py
print('你好')
