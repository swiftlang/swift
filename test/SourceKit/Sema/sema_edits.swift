
{
    
print("hello")

// RUN: %sourcekitd-test -req=open %s -- %s == -req=edit -pos=3:5 -replace="}" -length=0 %s == \
// RUN:    -req=edit -pos=3:1 -replace="}" -length=5 %s == \
// RUN:    -req=print-annotations %s > %t.response
// RUN: %diff -u %s.response %t.response
