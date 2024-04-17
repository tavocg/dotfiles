.\" Table
.\" Requires .so utils for auto-numbering
.ad c
.B "Table \\n+[tblnum]:"
Title
.TS
box center tab(|) nospaces decimalpoint(.) ;
cb s ci s
_ _ _ _
c n l n
c n l n
c c c c
c ^ c c
c   c c
.
              One                   |           Two
   Numbers   |        3.14159       |   Left-align         |   10000.2
   Numbers   |        3.14          |   Left-align         |     100.2
    This     |          is          | \\f[I]centered\\f[P] |     text
    This     |                      |       centered       |     too
_
     U       |                      |          T{
This entry will be wrapped to fit
T}|V
.TE
