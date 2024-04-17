.\" Graph
.\" copy "file.tsv" will source data entries
.G1

label left "Frequency (Hz)"
label bottom "Time (s)"

coord x 0,6 y 0,25

frame wid 5
frame top invis
frame right invis

grid left from 0 to 25 by 5

draw solid
0   5.6
1   10.2
2   11.4
3   17.2
4   22.9
5   22.2
6   23.8

bullet size 20 at 1,9.8

"(1 , 10.2)" size 12 above at 1,11

line dashed from 4,0 to 4,22.9

.G2
