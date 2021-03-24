
in stack ghci
```
demo = "demoExamples/search/demo.txt"
runner = getRunner demo
p1 <- getProposition demo "p1"
q <- runner $ initAgu [p1]
r <- runner $ query q
```
![Demo Graph succ](./imgs/search-demo-succ.png)
The default ording function consists of lastlink and eli.

when "r101:p75=>!p66,0"
result is :
```
(Warranted : Path: [[r6: p6 p7->p1],[r7: p8=>p6,r8: p9=>p7],[r9: p10=>p8,r10: p11=>p9],[r11: ->p10,r12: ->p11]]defeater: Unwarranted : Path: [[r69: p54 p55->!p7],[r70: p56=>p54,r74: p58=>p55]]defeater: Self-Warranted : [[[r87: p66->!r74],[r88: p67=>p66],[r89: p68=>p67],[r90: ->p68]]]Path: [[r69: p54 p55->!p7],[r70: p56=>p54,r71: p57=>p55]]defeater: Self-Warranted : [[[r79: p60 p61->!r71],[r80: p62=>p60,r81: p63=>p61],[r82: ->p62,r83: ->p63]]],[!p7,p7,!p7,p7,!r71: p57=>p55,!r74: p58=>p55])
```

when "r101:p75=>!p66,2"
```
(Unwarranted : Path: [[r6: p6 p7->p1],[r7: p8=>p6,r8: p9=>p7]]defeater: Warranted : Path: [[r69: p54 p55->!p7],[r70: p56=>p54,r74: p58=>p55],[r72: ->p56,r76: p59=>p58],[r78: ->p59]]defeater: Unwarranted : Path: [[r87: p66->!r74],[r88: p67=>p66]]defeater: Self-Warranted : [[[r101: p75=>!p66],[r102: ->p75]]],[!p7,p7])
```