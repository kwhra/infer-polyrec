open Syntax
open EnvU
open EnvD
open Subst

(* expvar *)
let	a	=	"a"
let	b	=	"b"
let	c	=	"c"
let	d	=	"d"
let	e	=	"e"
let	f	=	"f"
let	g	=	"g"
let	h	=	"h"
let	i	=	"i"
let	j	=	"j"
let	k	=	"k"
let	l	=	"l"
let	m	=	"m"
let	n	=	"n"
let	o	=	"o"
let	p	=	"p"
let	q	=	"q"
let	r	=	"r"
let	s	=	"s"
let	t	=	"t"
let	u	=	"u"
let	v	=	"v"
let	w	=	"w"
let	x	=	"x"
let	y	=	"y"
let	z	=	"z"

let	expA	=	ExpVar	a
let	expB	=	ExpVar	b
let	expC	=	ExpVar	c
let	expD	=	ExpVar	d
let	expE	=	ExpVar	e
let	expF	=	ExpVar	f
let	expG	=	ExpVar	g
let	expH	=	ExpVar	h
let	expI	=	ExpVar	i
let	expJ	=	ExpVar	j
let	expK	=	ExpVar	k
let	expL	=	ExpVar	l
let	expM	=	ExpVar	m
let	expN	=	ExpVar	n
let	expO	=	ExpVar	o
let	expP	=	ExpVar	p
let	expQ	=	ExpVar	q
let	expR	=	ExpVar	r
let	expS	=	ExpVar	s
let	expT	=	ExpVar	t
let	expU	=	ExpVar	u
let	expV	=	ExpVar	v
let	expW	=	ExpVar	w
let	expX	=	ExpVar	x
let	expY	=	ExpVar	y
let	expZ	=	ExpVar	z

(* example on paper *)
(* rec{f = \gy.ifc false y g(fgy)} *)
let appFG = ExpApp(expF, expG)
let exp0''' = ExpApp(expG, ExpApp(appFG, expY))
let exp0'' = ExpApp(ExpApp(ExpApp(ExpCon Ifc, ExpCon Bool), expY), exp0''')
let exp0' = ExpAbs(g, ExpAbs(y, exp0''))
let exp0 = ExpRec(f, exp0')

(* rec{f = \ghy.ifc false y g(fgh(fhgy))} *)
let appFH = ExpApp(expF, expH)
let exp1''' = ExpApp(expG, ExpApp(ExpApp(appFG, expH), ExpApp(ExpApp(appFH, expG), expY)))
let exp1'' = ExpApp(ExpApp(ExpApp(ExpCon Ifc, ExpCon Bool), expY), exp1''')
let exp1' = ExpAbs(g, ExpAbs(h, ExpAbs(y, exp1'')))
let exp1 = ExpRec(f, exp1')

let exp1''a = ExpIf(expY, exp1''')
let exp1'a = ExpAbs(g, ExpAbs(h, ExpAbs(y, exp1''a)))
let exp1a = ExpRec(f, exp1'a)


(* rec{f = \gh1h2y.ifc false y (g(fgh1h2(fh1gh2(fh1h2g y))))} *)
let h' = "h'"
let expH' = ExpVar h'
let appFH = ExpApp(expF, expH)

let exp2''' = ExpApp(expG, ExpApp(ExpApp(ExpApp(appFG, expH), expH'), ExpApp(ExpApp(ExpApp(appFH, expG), expH'), ExpApp(ExpApp(ExpApp(appFH, expH'), expG), expY))))
let exp2'' = ExpApp(ExpApp(ExpApp(ExpCon Ifc, ExpCon Bool), expY), exp2''')
let exp2' = ExpAbs(g, ExpAbs(h, ExpAbs(h', ExpAbs(y, exp2''))))
let exp2 = ExpRec(f, exp2')

(* rec{f = \gh1h2h3y.ifc false y (g(fgh1h2h3(fh1gh2h3(fh1h2gh3(fh1h2h3g y)))))} *)
let h'' = "h''"
let expH'' = ExpVar h''

let exp3''' = ExpApp(expG, ExpApp(ExpApp(ExpApp(ExpApp(appFG, expH), expH'), expH''), ExpApp(ExpApp(ExpApp(ExpApp(appFH, expG), expH'), expH''), ExpApp(ExpApp(ExpApp(ExpApp(appFH, expH'), expG), expH''), ExpApp(ExpApp(ExpApp(ExpApp(appFH, expH'), expH''), expG), expY)))))
let exp3'' = ExpApp(ExpApp(ExpApp(ExpCon Ifc, ExpCon Bool), expY), exp3''')
let exp3' = ExpAbs(g, ExpAbs(h, ExpAbs(h', ExpAbs(h'', ExpAbs(y, exp3'')))))
let exp3 = ExpRec(f, exp3')



let initD = EnvD.empty